## GJRGARCH(1,1) student t fixed df parameter fits over time ##

# =============================
# Rolling parameter fits (diagnostics only)
# GJR-GARCH(1,1), Student-t (df = 10 fixed)
# Refit ~hourly using a ~200-minute window
# Outputs: params_<freq>.csv, params_all_frequencies.csv, params_trajectories.png
# =============================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(rugarch)
  library(readr)
})

# ---------- Config ----------
freqs        <- c("1M","5M","15M","30M","60M")   # expects USDEUR1M.csv, USDEUR5M.csv, ...
fixed_df     <- 10
global_start <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
global_end   <- as.POSIXct("2025-07-01 00:00:00", tz = "UTC")

# EXACTLY match Table 5.15 windows (bins) and refit hourly
freq_minutes     <- c(`1M`=1L, `5M`=5L, `15M`=15L, `30M`=30L, `60M`=60L)
window_bins_tbl  <- c(`1M`=200L, `5M`=40L, `15M`=14L, `30M`=7L, `60M`=4L)
refit_minutes    <- 60L
pretty_freq <- function(f) recode(f, `60M`="1-Hour", `30M`="30-Minute",
                                  `15M`="15-Minute", `5M`="5-Minute", `1M`="1-Minute", .default=f)

# ---------- Data loader ----------
read_usdeur <- function(freq) {
  path <- paste0("USDEUR", freq, ".csv")
  if (!file.exists(path)) stop("Missing file: ", path)
  readr::read_csv(
    path,
    col_types = cols(
      pair       = col_character(),
      bin_start  = col_datetime(),
      bin_end    = col_datetime(),
      bin        = col_datetime(),
      n_1m       = col_integer(),
      expected_n = col_integer(),
      log_return = col_double()
    ),
    show_col_types = FALSE
  ) %>%
    filter(n_1m == expected_n, is.finite(log_return)) %>%
    transmute(Timestamp = with_tz(bin_end, "UTC"), log_return = log_return) %>%
    arrange(Timestamp) %>%
    filter(Timestamp >= global_start, Timestamp < global_end)
}

# ---------- Model spec ----------
gjr_spec_df10 <- ugarchspec(
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model         = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std",
  fixed.pars         = list(shape = fixed_df)     # df fixed at 10
)

# ---------- Fit loop ----------
param_files <- list()

for (f in freqs) {
  message("== Frequency: ", f, " ==")
  df <- read_usdeur(f)
  if (nrow(df) == 0) { warning("No data after filters for ", f); next }
  
  fm          <- freq_minutes[[f]]
  window_bins <- window_bins_tbl[[f]]                                # exact Table 5.15
  refit_bins  <- max(1L, as.integer(ceiling(refit_minutes / fm)))    # hourly refit
  
  # Refit positions across the full series
  n <- nrow(df)
  refit_positions <- seq.int(from = window_bins, to = n, by = refit_bins)
  if (!length(refit_positions)) { warning("No refit positions for ", f); next }
  
  rows <- vector("list", length(refit_positions))
  set.seed(123)
  
  for (i in seq_along(refit_positions)) {
    ref_idx <- refit_positions[i]
    start   <- ref_idx - window_bins + 1L
    if (start < 1L) { rows[[i]] <- NULL; next }
    
    r_win <- df$log_return[start:ref_idx]
    
    # guardrail for degenerate windows
    sd_win <- suppressWarnings(sd(r_win, na.rm = TRUE))
    if (!is.finite(sd_win) || sd_win <= sqrt(.Machine$double.eps)) {
      rows[[i]] <- tibble(
        freq        = f,
        pretty_freq = pretty_freq(f),
        refit_time  = df$Timestamp[ref_idx],
        window_bins = window_bins,
        refit_bins  = refit_bins,
        omega  = NA_real_, alpha1 = NA_real_, beta1 = NA_real_, gamma1 = NA_real_, shape = fixed_df,
        persistence = NA_real_, fit_failed = 1L
      )
      next
    }
    
    # fit (gosolnp, then hybrid fallback)
    fit <- tryCatch(
      ugarchfit(gjr_spec_df10, data = r_win, solver = "gosolnp",
                fit.control = list(stationarity = 1),
                solver.control = list(trace = 0, n.restarts = 5, rseed = 123)),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      fit <- tryCatch(
        ugarchfit(gjr_spec_df10, data = r_win, solver = "hybrid",
                  fit.control = list(stationarity = 1),
                  solver.control = list(trace = 0)),
        error = function(e) NULL
      )
    }
    
    if (is.null(fit)) {
      rows[[i]] <- tibble(
        freq        = f,
        pretty_freq = pretty_freq(f),
        refit_time  = df$Timestamp[ref_idx],
        window_bins = window_bins,
        refit_bins  = refit_bins,
        omega  = NA_real_, alpha1 = NA_real_, beta1 = NA_real_, gamma1 = NA_real_, shape = fixed_df,
        persistence = NA_real_, fit_failed = 1L
      )
      next
    }
    
    cf <- tryCatch(coef(fit), error = function(e) NULL)
    if (is.null(cf)) {
      rows[[i]] <- tibble(
        freq        = f,
        pretty_freq = pretty_freq(f),
        refit_time  = df$Timestamp[ref_idx],
        window_bins = window_bins,
        refit_bins  = refit_bins,
        omega  = NA_real_, alpha1 = NA_real_, beta1 = NA_real_, gamma1 = NA_real_, shape = fixed_df,
        persistence = NA_real_, fit_failed = 1L
      )
      next
    }
    
    getv <- function(nm) if (nm %in% names(cf)) unname(cf[nm]) else NA_real_
    omega  <- getv("omega")
    alpha1 <- getv("alpha1")
    beta1  <- getv("beta1")
    gamma1 <- getv("gamma1")
    shape  <- if ("shape" %in% names(cf)) unname(cf["shape"]) else fixed_df
    
    rows[[i]] <- tibble(
      freq        = f,
      pretty_freq = pretty_freq(f),
      refit_time  = df$Timestamp[ref_idx],
      window_bins = window_bins,
      refit_bins  = refit_bins,
      omega, alpha1, beta1, gamma1, shape,
      persistence = alpha1 + beta1 + pmax(gamma1, 0)/2,
      fit_failed = 0L
    )
  }
  
  params_f <- bind_rows(rows) %>% arrange(refit_time)
  out_csv  <- paste0("params_", tolower(pretty_freq(f)), ".csv") %>%
    gsub("-", "", .) %>% gsub(" ", "", .) %>% gsub("Minute", "m", .) %>% gsub("Hour", "h", .)
  write_csv(params_f, out_csv)
  message("Wrote: ", normalizePath(out_csv))
  param_files[[f]] <- out_csv
}

# ---------- Combine & plot (one page per frequency) ----------
all_params <- list()
for (f in freqs) {
  if (!is.null(param_files[[f]]) && file.exists(param_files[[f]])) {
    all_params[[f]] <- read_csv(param_files[[f]], show_col_types = FALSE)
  }
}
params_df <- bind_rows(all_params)
if (nrow(params_df) == 0) {
  warning("No parameter rows collected; skipping plot.")
  quit(save = "no")
}

# Save combined CSV for convenience
write_csv(params_df, "params_all_frequencies.csv")

# Factor for plotting labels
params_df <- params_df %>%
  mutate(freq_plot = factor(pretty_freq,
                            levels = c("1-Minute","5-Minute","15-Minute","30-Minute","1-Hour"),
                            labels = c("1m","5m","15m","30m","1h")))

# Long format for plotting; mask failed-fit values
p_long <- params_df %>%
  select(freq_plot, refit_time, omega, alpha1, beta1, gamma1, persistence, fit_failed) %>%
  pivot_longer(
    cols = c(omega, alpha1, beta1, gamma1, persistence),
    names_to = "param", values_to = "value"
  ) %>%
  mutate(value = ifelse(fit_failed == 1L, NA_real_, value))

# Multi-page PDF: one page per frequency (monthly ticks) + per-frequency PNGs
pdf("params_trajectories_by_freq.pdf", width = 8.5, height = 11)
for (ff in levels(p_long$freq_plot)) {
  p_ff <- ggplot(dplyr::filter(p_long, freq_plot == ff),
                 aes(refit_time, value)) +
    geom_line(linewidth = 0.4, na.rm = TRUE) +
    facet_wrap(~ param, ncol = 1, scales = "free_y") +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b '%y") +
    labs(title = paste("GJR–GARCH Parameters —", ff), x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_ff)
  ggsave(paste0("params_trajectories_", ff, ".png"), p_ff,
         width = 8, height = 10, dpi = 300)
}
dev.off()

message("Saved: params_all_frequencies.csv, params_trajectories_by_freq.pdf, and per-frequency PNGs.")




