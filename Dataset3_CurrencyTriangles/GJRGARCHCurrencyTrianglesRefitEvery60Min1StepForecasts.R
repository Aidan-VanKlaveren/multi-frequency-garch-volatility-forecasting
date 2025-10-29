## Currency Triangles — Step 1: 1-step GJR-GARCH across multi-frequencies
## ======================================================================
## Pairs: GBPEUR, USDEUR, USDGBP
## Freqs: 1M, 5M, 15M, 30M, 60M (reads {PAIR}_{FREQ}.csv or {PAIR}{FREQ}.csv)
## Window: rolling window = 200 observations (fixed across frequencies)
## Global period: 2025-01-01 00:00:00 UTC → 2025-07-01 00:00:00 UTC (three 2-month periods)
## Refit cadence: hourly (scaled per frequency)
## Output: one CSV per pair×freq with summary.* + details.*
## ======================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(rugarch)
  library(cli)
  library(glue)
  library(readr)
  library(stringr)
})

# ---------------- Config ----------------
pairs  <- c("GBPEUR", "USDEUR", "USDGBP")
freqs  <- c("1M","5M","15M","30M","60M")

# FIXED window: exactly 200 observations for every frequency
window_bins_fixed <- 200L
refit_minutes     <- 60L      # refit cadence (hourly)
fixed_df          <- 10
tz_use            <- "UTC"

# EXACTLY three two-month periods:
global_start <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
global_end   <- as.POSIXct("2025-07-01 00:00:00", tz = "UTC")  # Jan–Mar, Mar–May, May–Jul
# ----------------------------------------

# ----- Output folder -----
run_id  <- format(with_tz(Sys.time(), "Australia/Sydney"), "%Y%m%d_%H%M%S")
out_dir <- file.path("results", paste0("step1_gjr_multi_df", fixed_df, "_refit60_", run_id))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cli::cli_alert_info("Writing outputs to: {normalizePath(out_dir)}")
cli::cli_alert_info("Global window (UTC): {format(global_start)}  →  {format(global_end)}")

# ----- Metrics (variance scale) -----
qlike <- function(y2, s2) {
  y2 <- pmax(y2, .Machine$double.eps); s2 <- pmax(s2, .Machine$double.eps)
  mean(log(s2) + y2 / s2, na.rm = TRUE)
}
quasi_deviance <- function(y2, s2) {
  y2 <- pmax(y2, .Machine$double.eps); s2 <- pmax(s2, .Machine$double.eps)
  r <- y2 / s2
  mean(2 * (r - log(r) - 1), na.rm = TRUE)
}
msep <- function(y2, s2) {
  y2 <- pmax(y2, .Machine$double.eps); s2 <- pmax(s2, .Machine$double.eps)
  mean((s2 - y2)^2, na.rm = TRUE)
}

# ----- Reader for {PAIR}_{FREQ}.csv OR {PAIR}{FREQ}.csv (expects fixed headers) -----
# Required columns: Timestamp, Last  (Open/High/Low/Change/%Chg/Volume may be present but are ignored here)
read_pair_freq <- function(pair, freq_label) {
  candidates <- paste0(pair, "_", freq_label, ".csv")
  path <- candidates[file.exists(candidates)][1]
  if (is.na(path)) stop(glue("Missing: {paste(candidates, collapse=' or ')}"))
  cli::cli_alert_info("Reading {path}")
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # normalize names (BOM/whitespace)
  nms <- gsub("\ufeff", "", names(df), fixed = TRUE)
  nms <- trimws(nms)
  names(df) <- nms
  
  # small fallback if exported as "Time"
  if (!("Timestamp" %in% names(df)) && ("Time" %in% names(df))) {
    df <- dplyr::rename(df, Timestamp = Time)
  }
  
  if (!all(c("Timestamp","Last") %in% names(df))) {
    stop(glue("{pair}-{freq_label}: file must have columns: Timestamp, Last. Got: {paste(names(df), collapse=', ')}"))
  }
  
  df %>%
    transmute(
      Timestamp = suppressWarnings(
        lubridate::parse_date_time(
          Timestamp,
          orders = c("ymd HMS","ymd HM","mdy HMS","mdy HM","dmy HMS","dmy HM"),
          tz = tz_use, exact = FALSE
        )
      ),
      Last = suppressWarnings(as.numeric(Last))
    ) %>%
    filter(!is.na(Timestamp), is.finite(Last)) %>%
    arrange(Timestamp) %>%
    dplyr::distinct(Timestamp, .keep_all = TRUE) %>%
    mutate(log_return = log(Last / dplyr::lag(Last))) %>%
    filter(is.finite(log_return)) %>%
    select(Timestamp, log_return)
}

# ----- Periods: exactly Jan–Mar, Mar–May, May–Jul 2025 -----
make_three_two_month_periods <- function(global_start, global_end) {
  starts <- floor_date(global_start, "month") %>%
    seq(to = global_start %m+% months(4), by = "2 months")
  ends <- starts %m+% months(2)
  tibble(
    period_id = paste0(format(starts, "%Y-%m-%d"), "_", format(ends, "%Y-%m-%d")),
    start = starts,
    end   = pmin(ends, global_end)
  )
}

# ----- GJR-GARCH(1,1) Student-t(df=fixed) spec -----
gjr_spec <- ugarchspec(
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model         = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std",
  fixed.pars         = list(shape = fixed_df)
)

# ----- Minutes per label -----
freq_minutes <- c(`1M`=1L, `5M`=5L, `15M`=15L, `30M`=30L, `60M`=60L)

# Student-t (standardized) 97.5% quantile (for per-step coverage)
tcrit_std  <- rugarch::qdist("std", 0.975, mu = 0, sigma = 1, shape = fixed_df)
tcrit2_std <- tcrit_std^2

# ----- Per-period 1-step forecaster (ONE forecast per bar) -----
run_period_one_pair <- function(df_period, period_id, pair_name, freq_label) {
  r  <- df_period$log_return
  ts <- df_period$Timestamp
  n  <- nrow(df_period)
  
  fm          <- freq_minutes[[freq_label]]
  window_bins <- max(4L, window_bins_fixed)                               # FIXED to 200 obs
  refit_bins  <- max(1L, as.integer(ceiling(refit_minutes / fm)))
  cli::cli_alert_info("{pair_name} {freq_label}: window_bins={window_bins}; refit_bins={refit_bins}")
  
  if (n <= (window_bins + 1L)) return(tibble())
  
  last_refit_i <- NA_integer_
  fit <- NULL
  failed_fits <- 0L
  
  det_rows <- vector("list", n)   # fill from (window_bins+1):n
  
  for (i in (window_bins + 1L):n) {
    # decide whether to refit
    need_refit <- is.na(last_refit_i) || ((i - (window_bins + 1L)) %% refit_bins == 0L)
    if (need_refit) {
      idx <- (i - window_bins):(i - 1L)
      tr  <- r[idx]
      sd_win <- suppressWarnings(sd(tr))
      if (!is.finite(sd_win) || sd_win <= sqrt(.Machine$double.eps)) {
        fit <- NULL
        last_refit_i <- i
      } else {
        fit <- tryCatch(
          ugarchfit(gjr_spec, data = tr,
                    solver = "gosolnp",
                    fit.control = list(stationarity = 1),
                    solver.control = list(trace = 0, n.restarts = 5, rseed = 123)),
          error = function(e) NULL
        )
        if (is.null(fit)) {
          fit <- tryCatch(
            ugarchfit(gjr_spec, data = tr,
                      solver = "hybrid",
                      fit.control = list(stationarity = 1),
                      solver.control = list(trace = 0)),
            error = function(e) NULL
          )
        }
        last_refit_i <- i
      }
    }
    
    if (is.null(fit)) { failed_fits <- failed_fits + 1L; next }
    f1 <- tryCatch(ugarchforecast(fit, 1), error = function(e) NULL)
    if (is.null(f1)) { failed_fits <- failed_fits + 1L; next }
    
    # ---- SCALAR extraction (fix for 'length = 10' issue)
    mu1_vec    <- as.numeric(fitted(f1))
    sigma1_vec <- as.numeric(sigma(f1))
    mu1        <- if (length(mu1_vec)) mu1_vec[1] else NA_real_
    sigma1     <- if (length(sigma1_vec)) sigma1_vec[1] else NA_real_
    
    h1     <- if (is.finite(sigma1)) sigma1^2 else NA_real_
    resid1 <- if (is.finite(mu1)) r[i] - mu1 else NA_real_
    z1     <- if (is.finite(sigma1) && sigma1 > 0) resid1 / sigma1 else NA_real_
    
    det_rows[[i]] <- tibble(
      details.timestamp    = ts[i],
      details.forecast     = h1,
      details.actual       = r[i]^2,
      details.mu           = mu1,
      details.sigma        = sigma1,
      details.residual     = resid1,
      details.std_residual = z1
    )
  }
  
  details <- bind_rows(det_rows[(window_bins + 1L):n]) %>%
    arrange(details.timestamp) %>%
    dplyr::distinct(details.timestamp, .keep_all = TRUE)
  
  # ----- summaries (H=1) -----
  valid <- details %>% filter(is.finite(details.forecast), is.finite(details.actual))
  coverage_t <- if (nrow(valid)) mean(valid$details.actual <= tcrit2_std * valid$details.forecast, na.rm = TRUE) else NA_real_
  
  sum_tbl <- tibble(
    summary.pair            = pair_name,
    summary.freq            = freq_label,
    summary.window          = window_bins_fixed,   # report fixed 200 obs
    summary.step_ahead      = 1L,
    summary.n               = nrow(valid),
    summary.failed_fits     = failed_fits,
    summary.coverage_tdf10  = coverage_t,
    summary.mse             = if (nrow(valid)) msep(valid$details.actual, valid$details.forecast) else NA_real_,
    summary.mae             = if (nrow(valid)) mean(abs(valid$details.forecast - valid$details.actual), na.rm = TRUE) else NA_real_,
    summary.rmse            = if (nrow(valid)) sqrt(mean((valid$details.forecast - valid$details.actual)^2, na.rm = TRUE)) else NA_real_,
    summary.qlike           = if (nrow(valid)) qlike(valid$details.actual, valid$details.forecast) else NA_real_,
    summary.quasi_deviance  = if (nrow(valid)) quasi_deviance(valid$details.actual, valid$details.forecast) else NA_real_
  )
  
  if (!nrow(details)) {
    combined <- sum_tbl %>%
      mutate(details.timestamp = as.POSIXct(character()),
             details.forecast  = double(),
             details.actual    = double(),
             details.mu        = double(),
             details.sigma     = double(),
             details.residual  = double(),
             details.std_residual = double()) %>%
      slice(0)
  } else {
    combined <- bind_cols(
      sum_tbl[rep(1, nrow(details)), , drop = FALSE],
      tibble(period_id = period_id),
      details
    )
  }
  
  combined
}

# ----- Main runner for a single pair×freq -----
run_one_pair_one_freq <- function(pair_name, freq_label) {
  cli::cli_h1(glue("Running {pair_name} ({freq_label})"))
  
  df <- read_pair_freq(pair_name, freq_label) %>%
    dplyr::filter(Timestamp >= global_start, Timestamp < global_end)
  
  if (!nrow(df)) {
    cli::cli_alert_warning(glue("{pair_name}-{freq_label}: no rows in global window"))
    return(invisible(NULL))
  }
  
  periods_tbl <- make_three_two_month_periods(global_start, global_end)
  cli::cli_alert_info(glue("{pair_name}-{freq_label}: {nrow(periods_tbl)} periods (expected 3)"))
  
  res_list <- vector("list", nrow(periods_tbl))
  for (k in seq_len(nrow(periods_tbl))) {
    ps <- periods_tbl$start[k]; pe <- periods_tbl$end[k]; pid <- periods_tbl$period_id[k]
    df_p <- df %>% dplyr::filter(Timestamp >= ps, Timestamp < pe)
    cli::cli_h2(glue("{pair_name} — {freq_label} — {pid} — rows: {nrow(df_p)}"))
    if (!nrow(df_p)) { res_list[[k]] <- tibble(); next }
    res_list[[k]] <- run_period_one_pair(df_p, pid, pair_name, freq_label)
  }
  
  out <- dplyr::bind_rows(res_list)
  fn <- file.path(out_dir, glue("step1_{tolower(pair_name)}_{tolower(freq_label)}_gjr_tdf{fixed_df}_1step.csv"))
  readr::write_csv(out, fn)
  cli::cli_alert_success(glue("{pair_name}-{freq_label}: wrote {fn} with {nrow(out)} rows"))
  invisible(out)
}

# ----- Run all pair×freq combos -----
for (p in pairs) {
  for (f in freqs) {
    try(run_one_pair_one_freq(p, f), silent = FALSE)
  }
}

cli::cli_alert_success("All per-pair×freq files saved in: {normalizePath(out_dir)}")




