# =============================
# USDEUR: GJR-GARCH(1,1), Student-t df=10 (fixed)
# Parallel per-period evaluation; END-ALIGNED multi-step origins.
# Runs 3 rolling windows per freq: 50, 100, 200 bins.
# DETAILS: per-step rows (sigma^2 forecast, r^2 actual) for step=1..H.
# SUMMARIES: metrics on H-step aggregated sums by origin; coverage is per-step.
# OUTPUT: results/<runid>/w{50|100|200}/test_<tag>_<H>step.csv
# =============================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(rugarch)
  library(foreach)
  library(doFuture)
  library(doRNG)
  library(readr)
  library(cli)
  library(glue)
})

# ----- Output root -----
run_id  <- format(with_tz(Sys.time(), "Australia/Sydney"), "%Y%m%d_%H%M%S")
out_root <- file.path("results", paste0("USDEUR_GJR_multihorizon_df10_refit60_everybin_", run_id))
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
cli::cli_alert_info("Writing outputs under: {normalizePath(out_root)}")

# ----- Metrics (variance-scale) -----
qlike <- function(y2, s2) {
  y2 <- pmax(y2, .Machine$double.eps); s2 <- pmax(s2, .Machine$double.eps)
  mean(log(s2) + y2 / s2, na.rm = TRUE)
}
quasi_deviance <- function(y2, s2) {
  y2 <- pmax(y2, .Machine$double.eps); s2 <- pmax(s2, .Machine$double.eps)
  r  <- y2 / s2
  mean(2 * (r - log(r) - 1), na.rm = TRUE)
}
msep <- function(y2, s2) {
  y2 <- pmax(y2, .Machine$double.eps); s2 <- pmax(s2, .Machine$double.eps)
  mean((s2 - y2)^2, na.rm = TRUE)
}

# ----- Empty details schema (per-step rows) -----
empty_detail_df <- function() tibble::tibble(
  period_id  = character(),
  model_id   = character(),
  horizon    = character(),
  freq       = character(),
  H_bins     = integer(),
  time_index = integer(),            # origin index within test window
  step       = integer(),            # 1..H
  timestamp  = as.POSIXct(character()),
  forecast   = numeric(),            # per-step sigma^2
  actual     = numeric(),            # per-step r^2
  fit_failed = integer(),
  shape_hat  = numeric()
)

# ----- Load USDEUR aggregates (complete bins only), label by bin_end -----
freqs <- c("1M","5M","15M","30M","60M")
read_usdeur <- function(freq) {
  path <- paste0("USDEUR", freq, ".csv")
  if (!file.exists(path)) stop(glue("Missing: {path}"))
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
    arrange(Timestamp)
}
data_by_freq <- setNames(lapply(freqs, read_usdeur), freqs)

# ----- Two-month periods (from 2025-01-01 UTC) -----
make_two_month_periods <- function(
    data, ts_col = "Timestamp",
    global_start = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    global_end   = as.POSIXct(format(ceiling_date(Sys.time(), "month") - 1, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
) {
  ts <- data[[ts_col]]; stopifnot(inherits(ts,"POSIXct"))
  data_start <- max(min(ts, na.rm = TRUE), global_start)
  data_end   <- min(max(ts, na.rm = TRUE), global_end)
  if (data_end <= data_start) {
    return(tibble::tibble(period_id = character(),
                          start = as.POSIXct(NA), end = as.POSIXct(NA))[0,])
  }
  starts <- floor_date(data_start, "month") %>%
    seq(to = floor_date(data_end, "month"), by = "2 months")
  ends   <- starts %m+% months(2)
  tibble::tibble(
    period_id = paste0(format(starts,"%Y-%m-%d"), "_", format(ends,"%Y-%m-%d")),
    start = starts,
    end   = pmin(ends, data_end)
  ) %>% dplyr::filter(start < data_end)
}

split_train_test <- function(data, periods_tbl, train_len) {
  data <- dplyr::arrange(data, Timestamp)
  out <- vector("list", nrow(periods_tbl)); names(out) <- periods_tbl$period_id
  for (i in seq_len(nrow(periods_tbl))) {
    ps <- periods_tbl$start[i]; pe <- periods_tbl$end[i]
    pre_idx  <- which(data$Timestamp <  ps)
    test_idx <- which(data$Timestamp >= ps & data$Timestamp < pe)
    k <- min(train_len, length(pre_idx))
    train_idx <- if (k > 0) tail(pre_idx, k) else integer(0)
    out[[i]] <- list(
      train = if (length(train_idx)) data[train_idx,] else data[0,],
      test  = if (length(test_idx))  data[test_idx,]  else data[0,],
      meta  = list(start = ps, end = pe,
                   train_n = length(train_idx),
                   test_n  = length(test_idx))
    )
  }
  out
}

# ----- GJR-GARCH spec with Student-t df fixed at 10 -----
fixed_df <- 10
gjr_spec_df10 <- ugarchspec(
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model         = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std",
  fixed.pars         = list(shape = fixed_df)
)

# ----- Cadences -----
freq_minutes  <- c(`1M`=1L, `5M`=5L, `15M`=15L, `30M`=30L, `60M`=60L)
refit_minutes <- 60L   # refit once per hour

# ----- Models (H_bins defines horizon) -----
models <- tribble(
  ~horizon, ~freq,  ~H_bins, ~model_id,
  # 5m horizon
  "H5m",    "1M",   5L,      "1M_5step",
  "H5m",    "5M",   1L,      "5M_1step",
  # 15m horizon
  "H15m",   "5M",   3L,      "5M_3step",
  "H15m",   "15M",  1L,      "15M_1step",
  # 30m horizon
  "H30m",   "5M",   6L,      "5M_6step",
  "H30m",   "15M",  2L,      "15M_2step",
  "H30m",   "30M",  1L,      "30M_1step",
  # 60m horizon
  "H60m",   "15M",  4L,      "15M_4step",
  "H60m",   "30M",  2L,      "30M_2step",
  "H60m",   "60M",  1L,      "60M_1step"
)

# ----- Allowed end-of-block minute marks for (freq, H_bins) -----
allowed_end_minutes <- function(freq, H_bins) {
  base <- switch(freq, `1M`=1L, `5M`=5L, `15M`=15L, `30M`=30L, `60M`=60L)
  stopifnot(!is.null(base))
  block <- base * H_bins
  mins <- (0:59)[(0:59) %% block == 0]
  if (!length(mins)) mins <- 0L
  sprintf("%02d", mins)
}

# ----- Filter origins so END timestamp minute ∈ end_align_minutes -----
filter_origins_by_end <- function(test_timestamps, origins, H_bins, end_align_minutes) {
  if (is.null(end_align_minutes) || !length(end_align_minutes)) return(origins)
  end_idx <- origins + H_bins - 1L
  end_idx <- end_idx[end_idx >= 1L & end_idx <= length(test_timestamps)]
  keep <- format(test_timestamps[end_idx], "%M") %in% end_align_minutes
  origins[keep]
}

# ----- Core: origin loop with hourly refit + cached long path -----
# EMITS PER-STEP rows (step = 1..H)
multi_step_block <- function(train_data, test_data,
                             window_bins, H_bins,
                             refit_every_bins,
                             n_restarts = 5L, spec,
                             end_align_minutes = NULL,
                             model_id = "", horizon = "", freq = "", period_id = "") {
  if (nrow(test_data) < H_bins || nrow(train_data) < window_bins)
    return(empty_detail_df())
  
  all_r <- c(train_data$log_return, test_data$log_return)
  n_tr  <- nrow(train_data); n_te <- nrow(test_data)
  
  origins <- seq.int(1L, n_te - H_bins + 1L, by = 1L)
  if (!is.null(end_align_minutes)) {
    origins <- filter_origins_by_end(test_data$Timestamp, origins, H_bins, end_align_minutes)
    if (!length(origins)) return(empty_detail_df())
  }
  
  out_rows <- vector("list", length(origins))
  last_fit <- NULL
  last_refit_origin <- NA_integer_
  cached_sig2_long  <- NULL
  cached_len_long   <- 0L
  
  for (j in seq_along(origins)) {
    bstart <- origins[j]
    window_end   <- n_tr + bstart - 1L
    window_start <- window_end - window_bins + 1L
    if (window_start < 1L) { out_rows[[j]] <- NULL; next }
    
    need_refit <- is.na(last_refit_origin) || ((bstart - 1L) %% refit_every_bins == 0L)
    
    # H plus within-hour rolling cache span
    max_roll_possible <- max(0L, min(refit_every_bins - 1L,
                                     (n_te - bstart) - (H_bins - 1L)))
    nAheadLong <- H_bins + max_roll_possible
    max_nAheadPossible <- length(all_r) - window_end
    if (max_nAheadPossible <= 0L) { out_rows[[j]] <- NULL; next }
    nAheadLong <- max(1L, min(nAheadLong, max_nAheadPossible))
    
    series_block_end <- min(window_end + nAheadLong, length(all_r))
    series_block <- all_r[window_start:series_block_end]
    
    # ---- robust window sanity check ----
    win_vec   <- suppressWarnings(as.numeric(series_block[1:window_bins]))
    finite_ix <- is.finite(win_vec)
    n_finite  <- sum(finite_ix)
    win_var   <- if (n_finite >= 2L) stats::var(win_vec[finite_ix], na.rm = TRUE) else NA_real_
    ok_window <- is.finite(win_var) && (win_var > .Machine$double.eps)
    
    # Shared stub
    row_stub <- tibble::tibble(
      period_id  = period_id,
      model_id   = model_id,
      horizon    = horizon,
      freq       = freq,
      H_bins     = H_bins,
      time_index = bstart
    )
    
    if (!ok_window) {
      out_rows[[j]] <- bind_cols(
        row_stub,
        tibble::tibble(
          step       = seq_len(H_bins),
          timestamp  = test_data$Timestamp[bstart + 0:(H_bins - 1L)],
          forecast   = NA_real_,
          actual     = test_data$log_return[bstart + 0:(H_bins - 1L)]^2,
          fit_failed = 1L,
          shape_hat  = fixed_df
        )
      )
      next
    }
    
    # (Re)fit if needed
    if (need_refit) {
      fit <- tryCatch(
        ugarchfit(spec, data = series_block, out.sample = nAheadLong,
                  solver = "gosolnp",
                  fit.control = list(stationarity = 1),
                  solver.control = list(trace = 0, n.restarts = n_restarts, rseed = 123)),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        fit <- tryCatch(
          ugarchfit(spec, data = series_block, out.sample = nAheadLong,
                    solver = "hybrid",
                    fit.control = list(stationarity = 1),
                    solver.control = list(trace = 0)),
          error = function(e) NULL
        )
      }
      if (!is.null(fit)) {
        fc_long <- tryCatch(ugarchforecast(fit, n.ahead = nAheadLong, n.roll = 0),
                            error = function(e) NULL)
        if (!is.null(fc_long)) {
          cached_sig2_long  <- as.numeric(sigma(fc_long))^2
          cached_len_long   <- length(cached_sig2_long)
          last_fit <- fit
          last_refit_origin <- bstart
        } else {
          cached_sig2_long <- NULL; cached_len_long <- 0L
          last_fit <- NULL; last_refit_origin <- bstart
        }
      } else {
        cached_sig2_long <- NULL; cached_len_long <- 0L
        last_fit <- NULL; last_refit_origin <- bstart
      }
    }
    
    if (is.null(last_fit)) {
      out_rows[[j]] <- bind_cols(
        row_stub,
        tibble::tibble(
          step       = seq_len(H_bins),
          timestamp  = test_data$Timestamp[bstart + 0:(H_bins - 1L)],
          forecast   = NA_real_,
          actual     = test_data$log_return[bstart + 0:(H_bins - 1L)]^2,
          fit_failed = 1L,
          shape_hat  = fixed_df
        )
      )
      next
    }
    
    # Pull the right forecast chunk (cached long path or fresh short)
    d <- bstart - last_refit_origin
    if (!is.null(cached_sig2_long) && cached_len_long >= (H_bins + d)) {
      sig2_chunk <- cached_sig2_long[(1 + d):(H_bins + d)]
    } else {
      fc_short <- tryCatch(ugarchforecast(last_fit, n.ahead = H_bins + d, n.roll = 0),
                           error = function(e) NULL)
      if (is.null(fc_short)) {
        out_rows[[j]] <- bind_cols(
          row_stub,
          tibble::tibble(
            step       = seq_len(H_bins),
            timestamp  = test_data$Timestamp[bstart + 0:(H_bins - 1L)],
            forecast   = NA_real_,
            actual     = test_data$log_return[bstart + 0:(H_bins - 1L)]^2,
            fit_failed = 1L,
            shape_hat  = fixed_df
          )
        )
        next
      }
      sig2_all <- as.numeric(sigma(fc_short))^2
      if (length(sig2_all) < (H_bins + d)) {
        out_rows[[j]] <- bind_cols(
          row_stub,
          tibble::tibble(
            step       = seq_len(H_bins),
            timestamp  = test_data$Timestamp[bstart + 0:(H_bins - 1L)],
            forecast   = NA_real_,
            actual     = test_data$log_return[bstart + 0:(H_bins - 1L)]^2,
            fit_failed = 1L,
            shape_hat  = fixed_df
          )
        )
        next
      }
      sig2_chunk <- sig2_all[(1 + d):(H_bins + d)]
    }
    
    out_rows[[j]] <- bind_cols(
      row_stub,
      tibble::tibble(
        step       = seq_len(H_bins),
        timestamp  = test_data$Timestamp[bstart + 0:(H_bins - 1L)],
        forecast   = sig2_chunk,
        actual     = test_data$log_return[bstart + 0:(H_bins - 1L)]^2,
        fit_failed = 0L,
        shape_hat  = fixed_df
      )
    )
  }
  
  dplyr::bind_rows(out_rows)
}

# ----- Parallel setup -----
workers <- max(1L, parallel::detectCores() - 6L)
registerDoFuture()
plan(multisession, workers = workers)
set.seed(123)  # base seed for reproducible parallel RNG (with doRNG)

# Pretty labels + filename tags
pretty_freq   <- function(f) dplyr::recode(f, `60M`="1-Hour", `30M`="30-Minute", `15M`="15-Minute", `5M`="5-Minute", `1M`="1-Minute", .default=f)
freq_file_tag <- function(f) dplyr::recode(f, `60M`="1h",     `30M`="30m",      `15M`="15m",      `5M`="5m",       `1M`="1m",       .default=tolower(f))

# Student-t (standardized) 97.5% quantile used by rugarch
tcrit_std  <- rugarch::qdist("std", 0.975, mu = 0, sigma = 1, shape = fixed_df)
tcrit2_std <- tcrit_std^2

# ----- Window sizes to run -----
window_sizes <- c(50L, 100L, 200L)

# ----- Main loops: for each model row and each window size -----
for (i in seq_len(nrow(models))) {
  row <- models[i,]
  f   <- row$freq
  H   <- row$H_bins
  fm  <- freq_minutes[[f]]
  end_marks <- allowed_end_minutes(f, H)
  
  # data & periods once per freq
  df <- data_by_freq[[f]]
  periods <- make_two_month_periods(
    df, ts_col = "Timestamp",
    global_start = as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  )
  splits  <- split_train_test(df, periods, train_len = max(window_sizes))  # we'll trim per window below
  
  # Parallelizable unit = per-period
  for (window_bins in window_sizes) {
    # Subfolder per window
    out_dir <- file.path(out_root, paste0("w", window_bins))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Refit cadence in bins
    refit_bins <- max(1L, as.integer(ceiling(refit_minutes / fm)))
    
    cli::cli_h2(glue("{row$model_id} ({row$horizon}) — window={window_bins}, H={H}, refit={refit_bins}; periods: {length(splits)}"))
    
    # Run periods in parallel
    det <- foreach(
      k = seq_along(splits),
      .combine  = dplyr::bind_rows,
      .packages = c("tibble","dplyr","rugarch"),
      .options.RNG = 123
    ) %dorng% {
      s <- splits[[k]]
      # Trim training to this window size
      if (nrow(s$train) > window_bins) {
        s$train <- s$train[(nrow(s$train) - window_bins + 1):nrow(s$train), , drop = FALSE]
      }
      multi_step_block(
        train_data = s$train, test_data = s$test,
        window_bins = window_bins, H_bins = H,
        refit_every_bins = refit_bins,
        n_restarts = 5L, spec = gjr_spec_df10,
        end_align_minutes = end_marks,
        model_id = row$model_id, horizon = row$horizon, freq = f, period_id = names(splits)[k]
      )
    }
    
    # ---------- summaries ----------
    valid_steps <- det %>% filter(fit_failed == 0, is.finite(forecast), is.finite(actual))
    coverage_t <- if (nrow(valid_steps)) mean(valid_steps$actual <= tcrit2_std * valid_steps$forecast, na.rm = TRUE) else NA_real_
    
    agg_origin <- det %>%
      group_by(period_id, time_index) %>%
      mutate(ok = all(fit_failed == 0L) & n() == H) %>%
      filter(ok) %>%
      summarise(forecast = sum(forecast), actual = sum(actual), .groups = "drop") %>%
      filter(is.finite(forecast), is.finite(actual))
    
    sum_tbl <- tibble::tibble(
      summary.freq           = pretty_freq(f),
      summary.window         = window_bins,
      summary.step_ahead     = H,
      summary.mse            = if (nrow(agg_origin)) msep(agg_origin$actual, agg_origin$forecast) else NA_real_,
      summary.mae            = if (nrow(agg_origin)) mean(abs(agg_origin$forecast - agg_origin$actual), na.rm = TRUE) else NA_real_,
      summary.rmse           = if (nrow(agg_origin)) sqrt(mean((agg_origin$forecast - agg_origin$actual)^2, na.rm = TRUE)) else NA_real_,
      summary.n              = nrow(agg_origin),
      summary.failed_fits    = sum(det$fit_failed == 1L, na.rm = TRUE),
      summary.coverage_tdf10 = coverage_t,
      summary.qlike          = if (nrow(agg_origin)) qlike(agg_origin$actual, agg_origin$forecast) else NA_real_,
      summary.quasi_deviance = if (nrow(agg_origin)) quasi_deviance(agg_origin$actual, agg_origin$forecast) else NA_real_
    )
    
    # ---------- combine summary.* + details.* ----------
    if (!nrow(det)) {
      combined_out <- sum_tbl %>%
        mutate(details.time_index = integer(),
               details.step = integer(),
               details.timestamp = as.POSIXct(character()),
               details.forecast = double(),
               details.actual   = double(),
               details.fit_failed = integer()) %>% slice(0)
    } else {
      details_pref <- det %>%
        transmute(
          details.time_index = time_index,
          details.step       = step,
          details.timestamp  = timestamp,
          details.forecast   = forecast,
          details.actual     = actual,
          details.fit_failed = fit_failed
        ) %>% arrange(details.timestamp)
      combined_out <- bind_cols(
        sum_tbl[rep(1, nrow(details_pref)), , drop = FALSE],
        details_pref
      )
    }
    
    # ---------- write ----------
    tag <- freq_file_tag(f)
    fn  <- file.path(out_dir, sprintf("test_%s_%dstep.csv", tag, H))
    readr::write_csv(combined_out, fn)
    cli::cli_alert_success("Wrote: {fn}")
  }
}

cli::cli_alert_success("All per-model files saved under: {normalizePath(out_root)}")

