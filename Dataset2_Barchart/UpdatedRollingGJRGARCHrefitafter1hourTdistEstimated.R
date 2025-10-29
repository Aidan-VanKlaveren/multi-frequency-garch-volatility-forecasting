## Rolling GJR-GARCH (Student-t, df estimated) — refit every hour; 60 one-step forecasts per refit

# ---- Libraries ----
library(tidyverse)
library(lubridate)
library(zoo)
library(parallel)
library(rugarch)
library(doFuture)
library(foreach)
library(doRNG)
library(cli)
library(ggplot2)
library(readr)

# =========================================
#           DATA LOADING & PREP
# =========================================

## 1) List csvs
csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)

## 2) Extract the 6-letter pair at the start of the filename
get_pair <- function(fname) {
  nm <- toupper(basename(fname))
  sub("^([A-Z]{6}).*$", "\\1", nm)  # e.g. "USDEUR_2018..." -> "USDEUR"
}

pairs <- purrr::map_chr(csv_files, get_pair)
unique_pairs <- unique(pairs)

## 3) Build one table per pair (dedupe by first column)
for (p in unique_pairs) {
  pf <- csv_files[pairs == p]
  
  df <- pf %>%
    purrr::map_dfr(readr::read_csv, show_col_types = FALSE) %>%
    arrange(across(1)) %>%
    distinct(across(1), .keep_all = TRUE)
  
  assign(paste0(p, "1M"), df, envir = .GlobalEnv)
}
rm(df)

## 3b) Standardise: parse Time -> Timestamp, map Last/Close -> close (keep only these)
parse_time_safe <- function(x) as.POSIXct(x, format = "%m/%d/%Y %H:%M", tz = "UTC")

standardise_fx <- function(df) {
  nm <- names(df)
  
  # Build Timestamp
  if ("Timestamp" %in% nm) {
    ts <- as.POSIXct(df$Timestamp, tz = "UTC")
  } else if ("Time" %in% nm) {
    ts <- parse_time_safe(df$Time)
  } else if ("time" %in% nm) {
    ts <- as.POSIXct(df$time, origin = "1970-01-01", tz = "UTC")
  } else {
    stop("No time/Timestamp column found.")
  }
  
  # Close price (prefer Close, else Last)
  close_col <- if ("Close" %in% nm) "Close" else if ("close" %in% nm) "close" else
    if ("Last" %in% nm) "Last" else if ("last" %in% nm) "last" else NA
  if (is.na(close_col)) stop("No Close/Last column found.")
  close_vals <- as.numeric(df[[close_col]])
  
  tibble(
    Timestamp = ts,
    close = close_vals
  ) %>%
    arrange(Timestamp) %>%
    distinct(Timestamp, .keep_all = TRUE)
}

created_1m <- ls(pattern = "^[A-Z]{6}1M$")
for (nm in created_1m) {
  assign(nm, standardise_fx(get(nm)), envir = .GlobalEnv)
}

## 3c) Compute close-to-close log returns and keep only Timestamp + log_return
to_logreturns <- function(df) {
  df %>%
    arrange(Timestamp) %>%
    transmute(
      Timestamp,
      log_return = log(close) - dplyr::lag(log(close))
    ) %>%
    filter(is.finite(log_return))
}

for (nm in created_1m) {
  assign(nm, to_logreturns(get(nm)), envir = .GlobalEnv)
}

# =========================================
#           TRAIN / TEST SPLITS
# =========================================

make_two_month_periods <- function(
    data, ts_col = "Timestamp",
    global_start = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    global_end   = as.POSIXct(
      format(lubridate::ceiling_date(Sys.time(), "month") - 1, "%Y-%m-%d %H:%M:%S"),
      tz = "UTC"
    )
) {
  ts <- data[[ts_col]]
  if (!inherits(ts, "POSIXct")) stop("Timestamp must be POSIXct.")
  
  data_start <- max(min(ts, na.rm = TRUE), global_start)
  data_end   <- min(max(ts, na.rm = TRUE), global_end)
  if (data_end <= data_start)
    return(tibble::tibble(period_id = character(),
                          start = as.POSIXct(NA), end = as.POSIXct(NA))[0, ])
  
  starts <- lubridate::floor_date(data_start, "month") %>%
    seq(to = lubridate::floor_date(data_end, "month"), by = "2 months")
  ends <- starts %m+% months(2)
  
  tibble::tibble(
    period_id = paste0(format(starts, "%Y-%m-%d"), "_", format(ends, "%Y-%m-%d")),
    start = starts,
    end   = pmin(ends, data_end)
  ) |>
    dplyr::filter(start < data_end)
}

split_train_test_fixed90 <- function(data, ts_col = "Timestamp",
                                     periods_tbl, train_len = 200L,
                                     require_full_train = FALSE) {
  data <- data[order(data[[ts_col]]), ]
  out <- vector("list", nrow(periods_tbl))
  names(out) <- periods_tbl$period_id
  
  for (i in seq_len(nrow(periods_tbl))) {
    ps <- periods_tbl$start[i]; pe <- periods_tbl$end[i]
    
    pre_idx  <- which(data[[ts_col]] <  ps)
    test_idx <- which(data[[ts_col]] >= ps & data[[ts_col]] < pe)
    
    # choose last train_len observations strictly before ps
    if (length(pre_idx) == 0L) {
      train_idx <- integer(0)
    } else {
      k <- min(train_len, length(pre_idx))
      train_idx <- tail(pre_idx, k)
    }
    
    # optionally skip periods without a full 200-length training seed
    if (require_full_train && length(train_idx) < train_len) {
      out[[i]] <- NULL
      next
    }
    
    train_data <- if (length(train_idx)) data[train_idx, ] else data[0, ]
    test_data  <- if (length(test_idx))  data[test_idx, ]  else data[0, ]
    
    out[[i]] <- list(
      train = train_data,
      test  = test_data,
      meta  = list(start = ps, end = pe, train_n = nrow(train_data), test_n = nrow(test_data))
    )
  }
  
  # drop NULLs (if require_full_train = TRUE)
  out[!vapply(out, is.null, logical(1))]
}

# Build splits per pair
for (nm in created_1m) {
  df <- get(nm)  # expects: Timestamp (POSIXct), log_return (numeric)
  
  periods <- make_two_month_periods(
    df, ts_col = "Timestamp",
    global_start = as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  )
  
  splits  <- split_train_test_fixed90(
    df, ts_col = "Timestamp",
    periods_tbl = periods, train_len = 200L,
    require_full_train = FALSE
  )
  
  assign(paste0(nm, "_splits"), splits, envir = .GlobalEnv)
  
  # quick counts
  n_ok <- length(splits)
  rng  <- if (nrow(periods)) sprintf("%s to %s",
                                     format(min(periods$start), "%Y-%m-%d"),
                                     format(max(periods$end),   "%Y-%m-%d")) else "NA"
  cat(sprintf("%s: %d two-month test periods (range %s)\n", nm, n_ok, rng))
}

# =========================================
#          SELECT WHICH PAIRS TO RUN
# =========================================

g10 <- c("AUD","CAD","CHF","EUR","GBP","JPY","NOK","NZD","SEK","USD")
all_splits <- ls(pattern = "^[A-Z]{6}1M_splits$")

target_prefix <- c("USD")  # change/extend as you like, e.g. c("USD","EUR","GBP")

filtered <- all_splits[
  vapply(all_splits, function(nm) {
    pair <- substr(nm, 1, 6)
    base <- substr(pair, 1, 3)
    quote <- substr(pair, 4, 6)
    (base %in% target_prefix | quote %in% target_prefix) &&
      (base %in% g10 && quote %in% g10)
  }, logical(1))
]

# If you want to pin to a single pair during testing, set it here if present:
selected_splits <- if ("USDEUR1M_splits" %in% filtered) "USDEUR1M_splits" else filtered

# =========================================
#      FORECASTER: BLOCKWISE (stride=60)
# =========================================

registerDoFuture()
workers <- max(1L, parallel::detectCores() - 6L)  # keep machine responsive
plan(multisession, workers = workers)

refit_blockwise_stride_dfhat <- function(train_data, test_data,
                                         window_size = 200L,
                                         block_len   = 60L,
                                         stride      = 60L,
                                         n_starts    = 5L,
                                         freq_label  = "") {
  
  cli::cli_inform("===== BLOCKWISE (stride-{stride}) Forecast: {freq_label} (Window = {window_size}, Block = {block_len}, Multi-start = {n_starts}, df = estimated) =====")
  
  stopifnot(all(c("Timestamp","log_return") %in% names(train_data)))
  stopifnot(all(c("Timestamp","log_return") %in% names(test_data)))
  
  if (nrow(test_data) < block_len || nrow(train_data) < window_size) {
    return(list(
      summary = data.frame(freq=freq_label, window=window_size, step_ahead=1,
                           mse=NA, mae=NA, rmse=NA, qlike=NA, n=0,
                           failed_fits=0, coverage=NA, shape_hat=NA),
      details = data.frame()
    ))
  }
  
  all_data <- c(train_data$log_return, test_data$log_return)
  n_train  <- nrow(train_data)
  n_test   <- nrow(test_data)
  
  cand <- seq.int(1L, n_test - block_len + 1L, by = stride)
  cli::cli_inform("Blocks kept (stride={stride}): {length(cand)}")
  
  # Spec WITHOUT fixed.pars -> df is estimated (shape parameter)
  gjr_spec_dfhat <- ugarchspec(
    variance.model     = list(model = "gjrGARCH", garchOrder = c(1,1)),
    mean.model         = list(armaOrder = c(0,0), include.mean = FALSE),
    distribution.model = "std"    # Student-t (standardized), df estimated
  )
  
  `%dorng%` <- doRNG::`%dorng%`
  
  block_rows <- foreach(bstart = cand, .combine = rbind, .packages = "rugarch") %dorng% {
    tryCatch({
      window_end   <- n_train + bstart - 1L
      window_start <- window_end - window_size + 1L
      
      if (window_start < 1L || (window_end + block_len) > length(all_data)) {
        return(data.frame(time_index = integer(0), step = integer(0),
                          timestamp = as.POSIXct(character(0)),
                          forecast = numeric(0), actual = numeric(0),
                          fit_failed = integer(0), shape_hat = numeric(0)))
      }
      
      series_block <- all_data[window_start:(window_end + block_len)]
      sd_val <- suppressWarnings(sd(series_block[1:window_size], na.rm = TRUE))
      if (!is.numeric(series_block) ||
          !is.finite(sd_val) || is.na(sd_val) ||
          sd_val <= sqrt(.Machine$double.eps)) {
        return(data.frame(time_index = rep(bstart, block_len),
                          step = 1:block_len,
                          timestamp = test_data$Timestamp[bstart + 0:(block_len - 1L)],
                          forecast = NA_real_,
                          actual   = test_data$log_return[bstart + 0:(block_len - 1L)]^2,
                          fit_failed = 1L,
                          shape_hat = NA_real_))
      }
      
      # Multi-start via n.restarts; df estimated internally
      best_fit <- tryCatch(
        ugarchfit(
          gjr_spec_dfhat, data = series_block, out.sample = block_len,
          solver = "gosolnp",
          fit.control    = list(stationarity = 1),
          solver.control = list(trace = 0, n.restarts = n_starts, rseed = 123)
        ),
        error = function(e) NULL
      )
      
      # Optional fallback once with hybrid
      if (is.null(best_fit)) {
        best_fit <- tryCatch(
          ugarchfit(
            gjr_spec_dfhat, data = series_block, out.sample = block_len,
            solver = "hybrid",
            fit.control    = list(stationarity = 1),
            solver.control = list(trace = 0)
          ),
          error = function(e) NULL
        )
      }
      
      if (is.null(best_fit)) {
        return(data.frame(time_index = rep(bstart, block_len),
                          step = 1:block_len,
                          timestamp = test_data$Timestamp[bstart + 0:(block_len - 1L)],
                          forecast = NA_real_,
                          actual   = test_data$log_return[bstart + 0:(block_len - 1L)]^2,
                          fit_failed = 1L,
                          shape_hat = NA_real_))
      }
      
      # Extract estimated df ("shape")
      shape_hat <- tryCatch(unname(coef(best_fit)["shape"]), error = function(e) NA_real_)
      if (!is.finite(shape_hat) || shape_hat <= 2) shape_hat <- NA_real_  # safety
      
      fc <- tryCatch(ugarchforecast(best_fit, n.ahead = 1, n.roll = block_len - 1L),
                     error = function(e) NULL)
      if (is.null(fc)) {
        return(data.frame(time_index = rep(bstart, block_len),
                          step = 1:block_len,
                          timestamp = test_data$Timestamp[bstart + 0:(block_len - 1L)],
                          forecast = NA_real_,
                          actual   = test_data$log_return[bstart + 0:(block_len - 1L)]^2,
                          fit_failed = 1L,
                          shape_hat = shape_hat))
      }
      
      sigmas <- suppressWarnings(as.numeric(sigma(fc)))
      if (length(sigmas) >= block_len) sigmas <- tail(sigmas, block_len)
      if (length(sigmas) <  block_len) sigmas <- c(sigmas, rep(NA_real_, block_len - length(sigmas)))
      
      forecasts <- sigmas^2
      actuals   <- test_data$log_return[bstart + 0:(block_len - 1L)]^2
      stamps    <- test_data$Timestamp[bstart + 0:(block_len - 1L)]
      
      data.frame(
        time_index = rep(bstart, block_len),
        step       = 1:block_len,
        timestamp  = stamps,
        forecast   = forecasts,
        actual     = actuals,
        fit_failed = as.integer(!(is.finite(forecasts) & is.finite(actuals))),
        shape_hat  = rep(shape_hat, length(stamps))
      )
    }, error = function(e) {
      stamps <- tryCatch(test_data$Timestamp[bstart + 0:(block_len - 1L)],
                         error = function(.) as.POSIXct(rep(NA, block_len), origin="1970-01-01", tz="UTC"))
      acts   <- tryCatch(test_data$log_return[bstart + 0:(block_len - 1L)]^2,
                         error = function(.) rep(NA_real_, block_len))
      data.frame(
        time_index = rep(bstart, block_len),
        step       = 1:block_len,
        timestamp  = stamps,
        forecast   = NA_real_,
        actual     = acts,
        fit_failed = 1L,
        shape_hat  = NA_real_
      )
    })
  }
  
  if (!nrow(block_rows)) {
    return(list(
      summary = data.frame(freq=freq_label, window=window_size, step_ahead=1,
                           mse=NA, mae=NA, rmse=NA, qlike=NA, n=0,
                           failed_fits=0, coverage=NA, shape_hat=NA),
      details = data.frame()
    ))
  }
  
  valid <- block_rows$fit_failed == 0
  err   <- block_rows$forecast[valid] - block_rows$actual[valid]
  
  qlike <- function(rv, sig2) {
    rv   <- pmax(rv,   .Machine$double.eps)
    sig2 <- pmax(sig2, .Machine$double.eps)
    rv/sig2 - log(rv/sig2) - 1
  }
  ql_val      <- qlike(block_rows$actual[valid], block_rows$forecast[valid])
  forecast_sd <- sqrt(block_rows$forecast[valid])
  
  # Realized returns aligned to valid timestamps (index, not join)
  idx_valid <- match(block_rows$timestamp[valid], test_data$Timestamp)
  r_map     <- test_data$log_return[idx_valid]
  
  # Coverage with block-specific estimated df (shape_hat)
  # Use a single df per block (constant within block_rows$time_index)
  # We'll compute coverage per-row using the block's shape_hat
  cov_mask <- valid & is.finite(block_rows$shape_hat)
  coverage <- NA_real_
  if (any(cov_mask)) {
    # For rows in the same block, shape_hat is constant; safe to vectorize:
    q975 <- rugarch::qdist("std", 0.975, shape = block_rows$shape_hat[cov_mask][1])
    lower <- -q975 * forecast_sd[cov_mask]
    upper <-  q975 * forecast_sd[cov_mask]
    coverage <- mean(r_map[cov_mask] >= lower & r_map[cov_mask] <= upper, na.rm = TRUE)
  }
  
  # Use the first non-NA shape_hat among valid rows as the block estimate for summary
  shape_hat_summary <- suppressWarnings(
    as.numeric(stats::na.omit(block_rows$shape_hat[valid]))[1]
  )
  if (!is.finite(shape_hat_summary)) shape_hat_summary <- NA_real_
  
  summary_df <- data.frame(
    freq        = freq_label,
    window      = window_size,
    step_ahead  = 1L,
    mse         = mean(err^2, na.rm = TRUE),
    mae         = mean(abs(err), na.rm = TRUE),
    rmse        = sqrt(mean(err^2, na.rm = TRUE)),
    qlike       = mean(ql_val, na.rm = TRUE),
    n           = sum(valid),
    failed_fits = sum(block_rows$fit_failed),
    coverage    = coverage,
    shape_hat   = shape_hat_summary
  )
  
  list(summary = summary_df, details = block_rows)
}

# =========================================
#               RUN FORECASTS
# =========================================

# Params
window_size <- 200L
block_len   <- 60L
stride      <- 60L
n_starts    <- 5L
set.seed(123)

pair_summaries <- list()
pair_details   <- list()

pb_pairs <- cli::cli_progress_bar(
  name  = "Pairs",
  total = length(selected_splits),
  clear = FALSE
)
on.exit(try(cli::cli_progress_done(id = pb_pairs), silent = TRUE), add = TRUE)

for (nm in selected_splits) {
  splits <- get(nm)
  pair   <- sub("_splits$", "", nm)
  
  period_ids <- names(splits)
  pb_periods <- cli::cli_progress_bar(
    name  = paste0(pair, " periods"),
    total = length(period_ids),
    clear = FALSE
  )
  
  withCallingHandlers({
    res_list <- vector("list", length(period_ids))
    names(res_list) <- period_ids
    
    for (k in seq_along(period_ids)) {
      pid <- period_ids[k]
      s   <- splits[[pid]]
      
      if (nrow(s$train) < window_size || nrow(s$test) < block_len) {
        res_list[[k]] <- NULL
        try(cli::cli_progress_update(id = pb_periods, inc = 1, status = paste(pid, "(skipped)")), silent = TRUE)
        cli::cli_alert_warning("{pair}:{pid} skipped (insufficient data)")
        next
      }
      
      res_list[[k]] <- refit_blockwise_stride_dfhat(
        train_data  = s$train,
        test_data   = s$test,
        window_size = window_size,
        block_len   = block_len,
        stride      = stride,
        n_starts    = n_starts,
        freq_label  = paste0(pair, ":", pid)
      )
      
      try(cli::cli_progress_update(id = pb_periods, inc = 1, status = pid), silent = TRUE)
      done_n <- if (!is.null(res_list[[k]])) nrow(res_list[[k]]$details) else 0L
      cli::cli_alert_success("{pair}:{pid} finished ({done_n} forecasts)")
    }
  }, finally = {
    try(cli::cli_progress_done(id = pb_periods), silent = TRUE)
  })
  
  res_list <- Filter(Negate(is.null), res_list)
  sum_tbl <- if (length(res_list)) dplyr::bind_rows(lapply(res_list, `[[`, "summary")) else tibble::tibble()
  det_tbl <- if (length(res_list)) dplyr::bind_rows(lapply(res_list, `[[`, "details")) %>%
    dplyr::mutate(pair = pair, .before = 1) else tibble::tibble()
  
  pair_summaries[[pair]] <- sum_tbl
  pair_details[[pair]]   <- det_tbl
  
  cli::cli_alert_success("✓ {pair}: {nrow(sum_tbl)} periods processed")
  try(cli::cli_progress_update(id = pb_pairs, inc = 1, status = paste(pair, "done")), silent = TRUE)
}

try(cli::cli_progress_done(id = pb_pairs), silent = TRUE)

# Combine all pairs
all_summaries <- dplyr::bind_rows(pair_summaries)
all_details   <- dplyr::bind_rows(pair_details)

# Save (so you don’t lose results)
readr::write_csv(all_summaries, "gjr_dfhat_summaries.csv")
readr::write_csv(all_details,   "gjr_dfhat_details.csv")

cat("Saved: gjr_dfhat_summaries.csv and gjr_dfhat_details.csv\n")



## =========================================
##  After running the blockwise forecast loop
## =========================================

# Combine all pairs (already done in your pipeline)
all_summaries <- dplyr::bind_rows(pair_summaries)
all_details   <- dplyr::bind_rows(pair_details)

# Save
readr::write_csv(all_summaries, "gjr_dfhat_summaries.csv")
readr::write_csv(all_details,   "gjr_dfhat_details.csv")

cat("Saved: gjr_dfhat_summaries.csv and gjr_dfhat_details.csv\n")

# =========================================
#              PLOT ν OVER TIME
# =========================================

# Use summaries: each block/period has a single ν estimate
ggplot(all_summaries, aes(x = freq, y = shape_hat, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Estimated Student-t Degrees of Freedom (ν) over Periods",
    x = "Period",
    y = "Estimated ν (shape_hat)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# =========================================
#   Option: Plot ν at block resolution
# =========================================

# If you want ν estimates per block from details instead of summaries:
ggplot(all_details %>% filter(!is.na(shape_hat)),
       aes(x = timestamp, y = shape_hat, color = pair)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~pair, scales = "free_x") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Estimated Student-t ν over time (blockwise)",
    x = "Time",
    y = "ν (shape_hat)"
  )
