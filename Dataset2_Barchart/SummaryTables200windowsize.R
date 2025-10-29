# Summary Tables for Multi-step forecasting on barchart files
# ============================
# Post-processing for new GJR outputs (END-ALIGNED files)
# ============================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(forecast)
})

.must_have <- function(df, cols) {
  miss <- setdiff(cols, names(df))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse=", "))
  df
}

# Read and sanity-check a test file
read_test <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE)
  req <- c(
    "summary.freq","summary.window","summary.step_ahead",
    "summary.mse","summary.mae","summary.rmse",
    "summary.n","summary.failed_fits",
    "summary.coverage_tdf10","summary.qlike","summary.quasi_deviance",
    "details.time_index","details.step","details.timestamp",
    "details.forecast","details.actual","details.fit_failed"
  )
  .must_have(df, req)
}

# --- Utilities ---------------------------------------------------------------

# Mark NAs as failed fits
flag_na_as_failed <- function(df) {
  df %>%
    mutate(
      `details.fit_failed` = ifelse(
        is.na(`details.forecast`) | is.na(`details.actual`),
        1L, as.integer(`details.fit_failed`)
      )
    )
}

# Detect period resets: new block whenever time_index decreases in time order
add_block_id <- function(df) {
  df %>%
    arrange(`details.timestamp`, `details.step`) %>%  # chronological
    mutate(
      .prev_idx  = dplyr::lag(`details.time_index`, default = first(`details.time_index`)),
      .new_block = as.integer(`details.time_index` < .prev_idx),
      .block     = cumsum(.new_block)
    ) %>%
    select(-.prev_idx, -.new_block)
}

# Keep only valid rows and aggregate by origin within each block
aggregate_forecasts <- function(df) {
  df %>%
    filter(`details.fit_failed` == 0) %>%
    group_by(.block, `details.time_index`) %>%
    summarise(
      forecast_time  = max(`details.timestamp`, na.rm = TRUE),
      total_forecast = sum(`details.forecast`, na.rm = TRUE),
      total_actual   = sum(`details.actual`,   na.rm = TRUE),
      steps_included = paste(sort(unique(`details.step`)), collapse = ", "),
      .groups = "drop"
    ) %>%
    # guard in case of rare duplicate times
    distinct(forecast_time, .keep_all = TRUE)
}

compute_metrics <- function(actual, predicted) {
  mae  <- mean(abs(actual - predicted), na.rm = TRUE)
  mse  <- mean((actual - predicted)^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  c(MAE = mae, MSE = mse, RMSE = rmse)
}

dm_compare_both <- function(actual, fA, fB, h = 1, alt = "two.sided") {
  eA <- actual - fA
  eB <- actual - fB
  list(
    MSE = forecast::dm.test(e1 = eA, e2 = eB, h = h, power = 2, alternative = alt),
    MAE = forecast::dm.test(e1 = eA, e2 = eB, h = h, power = 1, alternative = alt)
  )
}

# --- Load files --------------------------------------------------------------

test_1m_5step  <- read_test("test_1m_5step.csv")  |> flag_na_as_failed() |> add_block_id()
test_5m_1step  <- read_test("test_5m_1step.csv")  |> flag_na_as_failed() |> add_block_id()
test_5m_3step  <- read_test("test_5m_3step.csv")  |> flag_na_as_failed() |> add_block_id()
test_5m_6step  <- read_test("test_5m_6step.csv")  |> flag_na_as_failed() |> add_block_id()
test_15m_1step <- read_test("test_15m_1step.csv") |> flag_na_as_failed() |> add_block_id()
test_15m_2step <- read_test("test_15m_2step.csv") |> flag_na_as_failed() |> add_block_id()
test_15m_4step <- read_test("test_15m_4step.csv") |> flag_na_as_failed() |> add_block_id()
test_30m_1step <- read_test("test_30m_1step.csv") |> flag_na_as_failed() |> add_block_id()
test_30m_2step <- read_test("test_30m_2step.csv") |> flag_na_as_failed() |> add_block_id()
test_60m_1step <- read_test("test_1h_1step.csv")  |> flag_na_as_failed() |> add_block_id()

# --- One-row summary table ---------------------------------------------------

get_summary_row <- function(df, file_label) {
  stopifnot(nrow(df) >= 1)
  tibble(
    file                    = file_label,
    summary.freq            = df$summary.freq[1],
    summary.window          = df$summary.window[1],
    summary.step_ahead      = df$summary.step_ahead[1],
    summary.mse             = df$summary.mse[1],
    summary.mae             = df$summary.mae[1],
    summary.rmse            = df$summary.rmse[1],
    summary.n               = df$summary.n[1],
    summary.failed_fits     = df$summary.failed_fits[1],
    summary.coverage_tdf10  = df$summary.coverage_tdf10[1],
    summary.qlike           = df$summary.qlike[1],
    summary.quasi_deviance  = df$summary.quasi_deviance[1]
  )
}

summary_table <- dplyr::bind_rows(
  get_summary_row(test_60m_1step,  "test_1h_1step"),
  get_summary_row(test_30m_2step,  "test_30m_2step"),
  get_summary_row(test_15m_4step,  "test_15m_4step"),
  get_summary_row(test_30m_1step,  "test_30m_1step"),
  get_summary_row(test_15m_2step,  "test_15m_2step"),
  get_summary_row(test_5m_6step,   "test_5m_6step"),
  get_summary_row(test_15m_1step,  "test_15m_1step"),
  get_summary_row(test_5m_3step,   "test_5m_3step"),
  get_summary_row(test_5m_1step,   "test_5m_1step"),
  get_summary_row(test_1m_5step,   "test_1m_5step")
)

print(summary_table)

# --- Aggregate by origin (block-safe) ---------------------------------------

agg_1m_5step   <- aggregate_forecasts(test_1m_5step)
agg_5m_1step   <- aggregate_forecasts(test_5m_1step)
agg_5m_3step   <- aggregate_forecasts(test_5m_3step)
agg_5m_6step   <- aggregate_forecasts(test_5m_6step)
agg_15m_1step  <- aggregate_forecasts(test_15m_1step)
agg_15m_2step  <- aggregate_forecasts(test_15m_2step)
agg_15m_4step  <- aggregate_forecasts(test_15m_4step)
agg_30m_1step  <- aggregate_forecasts(test_30m_1step)
agg_30m_2step  <- aggregate_forecasts(test_30m_2step)
agg_60m_1step  <- aggregate_forecasts(test_60m_1step)

# --- Panels & metrics --------------------------------------------------------

# 1H base
panel_1h <- agg_60m_1step %>%
  rename(forecast_1h = total_forecast, actual_1h = total_actual) %>%
  select(forecast_time, forecast_1h, actual_1h) %>%
  inner_join(agg_30m_2step %>%
               rename(forecast_30m = total_forecast, actual_30m = total_actual) %>%
               select(forecast_time, forecast_30m, actual_30m),
             by = "forecast_time") %>%
  inner_join(agg_15m_4step %>%
               rename(forecast_15m = total_forecast, actual_15m = total_actual) %>%
               select(forecast_time, forecast_15m, actual_15m),
             by = "forecast_time") %>%
  arrange(forecast_time)

metrics_1h  <- compute_metrics(panel_1h$actual_1h, panel_1h$forecast_1h)
metrics_30m <- compute_metrics(panel_1h$actual_1h, panel_1h$forecast_30m)
metrics_15m <- compute_metrics(panel_1h$actual_1h, panel_1h$forecast_15m)

# 30M base
panel_30m <- agg_30m_1step %>%
  rename(forecast_30m = total_forecast, actual_30m = total_actual) %>%
  select(forecast_time, forecast_30m, actual_30m) %>%
  inner_join(agg_15m_2step %>%
               rename(forecast_15m = total_forecast, actual_15m = total_actual) %>%
               select(forecast_time, forecast_15m, actual_15m),
             by = "forecast_time") %>%
  inner_join(agg_5m_6step %>%
               rename(forecast_5m = total_forecast, actual_5m = total_actual) %>%
               select(forecast_time, forecast_5m, actual_5m),
             by = "forecast_time") %>%
  arrange(forecast_time)

metrics_30m_b <- compute_metrics(panel_30m$actual_30m, panel_30m$forecast_30m)
metrics_15m_b <- compute_metrics(panel_30m$actual_30m, panel_30m$forecast_15m)
metrics_5m_b  <- compute_metrics(panel_30m$actual_30m, panel_30m$forecast_5m)

# 15M base
panel_15m <- agg_15m_1step %>%
  rename(forecast_15m = total_forecast, actual_15m = total_actual) %>%
  select(forecast_time, forecast_15m, actual_15m) %>%
  inner_join(agg_5m_3step %>%
               rename(forecast_5m = total_forecast, actual_5m = total_actual) %>%
               select(forecast_time, forecast_5m, actual_5m),
             by = "forecast_time") %>%
  arrange(forecast_time)

metrics_15m_c <- compute_metrics(panel_15m$actual_15m, panel_15m$forecast_15m)
metrics_5m_c  <- compute_metrics(panel_15m$actual_15m, panel_15m$forecast_5m)

# 5M base
panel_5m <- agg_5m_1step %>%
  rename(forecast_5m = total_forecast, actual_5m = total_actual) %>%
  select(forecast_time, forecast_5m, actual_5m) %>%
  inner_join(
    agg_1m_5step %>%
      rename(forecast_1m = total_forecast, actual_1m = total_actual) %>%
      select(forecast_time, forecast_1m, actual_1m),
    by = "forecast_time"
  ) %>%
  arrange(forecast_time)

metrics_5m_d <- compute_metrics(panel_5m$actual_5m, panel_5m$forecast_5m)   # 5m direct
metrics_1m_d <- compute_metrics(panel_5m$actual_5m, panel_5m$forecast_1m)   # 1m 5-step

# --- DM tests ---------------------------------------------------------------

dm_1h_vs_30m       <- dm_compare_both(panel_1h$actual_1h, panel_1h$forecast_1h,  panel_1h$forecast_30m)
dm_1h_vs_15m       <- dm_compare_both(panel_1h$actual_1h, panel_1h$forecast_1h,  panel_1h$forecast_15m)
dm_30m_vs_15m_in1h <- dm_compare_both(panel_1h$actual_1h, panel_1h$forecast_30m, panel_1h$forecast_15m)

dm_30m_vs_15m      <- dm_compare_both(panel_30m$actual_30m, panel_30m$forecast_30m, panel_30m$forecast_15m)
dm_30m_vs_5m       <- dm_compare_both(panel_30m$actual_30m, panel_30m$forecast_30m, panel_30m$forecast_5m)
dm_15m_vs_5m_in30m <- dm_compare_both(panel_30m$actual_30m, panel_30m$forecast_15m, panel_30m$forecast_5m)

dm_15m_vs_5m       <- dm_compare_both(panel_15m$actual_15m, panel_15m$forecast_15m, panel_15m$forecast_5m)
dm_5m_vs_1m        <- dm_compare_both(panel_5m$actual_5m, panel_5m$forecast_5m, panel_5m$forecast_1m)

# --- Write outputs ----------------------------------------------------------

readr::write_csv(summary_table, "pp_summary_table.csv")
readr::write_csv(panel_1h,       "pp_panel_1h.csv")
readr::write_csv(panel_30m,      "pp_panel_30m.csv")
readr::write_csv(panel_15m,      "pp_panel_15m.csv")
cat("\nSaved: pp_summary_table.csv, pp_panel_1h.csv, pp_panel_30m.csv, pp_panel_15m.csv\n")

# Return DM test objects to console
dm_1h_vs_30m
dm_1h_vs_15m
dm_30m_vs_15m_in1h

dm_30m_vs_15m
dm_30m_vs_5m
dm_15m_vs_5m_in30m

dm_15m_vs_5m
dm_5m_vs_1m

# ============================
# Dataset 2: console tables matching Dataset 1 style
# ============================

# 1) Compare forecasts against ACTUAL 1H variance
forecast_errors_1h <- data.frame(
  Model = c("1H Forecast", "30M Forecast", "15M Forecast"),
  MAE   = c(metrics_1h["MAE"],  metrics_30m["MAE"],  metrics_15m["MAE"]),
  MSE   = c(metrics_1h["MSE"],  metrics_30m["MSE"],  metrics_15m["MSE"]),
  RMSE  = c(metrics_1h["RMSE"], metrics_30m["RMSE"], metrics_15m["RMSE"])
)
print(forecast_errors_1h)

# 2) Compare forecasts against ACTUAL 30M variance
forecast_errors_30m <- data.frame(
  Model = c("30M Forecast", "15M Forecast", "5M Forecast"),
  MAE   = c(metrics_30m_b["MAE"],  metrics_15m_b["MAE"],  metrics_5m_b["MAE"]),
  MSE   = c(metrics_30m_b["MSE"],  metrics_15m_b["MSE"],  metrics_5m_b["MSE"]),
  RMSE  = c(metrics_30m_b["RMSE"], metrics_15m_b["RMSE"], metrics_5m_b["RMSE"])
)
print(forecast_errors_30m)

# 3) Compare forecasts against ACTUAL 15M variance
forecast_errors_15m <- data.frame(
  Model = c("15M Forecast", "5M Forecast"),
  MAE   = c(metrics_15m_c["MAE"],  metrics_5m_c["MAE"]),
  MSE   = c(metrics_15m_c["MSE"],  metrics_5m_c["MSE"]),
  RMSE  = c(metrics_15m_c["RMSE"], metrics_5m_c["RMSE"])
)
print(forecast_errors_15m)

# 4) Compare forecasts against ACTUAL 5M variance
forecast_errors_5m <- data.frame(
  Model = c("5M Forecast", "1M Forecast (5-step)"),
  MAE   = c(metrics_5m_d["MAE"],  metrics_1m_d["MAE"]),
  MSE   = c(metrics_5m_d["MSE"],  metrics_1m_d["MSE"]),
  RMSE  = c(metrics_5m_d["RMSE"], metrics_1m_d["RMSE"])
)
print(forecast_errors_5m)
