# Libraries
library(readxl)
library(DT)
library(purrr)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)

# Read each file into its own variable
test_1h_1step     <- read_csv("test_1h_1step.csv")
test_5m_1step     <- read_csv("test_5m_1step.csv")
test_5m_3step     <- read_csv("test_5m_3step.csv")
test_5m_6step     <- read_csv("test_5m_6step.csv")
test_5m_12step    <- read_csv("test_5m_12step.csv")
test_15m_1step    <- read_csv("test_15m_1step.csv")
test_15m_2step    <- read_csv("test_15m_2step.csv")
test_15m_4step    <- read_csv("test_15m_4step.csv")
test_30m_1step    <- read_csv("test_30m_1step.csv")
test_30m_2step    <- read_csv("test_30m_2step.csv")

# Create a data frame summarising the first row of each forecast result for summary columns
summary_table <- data.frame(
  file = c(
    "test_1h_1step",
    "test_30m_2step",
    "test_15m_4step",
    "test_5m_12step",
    "test_30m_1step",
    "test_15m_2step",
    "test_5m_6step",
    "test_15m_1step",
    "test_5m_3step",
    "test_5m_1step"
  ),
  summary.freq = c(
    test_1h_1step$summary.freq[1],
    test_30m_2step$summary.freq[1],
    test_15m_4step$summary.freq[1],
    test_5m_12step$summary.freq[1],
    test_30m_1step$summary.freq[1],
    test_15m_2step$summary.freq[1],
    test_5m_6step$summary.freq[1],
    test_15m_1step$summary.freq[1],
    test_5m_3step$summary.freq[1],
    test_5m_1step$summary.freq[1]
  ),
  summary.window = c(
    test_1h_1step$summary.window[1],
    test_30m_2step$summary.window[1],
    test_15m_4step$summary.window[1],
    test_5m_12step$summary.window[1],
    test_30m_1step$summary.window[1],
    test_15m_2step$summary.window[1],
    test_5m_6step$summary.window[1],
    test_15m_1step$summary.window[1],
    test_5m_3step$summary.window[1],
    test_5m_1step$summary.window[1]
  ),
  summary.step_ahead = c(
    test_1h_1step$summary.step_ahead[1],
    test_30m_2step$summary.step_ahead[1],
    test_15m_4step$summary.step_ahead[1],
    test_5m_12step$summary.step_ahead[1],
    test_30m_1step$summary.step_ahead[1],
    test_15m_2step$summary.step_ahead[1],
    test_5m_6step$summary.step_ahead[1],
    test_15m_1step$summary.step_ahead[1],
    test_5m_3step$summary.step_ahead[1],
    test_5m_1step$summary.step_ahead[1]
  ),
  summary.mse = c(
    test_1h_1step$summary.mse[1],
    test_30m_2step$summary.mse[1],
    test_15m_4step$summary.mse[1],
    test_5m_12step$summary.mse[1],
    test_30m_1step$summary.mse[1],
    test_15m_2step$summary.mse[1],
    test_5m_6step$summary.mse[1],
    test_15m_1step$summary.mse[1],
    test_5m_3step$summary.mse[1],
    test_5m_1step$summary.mse[1]
  ),
  summary.mae = c(
    test_1h_1step$summary.mae[1],
    test_30m_2step$summary.mae[1],
    test_15m_4step$summary.mae[1],
    test_5m_12step$summary.mae[1],
    test_30m_1step$summary.mae[1],
    test_15m_2step$summary.mae[1],
    test_5m_6step$summary.mae[1],
    test_15m_1step$summary.mae[1],
    test_5m_3step$summary.mae[1],
    test_5m_1step$summary.mae[1]
  ),
  summary.rmse = c(
    test_1h_1step$summary.rmse[1],
    test_30m_2step$summary.rmse[1],
    test_15m_4step$summary.rmse[1],
    test_5m_12step$summary.rmse[1],
    test_30m_1step$summary.rmse[1],
    test_15m_2step$summary.rmse[1],
    test_5m_6step$summary.rmse[1],
    test_15m_1step$summary.rmse[1],
    test_5m_3step$summary.rmse[1],
    test_5m_1step$summary.rmse[1]
  ),
  summary.n = c(
    test_1h_1step$summary.n[1],
    test_30m_2step$summary.n[1],
    test_15m_4step$summary.n[1],
    test_5m_12step$summary.n[1],
    test_30m_1step$summary.n[1],
    test_15m_2step$summary.n[1],
    test_5m_6step$summary.n[1],
    test_15m_1step$summary.n[1],
    test_5m_3step$summary.n[1],
    test_5m_1step$summary.n[1]
  ),
  summary.failed_fits = c(
    test_1h_1step$summary.failed_fits[1],
    test_30m_2step$summary.failed_fits[1],
    test_15m_4step$summary.failed_fits[1],
    test_5m_12step$summary.failed_fits[1],
    test_30m_1step$summary.failed_fits[1],
    test_15m_2step$summary.failed_fits[1],
    test_5m_6step$summary.failed_fits[1],
    test_15m_1step$summary.failed_fits[1],
    test_5m_3step$summary.failed_fits[1],
    test_5m_1step$summary.failed_fits[1]
  )
)

# View the resulting summary table
print(summary_table)





## Create a data frame summarising that groups entries for the same period together and aggregates the prediction differences to get a different view
# Define aggregation function
aggregate_forecasts <- function(df) {
  df %>%
    filter(!is.na(details.forecast), !is.na(details.actual)) %>%
    group_by(details.time_index) %>%
    summarise(
      forecast_time   = max(details.timestamp),
      total_forecast  = sum(details.forecast),
      total_actual    = sum(details.actual),
      steps_included  = paste(sort(unique(details.step)), collapse = ", "),
      .groups = "drop"
    )
}

# Apply aggregation function to each dataset
agg_1h_1step    <- aggregate_forecasts(test_1h_1step)
agg_30m_1step   <- aggregate_forecasts(test_30m_1step)
agg_30m_2step   <- aggregate_forecasts(test_30m_2step)
agg_15m_1step   <- aggregate_forecasts(test_15m_1step)
agg_15m_2step   <- aggregate_forecasts(test_15m_2step)
agg_15m_4step   <- aggregate_forecasts(test_15m_4step)
agg_5m_3step    <- aggregate_forecasts(test_5m_3step)
agg_5m_6step    <- aggregate_forecasts(test_5m_6step)
agg_5m_12step   <- aggregate_forecasts(test_5m_12step)

# match the timestamps to comparison datasets
# Rename columns for clarity aligned to 1 hour 1 step
agg_1h_1step_renamed <- agg_1h_1step %>%
  rename(
    forecast_1h   = total_forecast,
    actual_1h     = total_actual
  ) %>%
  select(forecast_time, forecast_1h, actual_1h)

agg_30m_2step_renamed <- agg_30m_2step %>%
  rename(
    forecast_30m   = total_forecast,
    actual_30m     = total_actual
  ) %>%
  select(forecast_time, forecast_30m, actual_30m)

agg_15m_4step_renamed <- agg_15m_4step %>%
  rename(
    forecast_15m   = total_forecast,
    actual_15m     = total_actual
  ) %>%
  select(forecast_time, forecast_15m, actual_15m)

# Perform intersection join on forecast_time
combined_forecasts <- agg_1h_1step_renamed %>%
  inner_join(agg_30m_2step_renamed, by = "forecast_time") %>%
  inner_join(agg_15m_4step_renamed, by = "forecast_time") %>%
  arrange(forecast_time)

# Function to compute error metrics
compute_metrics <- function(actual, predicted) {
  mae  <- mean(abs(actual - predicted), na.rm = TRUE)
  mse  <- mean((actual - predicted)^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  return(c(MAE = mae, MSE = mse, RMSE = rmse))
}

# Apply the function to each forecast model
metrics_1h  <- compute_metrics(combined_forecasts$actual_1h, combined_forecasts$forecast_1h)
metrics_30m <- compute_metrics(combined_forecasts$actual_1h, combined_forecasts$forecast_30m)
metrics_15m <- compute_metrics(combined_forecasts$actual_1h, combined_forecasts$forecast_15m)

# Combine into a single table for comparison
forecast_errors <- data.frame(
  Model = c("1H Forecast", "30M Forecast", "15M Forecast"),
  MAE   = c(metrics_1h["MAE"], metrics_30m["MAE"], metrics_15m["MAE"]),
  MSE   = c(metrics_1h["MSE"], metrics_30m["MSE"], metrics_15m["MSE"]),
  RMSE  = c(metrics_1h["RMSE"], metrics_30m["RMSE"], metrics_15m["RMSE"])
)

print(forecast_errors)




# Rename columns for clarity aligned to 30 minute 1 step
agg_30m_1step_renamed <- agg_30m_1step %>%
  rename(
    forecast_30m   = total_forecast,
    actual_30m     = total_actual
  ) %>%
  select(forecast_time, forecast_30m, actual_30m)

agg_15m_2step_renamed <- agg_15m_2step %>%
  rename(
    forecast_15m   = total_forecast,
    actual_15m     = total_actual
  ) %>%
  select(forecast_time, forecast_15m, actual_15m)

agg_5m_6step_renamed <- agg_5m_6step %>%
  rename(
    forecast_5m    = total_forecast,
    actual_5m      = total_actual
  ) %>%
  select(forecast_time, forecast_5m, actual_5m)

# Inner join by forecast_time
combined_forecasts_30m <- agg_30m_1step_renamed %>%
  inner_join(agg_15m_2step_renamed, by = "forecast_time") %>%
  inner_join(agg_5m_6step_renamed, by = "forecast_time") %>%
  arrange(forecast_time)

# Apply to each forecast
metrics_30m  <- compute_metrics(combined_forecasts_30m$actual_30m, combined_forecasts_30m$forecast_30m)
metrics_15m  <- compute_metrics(combined_forecasts_30m$actual_30m, combined_forecasts_30m$forecast_15m)
metrics_5m   <- compute_metrics(combined_forecasts_30m$actual_30m, combined_forecasts_30m$forecast_5m)

# Combine into comparison table
forecast_errors_30m <- data.frame(
  Model = c("30M Forecast", "15M Forecast", "5M Forecast"),
  MAE   = c(metrics_30m["MAE"], metrics_15m["MAE"], metrics_5m["MAE"]),
  MSE   = c(metrics_30m["MSE"], metrics_15m["MSE"], metrics_5m["MSE"]),
  RMSE  = c(metrics_30m["RMSE"], metrics_15m["RMSE"], metrics_5m["RMSE"])
)

print(forecast_errors_30m)


# Rename columns for clarity aligned to 15 minute 1 step
agg_15m_1step_renamed <- agg_15m_1step %>%
  rename(
    forecast_15m   = total_forecast,
    actual_15m     = total_actual
  ) %>%
  select(forecast_time, forecast_15m, actual_15m)

agg_5m_3step_renamed <- agg_5m_3step %>%
  rename(
    forecast_5m    = total_forecast,
    actual_5m      = total_actual
  ) %>%
  select(forecast_time, forecast_5m, actual_5m)

# Join on forecast_time
combined_forecasts_15m <- agg_15m_1step_renamed %>%
  inner_join(agg_5m_3step_renamed, by = "forecast_time") %>%
  arrange(forecast_time)

# Compute metrics using actual_15m as the benchmark
metrics_15m <- compute_metrics(combined_forecasts_15m$actual_15m, combined_forecasts_15m$forecast_15m)
metrics_5m  <- compute_metrics(combined_forecasts_15m$actual_15m, combined_forecasts_15m$forecast_5m)

# Combine into a results table
forecast_errors_15m <- data.frame(
  Model = c("15M Forecast", "5M Forecast"),
  MAE   = c(metrics_15m["MAE"], metrics_5m["MAE"]),
  MSE   = c(metrics_15m["MSE"], metrics_5m["MSE"]),
  RMSE  = c(metrics_15m["RMSE"], metrics_5m["RMSE"])
)

print(forecast_errors_15m)



## Diabold Mariano Test

# Wrapper: run both MSE & MAE versions
dm_compare_both <- function(actual, fA, fB, h = 1, alt = "two.sided") {
  eA <- actual - fA
  eB <- actual - fB
  list(
    MSE = forecast::dm.test(e1 = eA, e2 = eB, h = h, power = 2, alternative = alt),
    MAE = forecast::dm.test(e1 = eA, e2 = eB, h = h, power = 1, alternative = alt)
  )
}

### ===== 1H BASE CASE =====
dm_1h_vs_30m        <- dm_compare_both(combined_forecasts$actual_1h,
                                       combined_forecasts$forecast_1h,
                                       combined_forecasts$forecast_30m)
dm_1h_vs_15m        <- dm_compare_both(combined_forecasts$actual_1h,
                                       combined_forecasts$forecast_1h,
                                       combined_forecasts$forecast_15m)
dm_30m_vs_15m_in1h  <- dm_compare_both(combined_forecasts$actual_1h,
                                       combined_forecasts$forecast_30m,
                                       combined_forecasts$forecast_15m)

### ===== 30M BASE CASE =====
dm_30m_vs_15m       <- dm_compare_both(combined_forecasts_30m$actual_30m,
                                       combined_forecasts_30m$forecast_30m,
                                       combined_forecasts_30m$forecast_15m)
dm_30m_vs_5m        <- dm_compare_both(combined_forecasts_30m$actual_30m,
                                       combined_forecasts_30m$forecast_30m,
                                       combined_forecasts_30m$forecast_5m)
dm_15m_vs_5m_in30m  <- dm_compare_both(combined_forecasts_30m$actual_30m,
                                       combined_forecasts_30m$forecast_15m,
                                       combined_forecasts_30m$forecast_5m)

### ===== 15M BASE CASE =====
dm_15m_vs_5m        <- dm_compare_both(combined_forecasts_15m$actual_15m,
                                       combined_forecasts_15m$forecast_15m,
                                       combined_forecasts_15m$forecast_5m)

# Put into a named list
dm_results <- list(
  dm_1h_vs_30m       = dm_1h_vs_30m,
  dm_1h_vs_15m       = dm_1h_vs_15m,
  dm_30m_vs_15m_in1h = dm_30m_vs_15m_in1h,
  dm_30m_vs_15m      = dm_30m_vs_15m,
  dm_30m_vs_5m       = dm_30m_vs_5m,
  dm_15m_vs_5m_in30m = dm_15m_vs_5m_in30m,
  dm_15m_vs_5m       = dm_15m_vs_5m
)

# Example: printing MSE and MAE results for one comparison
dm_results
























### Extra Plotting subsets of data
# Pivot data to long format for plotting
plot_data <- combined_forecasts %>%
  select(forecast_time, actual_1h, forecast_1h, forecast_30m, forecast_15m) %>%
  pivot_longer(
    cols = c(forecast_1h, forecast_30m, forecast_15m),
    names_to = "model",
    values_to = "forecast"
  )

# Pivot forecast columns for easier plotting
plot_data <- combined_forecasts %>%
  select(forecast_time, actual_1h, forecast_1h, forecast_30m, forecast_15m) %>%
  pivot_longer(
    cols = c(forecast_1h, forecast_30m, forecast_15m),
    names_to = "model",
    values_to = "forecast"
  )

# Create ggplot with a 7-day zoom window
ggplot(plot_data, aes(x = forecast_time)) +
  geom_line(aes(y = actual_1h, color = "Actual 1H"), linetype = "dashed") +
  geom_line(aes(y = forecast, color = model), size = 0.8) +
  scale_color_manual(values = c(
    "Actual 1H"     = "black",
    "forecast_1h"   = "green",
    "forecast_30m"  = "red",
    "forecast_15m"  = "blue"
  ),
  labels = c("Actual 1H", "1H Forecast", "30M Forecast", "15M Forecast")) +
  labs(
    title = "Forecasted vs Actual 1-Hour Variance (Zoom: 7-Day Window)",
    x = "Time",
    y = "Variance",
    color = "Forecast Model"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = as.POSIXct(c("2024-06-10", "2024-06-17")))

