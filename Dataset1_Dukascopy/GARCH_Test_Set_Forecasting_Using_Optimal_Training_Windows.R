## GARCH Test Set forecasting using optimal training windows from validation set ##

## packages ##
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(zoo)
library(TTR)
library(gamlss.dist)
library(gamlss)
library(parallel)
library(pbapply)
library(moments)
library(caret)
library(tseries)
library(rugarch)
library(forecast)
library(DescTools)
library(Metrics)
library(doParallel)
library(foreach)
library(gridExtra)
library(scoringRules)
library(fitdistrplus)
library(reshape2)
library(extRemes)
library(progressr)
library(doParallel)
library(doFuture)
library(doRNG)
library(patchwork)



## Read all datasets ##
Ask5M  <- read.csv("GBPAUD_5 Mins_Ask_2022.01.01_2025.01.02.csv")
Bid5M  <- read.csv("GBPAUD_5 Mins_Bid_2022.01.01_2025.01.02.csv")
Ask15M <- read.csv("GBPAUD_15 Mins_Ask_2022.01.01_2025.01.02.csv")
Bid15M <- read.csv("GBPAUD_15 Mins_Bid_2022.01.01_2025.01.02.csv")
Ask30M <- read.csv("GBPAUD_30 Mins_Ask_2022.01.01_2025.01.02.csv")
Bid30M <- read.csv("GBPAUD_30 Mins_Bid_2022.01.01_2025.01.02.csv")
Ask1H  <- read.csv("GBPAUD_Hourly_Ask_2022.01.01_2025.01.02.csv")
Bid1H  <- read.csv("GBPAUD_Hourly_Bid_2022.01.01_2025.01.02.csv")

#Rename Timestamp Columns
Ask5M <- Ask5M %>% rename(Timestamp = `Time..EET.`)
Bid5M <- Bid5M %>% rename(Timestamp = `Time..EET.`)
Ask15M <- Ask15M %>% rename(Timestamp = `Time..EET.`)
Bid15M <- Bid15M %>% rename(Timestamp = `Time..EET.`)
Ask30M <- Ask30M %>% rename(Timestamp = `Time..EET.`)
Bid30M <- Bid30M %>% rename(Timestamp = `Time..EET.`)
Ask1H <- Ask1H %>% rename(Timestamp = `Time..EET.`)
Bid1H <- Bid1H %>% rename(Timestamp = `Time..EET.`)

#Change to POSIXct Format
Ask5M$Timestamp <- as.POSIXct(Ask5M$Timestamp, format="%Y.%m.%d %H:%M:%S", tz="EET")
Bid5M$Timestamp <- as.POSIXct(Bid5M$Timestamp, format="%Y.%m.%d %H:%M:%S", tz="EET")
Ask15M$Timestamp <- as.POSIXct(Ask15M$Timestamp, format="%Y.%m.%d %H:%M:%S", tz="EET")
Bid15M$Timestamp <- as.POSIXct(Bid15M$Timestamp, format="%Y.%m.%d %H:%M:%S", tz="EET")
Ask30M$Timestamp <- as.POSIXct(Ask30M$Timestamp, format="%Y.%m.%d %H:%M:%S", tz="EET")
Bid30M$Timestamp <- as.POSIXct(Bid30M$Timestamp, format="%Y.%m.%d %H:%M:%S", tz="EET")
Ask1H$Timestamp  <- as.POSIXct(Ask1H$Timestamp,  format="%Y.%m.%d %H:%M:%S", tz="EET")
Bid1H$Timestamp  <- as.POSIXct(Bid1H$Timestamp,  format="%Y.%m.%d %H:%M:%S", tz="EET")

# Function to merge Bid & Ask datasets and rename columns accordingly
merge_and_rename <- function(bid_df, ask_df, join_col = "Timestamp") {
  merged <- full_join(bid_df, ask_df, by = join_col, suffix = c("_bid", "_ask")) %>%
    rename(
      Bid_Open   = Open_bid,
      Bid_High   = High_bid,
      Bid_Low    = Low_bid,
      Bid_Close  = Close_bid,
      Bid_Volume = Volume_bid,
      Ask_Open   = Open_ask,
      Ask_High   = High_ask,
      Ask_Low    = Low_ask,
      Ask_Close  = Close_ask,
      Ask_Volume = Volume_ask
    )
  return(merged)
}

# Example usage for 5-minute data:
Combined5M <- merge_and_rename(Bid5M, Ask5M)
Combined15M <- merge_and_rename(Bid15M, Ask15M)
Combined30M <- merge_and_rename(Bid30M, Ask30M)
Combined1H <- merge_and_rename(Bid1H, Ask1H)



## Feature Engineering ##
# Function to add trading features for ARIMA, GARCH, and Transformer models
add_model_features <- function(df, volatility_window = 10) {
  df <- df %>%
    arrange(Timestamp) %>%  # Ensure data is ordered by Timestamp
    
    # Calculate Mid-Price (Common for All Models)
    mutate(
      Mid_Open  = (Bid_Open + Ask_Open) / 2,
      Mid_High  = (Bid_High + Ask_High) / 2,
      Mid_Low   = (Bid_Low + Ask_Low) / 2,
      Mid_Close = (Bid_Close + Ask_Close) / 2,
      
      # Calculate Log Returns
      Log_Returns = log(Mid_Close / lag(Mid_Close)),
      
      # Transformer Features: Spreads between Ask and Bid prices
      Spread_Open  = Ask_Open - Bid_Open,
      Spread_High  = Ask_High - Bid_High,
      Spread_Low   = Ask_Low - Bid_Low,
      Spread_Close = Ask_Close - Bid_Close,
      
      # Rolling Volatility using a rolling window
      Rolling_Volatility = rollapply(Log_Returns, width = volatility_window, FUN = sd, fill = NA, align = "right"),
      
      # Order Imbalance between Bid and Ask volumes
      Order_Imbalance = (Bid_Volume - Ask_Volume) / (Bid_Volume + Ask_Volume),
      
      # Simple and Exponential Moving Averages for Mid_Close
      SMA_5  = SMA(Mid_Close, n = 5),
      SMA_10 = SMA(Mid_Close, n = 10),
      EMA_5  = EMA(Mid_Close, n = 5),
      EMA_10 = EMA(Mid_Close, n = 10)
    ) %>%
    na.omit()  # Remove any rows with NA values caused by lagging
  
  return(df)
}

# Apply the function to each merged dataset
Combined5M  <- add_model_features(Combined5M)
Combined15M <- add_model_features(Combined15M)
Combined30M <- add_model_features(Combined30M)
Combined1H  <- add_model_features(Combined1H)

# Function to Compute Positional Encoding and attach it to the dataset
compute_positional_encoding <- function(df, d_model = 16) {
  # Ensure the data is ordered by Timestamp
  df <- df %>% arrange(Timestamp)
  
  max_seq_len <- nrow(df)  # Determine the number of rows dynamically
  positions <- seq(1, max_seq_len)
  pos_enc <- matrix(0, nrow = max_seq_len, ncol = d_model)
  
  # Compute sinusoidal positional encodings
  for (i in seq(1, d_model, by = 2)) {
    pos_enc[, i] <- sin(positions / (max_seq_len^(i / d_model)))
    if (i + 1 <= d_model) {
      pos_enc[, i + 1] <- cos(positions / (max_seq_len^(i / d_model)))
    }
  }
  
  pos_enc_df <- as.data.frame(pos_enc)
  colnames(pos_enc_df) <- paste0("Pos_Enc_", seq_len(d_model))
  
  # Attach the positional encoding columns to the original dataset
  return(cbind(df, pos_enc_df))
}

# Apply Positional Encoding to All Transformer Training Datasets
Combined5M  <- compute_positional_encoding(Combined5M)
Combined15M <- compute_positional_encoding(Combined15M)
Combined30M <- compute_positional_encoding(Combined30M)
Combined1H  <- compute_positional_encoding(Combined1H)

## Train and Test Splits##
## Train, Validation, and Test Splits ##

# Original 2-way split function
split_arima_data_by_date <- function(data, column_name, test_start_date) {
  if (!(column_name %in% colnames(data))) {
    stop(paste("Error:", column_name, "not found in dataset."))
  }
  test_start_date <- as.POSIXct(test_start_date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  data <- data[order(data[[column_name]]), ]
  test_start_index <- which(data[[column_name]] >= test_start_date)[1]
  if (is.na(test_start_index)) {
    stop("Error: Specified test_start_date is out of range.")
  }
  train_data <- data[1:(test_start_index - 1), ]
  test_data  <- data[test_start_index:nrow(data), ]
  return(list(train = train_data, test = test_data))
}

# New 3-way split function (validation within training set)
split_train_val_test <- function(data, column_name, test_start_date, validation_ratio = 0.2) {
  two_way <- split_arima_data_by_date(data, column_name, test_start_date)
  full_train <- two_way$train
  test_data  <- two_way$test
  val_cutoff <- floor(nrow(full_train) * (1 - validation_ratio))
  train_data <- full_train[1:val_cutoff, ]
  val_data   <- full_train[(val_cutoff + 1):nrow(full_train), ]
  return(list(train = train_data, val = val_data, test = test_data))
}

# Set test start date
fixed_test_start_date <- "2024-06-01 00:00:00"

# Apply the 3-way split to each frequency
split_5M  <- split_train_val_test(Combined5M,  "Timestamp", fixed_test_start_date)
split_15M <- split_train_val_test(Combined15M, "Timestamp", fixed_test_start_date)
split_30M <- split_train_val_test(Combined30M, "Timestamp", fixed_test_start_date)
split_1H  <- split_train_val_test(Combined1H,  "Timestamp", fixed_test_start_date)

# Extract train, validation, and test sets
train_5M <- split_5M$train; val_5M <- split_5M$val; test_5M <- split_5M$test
train_15M <- split_15M$train; val_15M <- split_15M$val; test_15M <- split_15M$test
train_30M <- split_30M$train; val_30M <- split_30M$val; test_30M <- split_30M$test
train_1H <- split_1H$train; val_1H <- split_1H$val; test_1H <- split_1H$test


# Display summaries for the training sets
summary(train_5M$Log_Returns)
summary(train_15M$Log_Returns)
summary(train_30M$Log_Returns)
summary(train_1H$Log_Returns)



#Forecasting on Test Set Using Optimal Training Windows
plan(multisession, workers = parallel::detectCores())
refit_rolling_forecast_multistep <- function(train_data, val_data, window_size, n_starts = 5, freq_label = "", n_ahead = 1, align_minutes = NULL) {
  cat(sprintf("===== PARALLEL Rolling Forecast: %s (Window = %d, Steps Ahead = %d, Multi-start = %d, Non-overlapping) =====\n",
              freq_label, window_size, n_ahead, n_starts))
  
  all_data       <- c(train_data$Log_Returns, val_data$Log_Returns)
  all_timestamps <- c(train_data$Timestamp, val_data$Timestamp)
  n_train        <- length(train_data$Log_Returns)
  n_val          <- length(val_data$Log_Returns)
  
  # Identify valid forecast start points
  steps <- seq(1, n_val - n_ahead + 1, by = n_ahead)
  
  # alignment filtering based on minute mark
  if (!is.null(align_minutes)) {
    align_filter <- format(val_data$Timestamp[steps], "%M") %in% align_minutes
    steps <- steps[align_filter]
  }
  
  results <- foreach(i = steps, .combine = rbind, .packages = "rugarch") %dorng% {
    tryCatch({
      window_start <- n_train + i - window_size
      window_end   <- n_train + i - 1
      
      if (!is.finite(window_start) || window_start < 1 || window_end > length(all_data)) {
        return(data.frame(time_index = i, step = NA, timestamp = NA, forecast = NA, actual = NA, fit_failed = 1))
      }
      
      window_data <- all_data[window_start:window_end]
      sd_val <- suppressWarnings(sd(window_data, na.rm = TRUE))
      
      if (!is.numeric(window_data) || length(window_data) < 5 || is.na(sd_val) || sd_val < .Machine$double.eps^0.5) {
        return(data.frame(time_index = i, step = NA, timestamp = NA, forecast = NA, actual = NA, fit_failed = 1))
      }
      
      spec <- ugarchspec(
        variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
        mean.model         = list(armaOrder = c(0, 0), include.mean = FALSE),
        distribution.model = "norm"
      )
      
      best_fit <- NULL
      best_llh <- -Inf
      
      for (j in 1:n_starts) {
        fit_try <- tryCatch({
          ugarchfit(spec, data = window_data, solver = "gosolnp",
                    fit.control = list(stationarity = 1),
                    solver.control = list(start = 1, trace = 0))
        }, error = function(e) NULL)
        
        if (!is.null(fit_try)) {
          llh <- tryCatch(likelihood(fit_try), error = function(e) NA_real_)
          if (is.finite(llh) && llh > best_llh) {
            best_llh <- llh
            best_fit <- fit_try
          }
        }
      }
      
      if (is.null(best_fit)) {
        return(data.frame(time_index = rep(i, n_ahead),
                          step = 1:n_ahead,
                          timestamp = rep(NA, n_ahead),
                          forecast = rep(NA, n_ahead),
                          actual = rep(NA, n_ahead),
                          fit_failed = rep(1, n_ahead)))
      }
      
      forecast <- tryCatch({
        ugarchforecast(best_fit, n.ahead = n_ahead)
      }, error = function(e) NULL)
      
      if (is.null(forecast)) {
        return(data.frame(time_index = rep(i, n_ahead),
                          step = 1:n_ahead,
                          timestamp = rep(NA, n_ahead),
                          forecast = rep(NA, n_ahead),
                          actual = rep(NA, n_ahead),
                          fit_failed = rep(1, n_ahead)))
      }
      
      forecast_vars <- tryCatch(as.numeric(sigma(forecast)[1:n_ahead])^2, error = function(e) rep(NA, n_ahead))
      actual_sqs    <- tryCatch(val_data$Log_Returns[i + 1:(n_ahead)]^2, error = function(e) rep(NA, n_ahead))
      timestamps    <- tryCatch(val_data$Timestamp[i + 1:(n_ahead)], error = function(e) rep(NA, n_ahead))
      
      data.frame(
        time_index = rep(i, n_ahead),
        step       = 1:n_ahead,
        timestamp  = timestamps,
        forecast   = forecast_vars,
        actual     = actual_sqs,
        fit_failed = as.integer(!is.finite(forecast_vars) | !is.finite(actual_sqs))
      )
    }, error = function(e) {
      data.frame(time_index = i, step = NA, timestamp = NA, forecast = NA, actual = NA, fit_failed = 1)
    })
  }
  
  # Compute errors only where valid
  valid_rows <- results$fit_failed == 0
  error      <- results$forecast[valid_rows] - results$actual[valid_rows]
  
  # Compute 95% coverage assuming normality
  z_crit <- qnorm(0.975, 0,1)
  forecast_sd <- sqrt(results$forecast[valid_rows])
  actual_sd   <- sqrt(results$actual[valid_rows])
  
  lower_bound <- forecast_sd * (-z_crit)
  upper_bound <- forecast_sd * z_crit
  
  coverage <- mean(actual_sd >= lower_bound & actual_sd <= upper_bound, na.rm = TRUE)
  
  summary_df <- data.frame(
    freq        = freq_label,
    window      = window_size,
    step_ahead  = n_ahead,
    mse         = mean(error^2, na.rm = TRUE),
    mae         = mean(abs(error), na.rm = TRUE),
    rmse        = sqrt(mean(error^2, na.rm = TRUE)),
    n           = sum(valid_rows),
    failed_fits = sum(results$fit_failed),
    coverage    = coverage
  )
  
  list(summary = summary_df, details = results)
}


# multi-step forecasts for each time frequency to compare against
test_1h_1step <- refit_rolling_forecast_multistep(rbind(train_1H, val_1H), test_1H, window_size = 200, n_ahead = 1, n_starts = 5, freq_label = "1-Hour", align_minutes = "00")
test_30m_1step <- refit_rolling_forecast_multistep(rbind(train_30M, val_30M), test_30M, window_size = 400, n_ahead = 1, n_starts = 5, freq_label = "30-Minute", align_minutes = c("00", "30"))
test_30m_2step <- refit_rolling_forecast_multistep(rbind(train_30M, val_30M), test_30M, window_size = 400, n_ahead = 2, n_starts = 5, freq_label = "30-Minute", align_minutes = "00")
test_15m_1step <- refit_rolling_forecast_multistep(rbind(train_15M, val_15M), test_15M, window_size = 800, n_ahead = 1, n_starts = 5, freq_label = "15-Minute", align_minutes = c("00", "15", "30", "45"))
test_15m_2step <- refit_rolling_forecast_multistep(rbind(train_15M, val_15M), test_15M, window_size = 800, n_ahead = 2, n_starts = 5, freq_label = "15-Minute", align_minutes = c("00", "30"))
test_15m_4step <- refit_rolling_forecast_multistep(rbind(train_15M, val_15M), test_15M, window_size = 800, n_ahead = 4, n_starts = 5, freq_label = "15-Minute", align_minutes = "00")
test_5m_1step  <- refit_rolling_forecast_multistep(rbind(train_5M,  val_5M),  test_5M,  window_size = 2400, n_ahead = 1,  n_starts = 5, freq_label = "5-Minute", align_minutes = as.character(sprintf("%02d", seq(0, 55, by = 5))))
test_5m_3step  <- refit_rolling_forecast_multistep(rbind(train_5M,  val_5M),  test_5M,  window_size = 2400, n_ahead = 3,  n_starts = 5, freq_label = "5-Minute", align_minutes = c("00", "15", "30", "45"))
test_5m_6step  <- refit_rolling_forecast_multistep(rbind(train_5M,  val_5M),  test_5M,  window_size = 2400, n_ahead = 6,  n_starts = 5, freq_label = "5-Minute", align_minutes = c("00", "30"))
test_5m_12step <- refit_rolling_forecast_multistep(rbind(train_5M,  val_5M),  test_5M,  window_size = 2400, n_ahead = 12, n_starts = 5, freq_label = "5-Minute", align_minutes = "00")

#Save outputs
write.csv(test_1h_1step,  "test_1h_1step.csv",  row.names = FALSE)
write.csv(test_30m_1step, "test_30m_1step.csv", row.names = FALSE)
write.csv(test_30m_2step, "test_30m_2step.csv", row.names = FALSE)
write.csv(test_15m_1step, "test_15m_1step.csv", row.names = FALSE)
write.csv(test_15m_2step, "test_15m_2step.csv", row.names = FALSE)
write.csv(test_15m_4step, "test_15m_4step.csv", row.names = FALSE)
write.csv(test_5m_1step,  "test_5m_1step.csv",  row.names = FALSE)
write.csv(test_5m_3step,  "test_5m_3step.csv",  row.names = FALSE)
write.csv(test_5m_6step,  "test_5m_6step.csv",  row.names = FALSE)
write.csv(test_5m_12step, "test_5m_12step.csv", row.names = FALSE)

# Plot Forecast vs Actual Squared Returns for 1-Hour Step
# Filter to a sample time window (e.g., June-July)
zoom_data <- test_1h_1step$details %>%
  filter(timestamp >= as.POSIXct("2024-06-01") & timestamp <= as.POSIXct("2024-07-31"))

ggplot(zoom_data, aes(x = timestamp)) +
  geom_line(aes(y = forecast), color = "steelblue", size = 1) +
  geom_line(aes(y = actual), color = "darkorange", size = 0.7, linetype = "dashed") +
  labs(
    title = "Forecast vs Actual Squared Returns (June July Zoom)",
    x = "Timestamp",
    y = "Variance"
  ) +
  theme_minimal()
