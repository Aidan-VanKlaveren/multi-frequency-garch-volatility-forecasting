## GARCH Optimised Training Windows ##

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
library(moments)


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
# 5-Minute Returns
cat("5-Minute Log Returns Summary:\n")
summary(train_5M$Log_Returns)
cat("Skewness:", skewness(train_5M$Log_Returns), "\n")
cat("Kurtosis:", kurtosis(train_5M$Log_Returns), "\n\n")

# 15-Minute Returns
cat("15-Minute Log Returns Summary:\n")
summary(train_15M$Log_Returns)
cat("Skewness:", skewness(train_15M$Log_Returns), "\n")
cat("Kurtosis:", kurtosis(train_15M$Log_Returns), "\n\n")

# 30-Minute Returns
cat("30-Minute Log Returns Summary:\n")
summary(train_30M$Log_Returns)
cat("Skewness:", skewness(train_30M$Log_Returns), "\n")
cat("Kurtosis:", kurtosis(train_30M$Log_Returns), "\n\n")

# 1-Hour Returns
cat("1-Hour Log Returns Summary:\n")
summary(train_1H$Log_Returns)
cat("Skewness:", skewness(train_1H$Log_Returns), "\n")
cat("Kurtosis:", kurtosis(train_1H$Log_Returns), "\n")


#Final adjusted model for each frequency and window size
refit_rolling_forecast_parallel <- function(train_data, val_data, window_size, n_starts = 10, freq_label = "") {
  cat(sprintf("===== PARALLEL Rolling Forecast: %s (Window = %d, Multi-start = %d) =====\n",
              freq_label, window_size, n_starts))
  
  all_data <- c(train_data$Log_Returns, val_data$Log_Returns)
  n_train  <- length(train_data$Log_Returns)
  n_val    <- length(val_data$Log_Returns)
  
  results <- foreach(i = 1:n_val, .combine = rbind, .packages = "rugarch") %dorng% {
    window_start <- n_train + i - window_size
    window_end   <- n_train + i - 1
    
    if (!is.finite(window_start) || !is.finite(window_end) || window_start < 1 || window_end > length(all_data)) {
      return(data.frame(forecast = NA, actual = NA, fit_failed = 1))
    }
    
    window_data <- all_data[window_start:window_end]
    
    if (!is.numeric(window_data) || length(window_data) < 5 || sd(window_data, na.rm = TRUE) < .Machine$double.eps^0.5) {
      return(data.frame(forecast = NA, actual = NA, fit_failed = 1))
    }
    
    spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model     = list(armaOrder = c(0, 0), include.mean = FALSE),
      distribution.model = "norm"
    )
    
    best_fit <- NULL
    best_llh <- -Inf
    
    for (j in 1:n_starts) {
      fit_try <- tryCatch({
        ugarchfit(
          spec, data = window_data,
          solver = "gosolnp",
          fit.control = list(stationarity = 1),
          solver.control = list(start = 1, trace = 0)
        )
      }, error = function(e) NULL)
      
      if (!is.null(fit_try)) {
        llh <- tryCatch(likelihood(fit_try), error = function(e) NA_real_)
        if (isTRUE(is.finite(llh)) && llh > best_llh) {
          best_llh <- llh
          best_fit <- fit_try
        }
      }
    }
    
    if (is.null(best_fit)) {
      return(data.frame(forecast = NA, actual = NA, fit_failed = 1))
    }
    
    forecast <- tryCatch({
      ugarchforecast(best_fit, n.ahead = 1)
    }, error = function(e) NULL)
    
    if (is.null(forecast)) {
      return(data.frame(forecast = NA, actual = NA, fit_failed = 1))
    }
    
    forecast_var <- tryCatch(as.numeric(sigma(forecast))^2, error = function(e) NA_real_)
    actual_sq    <- tryCatch(val_data$Log_Returns[i]^2, error = function(e) NA_real_)
    
    if (!is.finite(forecast_var) || !is.finite(actual_sq)) {
      return(data.frame(forecast = NA, actual = NA, fit_failed = 1))
    }
    
    data.frame(forecast = forecast_var, actual = actual_sq, fit_failed = 0)
  }
  
  # Error metrics
  error    <- results$forecast - results$actual
  mse_val  <- mean(error^2, na.rm = TRUE)
  mae_val  <- mean(abs(error), na.rm = TRUE)
  rmse_val <- sqrt(mse_val)
  valid_n  <- sum(!is.na(error))
  failed_n <- sum(results$fit_failed == 1)
  
  # 95% empirical coverage band based on residual spread
  error_sd   <- sd(error, na.rm = TRUE)
  mean_fore  <- mean(results$forecast, na.rm = TRUE)
  delta      <- 1.96 * error_sd / mean_fore  # Empirical 95% CI width
  
  lower_bound <- results$forecast * (1 - delta)
  upper_bound <- results$forecast * (1 + delta)
  
  coverage <- mean(results$actual >= lower_bound & results$actual <= upper_bound, na.rm = TRUE)
  
  return(data.frame(
    freq        = freq_label,
    window      = window_size,
    mse         = mse_val,
    mae         = mae_val,
    rmse        = rmse_val,
    n           = valid_n,
    failed_fits = failed_n,
    coverage    = coverage
  ))
}


#Function to loop through window sizes
run_all_windows <- function(train_data, val_data, window_sizes, freq_label, n_starts = 5) {
  with_progress({
    p <- progressor(steps = length(window_sizes))
    
    results <- lapply(window_sizes, function(w) {
      p(sprintf("Window %d", w))
      refit_rolling_forecast_parallel(
        train_data = train_data,
        val_data   = val_data,
        window_size = w,
        n_starts    = n_starts,
        freq_label  = freq_label
      )
    })
    
    do.call(rbind, results)
  })
}


plan(multisession, workers = parallel::detectCores() - 6)

# Define window size sequences
window_sizes_5m   <- seq(600, 2400, by = 600)
window_sizes_15m  <- seq(200, 1000, by = 200)
window_sizes_30m  <- seq(100, 800, by = 100)
window_sizes_1h   <- seq(50, 600, by = 50)

# Run all
results_5m  <- run_all_windows(train_5M, val_5M, window_sizes_5m,  "5-Minute")
results_15m <- run_all_windows(train_15M, val_15M, window_sizes_15m, "15-Minute")
results_30m <- run_all_windows(train_30M, val_30M, window_sizes_30m, "30-Minute")
results_1h  <- run_all_windows(train_1H,  val_1H,  window_sizes_1h,  "1-Hour")

# Tables
results_5m
results_15m
results_30m
results_1h


#Graphs
# 5 minute
p1_5m <- ggplot(results_5m, aes(x = window, y = mse)) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_point(aes(color = failed_fits > 0), size = 3) +
  scale_color_manual(values = c("black", "red"), labels = c("OK", "Some Failures")) +
  labs(title = "MSE vs Window Size (5-Minute)", x = "Training Window Size", y = "MSE", color = "Fit Status") +
  theme_minimal()

p2_5m <- ggplot(results_5m, aes(x = window, y = rmse)) +
  geom_line(color = "forestgreen", linewidth = 1.1) +
  geom_point(aes(color = failed_fits > 0), size = 3) +
  scale_color_manual(values = c("black", "red"), labels = c("OK", "Some Failures")) +
  labs(title = "RMSE vs Window Size (5-Minute)", x = "Training Window Size", y = "RMSE", color = "Fit Status") +
  theme_minimal()

p3_5m <- ggplot(results_5m, aes(x = window, y = mae)) +
  geom_line(color = "darkorange", linewidth = 1.1) +
  geom_point(aes(color = failed_fits > 0), size = 3) +
  scale_color_manual(values = c("black", "red"), labels = c("OK", "Some Failures")) +
  labs(title = "MAE vs Window Size (5-Minute)", x = "Training Window Size", y = "MAE", color = "Fit Status") +
  theme_minimal()

p1_5m / p2_5m / p3_5m

# 15 minute
p1_15m <- ggplot(results_15m, aes(x = window, y = mse)) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_point(aes(color = failed_fits > 0), size = 3) +
  scale_color_manual(values = c("black", "red"), labels = c("OK", "Some Failures")) +
  labs(title = "MSE vs Window Size (15-Minute)", x = "Training Window Size", y = "MSE", color = "Fit Status") +
  theme_minimal()

p2_15m <- ggplot(results_15m, aes(x = window, y = rmse)) +
  geom_line(color = "forestgreen", linewidth = 1.1) +
  geom_point(aes(color = failed_fits > 0), size = 3) +
  scale_color_manual(values = c("black", "red"), labels = c("OK", "Some Failures")) +
  labs(title = "RMSE vs Window Size (15-Minute)", x = "Training Window Size", y = "RMSE", color = "Fit Status") +
  theme_minimal()

p3_15m <- ggplot(results_15m, aes(x = window, y = mae)) +
  geom_line(color = "darkorange", linewidth = 1.1) +
  geom_point(aes(color = failed_fits > 0), size = 3) +
  scale_color_manual(values = c("black", "red"), labels = c("OK", "Some Failures")) +
  labs(title = "MAE vs Window Size (15-Minute)", x = "Training Window Size", y = "MAE", color = "Fit Status") +
  theme_minimal()

p1_15m / p2_15m / p3_15m

#30 minute
p1_30m <- ggplot(results_30m, aes(x = window, y = mse)) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_point(aes(color = failed_fits > 0), size = 3) +
  scale_color_manual(values = c("black", "red"), labels = c("OK", "Some Failures")) +
  labs(title = "MSE vs Window Size (30-Minute)", x = "Training Window Size", y = "MSE", color = "Fit Status") +
  theme_minimal()

p2_30m <- ggplot(results_30m, aes(x = window, y = rmse)) +
  geom_line(color = "forestgreen", linewidth = 1.1) +
  geom_point(aes(color = failed_fits > 0), size = 3) +
  scale_color_manual(values = c("black", "red"), labels = c("OK", "Some Failures")) +
  labs(title = "RMSE vs Window Size (30-Minute)", x = "Training Window Size", y = "RMSE", color = "Fit Status") +
  theme_minimal()

p3_30m <- ggplot(results_30m, aes(x = window, y = mae)) +
  geom_line(color = "darkorange", linewidth = 1.1) +
  geom_point(aes(color = failed_fits > 0), size = 3) +
  scale_color_manual(values = c("black", "red"), labels = c("OK", "Some Failures")) +
  labs(title = "MAE vs Window Size (30-Minute)", x = "Training Window Size", y = "MAE", color = "Fit Status") +
  theme_minimal()

p1_30m / p2_30m / p3_30m

# 1 hour
p1 <- ggplot(results_1h, aes(x = window, y = mse)) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_point(aes(color = failed_fits > 0), size = 3) +
  scale_color_manual(values = c("black", "red"), labels = c("OK", "Some Failures")) +
  labs(
    title = "MSE vs Window Size (1-Hour)",
    x = "Training Window Size",
    y = "MSE",
    color = "Fit Status"
  ) +
  theme_minimal()

# RMSE Plot
p2 <- ggplot(results_1h, aes(x = window, y = rmse)) +
  geom_line(color = "forestgreen", linewidth = 1.1) +
  geom_point(aes(color = failed_fits > 0), size = 3) +
  scale_color_manual(values = c("black", "red"), labels = c("OK", "Some Failures")) +
  labs(
    title = "RMSE vs Window Size (1-Hour)",
    x = "Training Window Size",
    y = "RMSE",
    color = "Fit Status"
  ) +
  theme_minimal()

# MAE Plot
p3 <- ggplot(results_1h, aes(x = window, y = mae)) +
  geom_line(color = "darkorange", linewidth = 1.1) +
  geom_point(aes(color = failed_fits > 0), size = 3) +
  scale_color_manual(values = c("black", "red"), labels = c("OK", "Some Failures")) +
  labs(
    title = "MAE vs Window Size (1-Hour)",
    x = "Training Window Size",
    y = "MAE",
    color = "Fit Status"
  ) +
  theme_minimal()
p1 / p2 / p3  # patchwork syntax for vertical stacking













## 2x2 data plots
# Add frequency labels to each dataset
train_5M <- train_5M %>% mutate(Set = "Train", Frequency = "5M")
val_5M   <- val_5M   %>% mutate(Set = "Validation", Frequency = "5M")
test_5M  <- test_5M  %>% mutate(Set = "Test", Frequency = "5M")

train_15M <- train_15M %>% mutate(Set = "Train", Frequency = "15M")
val_15M   <- val_15M   %>% mutate(Set = "Validation", Frequency = "15M")
test_15M  <- test_15M  %>% mutate(Set = "Test", Frequency = "15M")

train_30M <- train_30M %>% mutate(Set = "Train", Frequency = "30M")
val_30M   <- val_30M   %>% mutate(Set = "Validation", Frequency = "30M")
test_30M  <- test_30M  %>% mutate(Set = "Test", Frequency = "30M")

train_1H <- train_1H %>% mutate(Set = "Train", Frequency = "1H")
val_1H   <- val_1H   %>% mutate(Set = "Validation", Frequency = "1H")
test_1H  <- test_1H  %>% mutate(Set = "Test", Frequency = "1H")

# Combine all into one dataframe
plot_data_all <- bind_rows(
  train_5M, val_5M, test_5M,
  train_15M, val_15M, test_15M,
  train_30M, val_30M, test_30M,
  train_1H, val_1H, test_1H
)

# Create the faceted plot
ggplot(plot_data_all, aes(x = Timestamp, y = Log_Returns, color = Set)) +
  geom_line(alpha = 0.7, linewidth = 0.4) +
  facet_wrap(~ Frequency, scales = "free_x", ncol = 2) +
  scale_color_manual(values = c("Train" = "#1f77b4", "Validation" = "#2ca02c", "Test" = "#d62728")) +
  labs(
    title = "Train–Validation–Test Splits Across Frequencies",
    x = "Time",
    y = "Log Returns",
    color = "Data Set"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))