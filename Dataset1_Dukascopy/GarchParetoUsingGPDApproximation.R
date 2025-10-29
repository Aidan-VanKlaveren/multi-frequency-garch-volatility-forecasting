## GARCH Pareto Distribution ##

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
split_arima_data_by_date <- function(data, column_name, test_start_date) {
  # Check if the specified column exists in the dataset
  if (!(column_name %in% colnames(data))) {
    stop(paste("Error:", column_name, "not found in dataset."))
  }
  
  # Convert the test start date to POSIXct format
  test_start_date <- as.POSIXct(test_start_date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  
  # Ensure the dataset is sorted by the specified time column
  data <- data[order(data[[column_name]]), ]
  
  # Find the index where the test set should start
  test_start_index <- which(data[[column_name]] >= test_start_date)[1]
  
  if (is.na(test_start_index)) {
    stop("Error: Specified test_start_date is out of range.")
  }
  
  # Split the data into training and testing sets
  train_data <- data[1:(test_start_index - 1), ]
  test_data  <- data[test_start_index:nrow(data), ]
  
  return(list(train = train_data, test = test_data))
}

# Set the fixed test start date to "2024-06-01 00:00:00"
fixed_test_start_date <- "2024-06-01 00:00:00"

# Apply the split function to each dataset using "Timestamp" as the key column
split_5M  <- split_arima_data_by_date(Combined5M, "Timestamp", fixed_test_start_date)
split_15M <- split_arima_data_by_date(Combined15M, "Timestamp", fixed_test_start_date)
split_30M <- split_arima_data_by_date(Combined30M, "Timestamp", fixed_test_start_date)
split_1H  <- split_arima_data_by_date(Combined1H, "Timestamp", fixed_test_start_date)

# Extract train & test sets for each time interval
train_5M  <- split_5M$train
test_5M   <- split_5M$test

train_15M <- split_15M$train
test_15M  <- split_15M$test

train_30M <- split_30M$train
test_30M  <- split_30M$test

train_1H  <- split_1H$train
test_1H   <- split_1H$test

# Display summaries for the training sets
summary(train_5M$Log_Returns)
summary(train_15M$Log_Returns)
summary(train_30M$Log_Returns)
summary(train_1H$Log_Returns)


## Normalisation and Scaling on Training Sets ##

# Helper function to open a new plot window
new_plot <- function() {
  if (.Platform$OS.type == "windows") {
    windows()
  } else if (Sys.info()["sysname"] == "Darwin") {
    quartz()
  } else {
    x11()
  }
}

# ------------------------------------------------------------------------------
# Function to Plot the Mean Excess Function e(u) for Both Tails
# ------------------------------------------------------------------------------
plot_mean_excess_function_both <- function(residuals, 
                                           upper_range = c(0.85, 0.99),
                                           lower_range = c(0.01, 0.15),
                                           n_points = 50) {
  # --- Upper tail diagnostic (as before) ---
  # Determine the range of upper threshold values based on quantiles
  upper_lower <- quantile(residuals, upper_range[1], na.rm = TRUE)
  upper_upper <- quantile(residuals, upper_range[2], na.rm = TRUE)
  u_values_upper <- seq(upper_lower, upper_upper, length.out = n_points)
  
  # Compute mean excess for upper tail: e(u) = mean(x - u) for x > u.
  mean_excess_upper <- sapply(u_values_upper, function(u) {
    excesses <- residuals[residuals > u] - u
    if (length(excesses) > 0) {
      return(mean(excesses, na.rm = TRUE))
    } else {
      return(NA)
    }
  })
  df_upper <- data.frame(Threshold = u_values_upper, MeanExcess = mean_excess_upper)
  p_upper <- ggplot(df_upper, aes(x = Threshold, y = MeanExcess)) +
    geom_point(color = "blue") +
    geom_line(color = "red") +
    labs(title = "Upper Tail Mean Excess",
         subtitle = "e(u) = E[X - u | X > u]",
         x = "Threshold u", y = "Mean Excess e(u)") +
    theme_minimal()
  
  # --- Lower tail diagnostic ---
  # For the lower tail, we define thresholds from the low end.
  lower_lower <- quantile(residuals, lower_range[1], na.rm = TRUE)
  lower_upper <- quantile(residuals, lower_range[2], na.rm = TRUE)
  u_values_lower <- seq(lower_lower, lower_upper, length.out = n_points)
  
  # Compute mean excess for lower tail: e(u) = mean(u - x) for x < u.
  mean_excess_lower <- sapply(u_values_lower, function(u) {
    excesses <- u - residuals[residuals < u]
    if (length(excesses) > 0) {
      return(mean(excesses, na.rm = TRUE))
    } else {
      return(NA)
    }
  })
  df_lower <- data.frame(Threshold = u_values_lower, MeanExcess = mean_excess_lower)
  p_lower <- ggplot(df_lower, aes(x = Threshold, y = MeanExcess)) +
    geom_point(color = "blue") +
    geom_line(color = "red") +
    labs(title = "Lower Tail Mean Excess",
         subtitle = "e(u) = E[u - X | X < u]",
         x = "Threshold u", y = "Mean Excess e(u)") +
    theme_minimal()
  
  # Arrange plots side-by-side
  new_plot()
  grid.arrange(p_upper, p_lower, ncol = 2)
}

# ------------------------------------------------------------------------------
# Function to Estimate Tail Quantiles using EVT for Both Tails
# ------------------------------------------------------------------------------
# Inputs:
#   train_data: data frame containing the training set with a column 'Log_Returns'
#   q_lower: quantile for determining the lower tail threshold (e.g., 0.05 for the 5th percentile)
#   q_upper: quantile for determining the upper tail threshold (e.g., 0.95 for the 95th percentile)
#   tail_prob_lower: desired tail probability for lower tail quantile estimation (e.g., 0.01 for 1% loss quantile)
#   tail_prob_upper: desired tail probability for upper tail quantile estimation (e.g., 0.01 for 99th percentile)
#   label: label for printing and plotting
#   excess_diagnostic: logical; if TRUE, plot the mean excess function for both tails.
#
# Procedure:
#   1. Fit an AR(1)-GARCH(1,1) model with Student's t innovations to obtain standardized residuals.
#   2. Produce a QQ-plot of residuals versus the normal distribution.
#   3. Optionally, plot the mean excess function for both tails as a diagnostic (pop-out image).
#   4. Set thresholds as the q_lower and q_upper quantiles.
#   5. Compute excesses for each tail:
#         lower tail: excess = u_lower - x for x < u_lower;
#         upper tail: excess = x - u_upper for x > u_upper.
#   6. Fit a GPD separately to the lower and upper tail excesses.
#   7. Invert the tail estimators to obtain quantile estimates.
# ------------------------------------------------------------------------------
estimate_tail_quantiles_evt_both <- function(train_data, 
                                             q_lower = 0.05, 
                                             q_upper = 0.95, 
                                             tail_prob_lower = 0.01, 
                                             tail_prob_upper = 0.01, 
                                             label = "Train",
                                             excess_diagnostic = TRUE) {
  # (A) Fit AR(1)-GARCH(1,1) with Student's t innovations using sGARCH
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(1, 0), include.mean = TRUE),
    distribution.model = "std"  # Student's t distribution
  )
  
  fit <- ugarchfit(spec = spec, data = train_data$Log_Returns, solver = "hybrid", silent = TRUE)
  std_res <- residuals(fit, standardize = TRUE)
  res_values <- as.numeric(std_res)
  n <- length(res_values)
  
  cat("\n====================================\n")
  cat("GARCH (AR(1)) Fit with Student's t for", label, "(Training Set)\n")
  print(fit)
  cat("====================================\n")
  
  # (B) Diagnostic: QQ-Plot of residuals vs. normal distribution
  new_plot()
  qqnorm(res_values, main = paste("Normal QQ Plot -", label, "(Train)"))
  qqline(res_values, col = "red")
  
  # (C) Optional Diagnostic: Plot Mean Excess Function for both tails
  if (excess_diagnostic) {
    # The upper tail uses the default range (e.g., from 85th to 99th percentile)
    # The lower tail uses a low-range (e.g., from 1st to 15th percentile)
    plot_mean_excess_function_both(res_values)
  }
  
  # (D) Set thresholds for each tail
  u_lower <- quantile(res_values, q_lower)  # lower tail threshold (should be low/negative)
  u_upper <- quantile(res_values, q_upper)  # upper tail threshold (should be high)
  cat("\nLower tail threshold (", q_lower * 100, "th percentile):", u_lower, "\n")
  cat("Upper tail threshold (", q_upper * 100, "th percentile):", u_upper, "\n")
  
  # (E) Compute excesses for each tail
  # Lower tail: excess = u_lower - x for x < u_lower
  lower_tail_indices <- which(res_values < u_lower)
  excess_lower <- u_lower - res_values[lower_tail_indices]
  k_lower <- length(excess_lower)
  if (k_lower == 0) stop("No residuals in lower tail exceed the threshold.")
  
  # Upper tail: excess = x - u_upper for x > u_upper
  upper_tail_indices <- which(res_values > u_upper)
  excess_upper <- res_values[upper_tail_indices] - u_upper
  k_upper <- length(excess_upper)
  if (k_upper == 0) stop("No residuals in upper tail exceed the threshold.")
  
  cat("\nNumber of observations in lower tail:", k_lower, "\n")
  cat("Number of observations in upper tail:", k_upper, "\n")
  
  # (F) Fit a GPD to each set of excesses
  gpd_fit_lower <- fevd(excess_lower, threshold = 0, type = "GP", method = "MLE")
  xi_lower <- gpd_fit_lower$results$par["shape"]
  beta_lower <- gpd_fit_lower$results$par["scale"]
  cat("\nEstimated GPD parameters for lower tail:\n")
  cat("Shape (xi_lower) =", xi_lower, "\n")
  cat("Scale (beta_lower) =", beta_lower, "\n")
  
  gpd_fit_upper <- fevd(excess_upper, threshold = 0, type = "GP", method = "MLE")
  xi_upper <- gpd_fit_upper$results$par["shape"]
  beta_upper <- gpd_fit_upper$results$par["scale"]
  cat("\nEstimated GPD parameters for upper tail:\n")
  cat("Shape (xi_upper) =", xi_upper, "\n")
  cat("Scale (beta_upper) =", beta_upper, "\n")
  
  # (G) Plot diagnostic plots for each tail's GPD fit
  new_plot()
  plot(gpd_fit_lower, main = paste("GPD Fit Diagnostics - Lower Tail -", label))
  
  new_plot()
  plot(gpd_fit_upper, main = paste("GPD Fit Diagnostics - Upper Tail -", label))
  
  # (H) Invert the tail estimators to obtain quantile estimates
  quantile_estimator_upper <- function(p) {
    return(u_upper + beta_upper * (((n / k_upper) * p) ^ (-xi_upper)))
  }
  est_quantile_upper <- quantile_estimator_upper(tail_prob_upper)
  
  quantile_estimator_lower <- function(p) {
    return(u_lower - beta_lower * (((n / k_lower) * p) ^ (-xi_lower)))
  }
  est_quantile_lower <- quantile_estimator_lower(tail_prob_lower)
  
  cat("\nEstimated upper tail quantile for tail probability", tail_prob_upper, ":", est_quantile_upper, "\n")
  cat("Estimated lower tail quantile for tail probability", tail_prob_lower, ":", est_quantile_lower, "\n")
  
  return(list(
    garch_fit = fit,
    std_residuals = res_values,
    lower_threshold = u_lower,
    upper_threshold = u_upper,
    lower_excesses = excess_lower,
    upper_excesses = excess_upper,
    gpd_fit_lower = gpd_fit_lower,
    gpd_fit_upper = gpd_fit_upper,
    xi_lower = xi_lower,
    beta_lower = beta_lower,
    xi_upper = xi_upper,
    beta_upper = beta_upper,
    quantile_estimator_lower = quantile_estimator_lower,
    quantile_estimator_upper = quantile_estimator_upper,
    est_lower_quantile = est_quantile_lower,
    est_upper_quantile = est_quantile_upper
  ))
}



# ------------------------------------------------------------------------------
# Example calls for each time interval (adjust dataset names as needed)
# ------------------------------------------------------------------------------
model_tail_5M_both <- estimate_tail_quantiles_evt_both(train_5M, q_lower = 0.06, q_upper = 0.94,
                                                       tail_prob_lower = 0.01, tail_prob_upper = 0.01,
                                                       label = "5 Minute")

model_tail_15M_both <- estimate_tail_quantiles_evt_both(train_15M, q_lower = 0.05, q_upper = 0.96,
                                                        tail_prob_lower = 0.01, tail_prob_upper = 0.01,
                                                        label = "15 Minute")

model_tail_30M_both <- estimate_tail_quantiles_evt_both(train_30M, q_lower = 0.045, q_upper = 0.93,
                                                        tail_prob_lower = 0.01, tail_prob_upper = 0.01,
                                                        label = "30 Minute")

model_tail_1H_both  <- estimate_tail_quantiles_evt_both(train_1H,  q_lower = 0.05, q_upper = 0.925,
                                                        tail_prob_lower = 0.01, tail_prob_upper = 0.01,
                                                        label = "1 Hour")


## Stability plots for tails ##
# Define the function for stability plots
plot_tail_stability <- function(train_data, returns_column = "Log_Returns",
                                upper_range = seq(0.90, 0.99, by = 0.01),
                                lower_range = seq(0.01, 0.10, by = 0.01)) {
  
  # (A) Fit the AR(1)-GARCH(1,1) model with Student's t innovations
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(1, 0), include.mean = TRUE),
    distribution.model = "std"
  )
  fit <- ugarchfit(spec = spec, data = train_data[[returns_column]], 
                   solver = "hybrid", silent = TRUE)
  res_values <- as.numeric(residuals(fit, standardize = TRUE))
  n <- length(res_values)
  
  # (B) Upper Tail Stability: Loop over candidate upper quantile thresholds
  xi_upper_est <- numeric(length(upper_range))
  beta_upper_est <- numeric(length(upper_range))
  k_upper <- numeric(length(upper_range))
  
  for (i in seq_along(upper_range)) {
    u_candidate <- quantile(res_values, upper_range[i])
    excess_upper <- res_values[res_values > u_candidate] - u_candidate
    k_upper[i] <- length(excess_upper)
    if (length(excess_upper) > 10) {  # Ensure enough exceedances for a reliable GPD fit
      gpd_fit <- fevd(excess_upper, threshold = 0, type = "GP", method = "MLE")
      xi_upper_est[i] <- gpd_fit$results$par["shape"]
      beta_upper_est[i] <- gpd_fit$results$par["scale"]
    } else {
      xi_upper_est[i] <- NA
      beta_upper_est[i] <- NA
    }
  }
  
  df_upper <- data.frame(
    ThresholdQuantile = upper_range,
    xi = xi_upper_est,
    beta = beta_upper_est,
    Exceedances = k_upper
  )
  
  p_upper_xi <- ggplot(df_upper, aes(x = ThresholdQuantile, y = xi)) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    labs(title = "Upper Tail Stability Plot for ξ",
         x = "Upper Threshold Quantile",
         y = "Estimated ξ")
  
  p_upper_beta <- ggplot(df_upper, aes(x = ThresholdQuantile, y = beta)) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    labs(title = "Upper Tail Stability Plot for β",
         x = "Upper Threshold Quantile",
         y = "Estimated β")
  
  # (C) Lower Tail Stability: Loop over candidate lower quantile thresholds
  xi_lower_est <- numeric(length(lower_range))
  beta_lower_est <- numeric(length(lower_range))
  k_lower <- numeric(length(lower_range))
  
  for (i in seq_along(lower_range)) {
    u_candidate <- quantile(res_values, lower_range[i])
    excess_lower <- u_candidate - res_values[res_values < u_candidate]
    k_lower[i] <- length(excess_lower)
    if (length(excess_lower) > 10) {
      gpd_fit <- fevd(excess_lower, threshold = 0, type = "GP", method = "MLE")
      xi_lower_est[i] <- gpd_fit$results$par["shape"]
      beta_lower_est[i] <- gpd_fit$results$par["scale"]
    } else {
      xi_lower_est[i] <- NA
      beta_lower_est[i] <- NA
    }
  }
  
  df_lower <- data.frame(
    ThresholdQuantile = lower_range,
    xi = xi_lower_est,
    beta = beta_lower_est,
    Exceedances = k_lower
  )
  
  p_lower_xi <- ggplot(df_lower, aes(x = ThresholdQuantile, y = xi)) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    labs(title = "Lower Tail Stability Plot for ξ",
         x = "Lower Threshold Quantile",
         y = "Estimated ξ")
  
  p_lower_beta <- ggplot(df_lower, aes(x = ThresholdQuantile, y = beta)) +
    geom_line(na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    labs(title = "Lower Tail Stability Plot for β",
         x = "Lower Threshold Quantile",
         y = "Estimated β")
  
  # Return results and plots
  return(list(
    garch_fit = fit,
    res_values = res_values,
    df_upper = df_upper,
    df_lower = df_lower,
    plots = list(upper_xi = p_upper_xi, upper_beta = p_upper_beta,
                 lower_xi = p_lower_xi, lower_beta = p_lower_beta)
  ))
}

# Now, apply the function for each of your time frequencies
# Helper function to arrange stability plots in a 2x2 grid
plot_stability_grid <- function(result) {
  grid.arrange(result$plots$upper_xi, result$plots$upper_beta,
               result$plots$lower_xi, result$plots$lower_beta,
               nrow = 2, ncol = 2)
}
# For 5-Minute Data:
result_5M <- plot_tail_stability(train_5M, returns_column = "Log_Returns")
plot_stability_grid(result_5M)

# For 15-Minute Data:
result_15M <- plot_tail_stability(train_15M, returns_column = "Log_Returns")
plot_stability_grid(result_15M)

# For 30-Minute Data:
result_30M <- plot_tail_stability(train_30M, returns_column = "Log_Returns")
plot_stability_grid(result_30M)

# For 1-Hour Data:
result_1H <- plot_tail_stability(train_1H, returns_column = "Log_Returns")
plot_stability_grid(result_1H)










