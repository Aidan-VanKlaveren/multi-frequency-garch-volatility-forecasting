# triangle_mare.R
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
})

# -------- helpers -------------------------------------------------------------

# find a column by loose regex (case-insensitive), error if missing
find_col <- function(df, pattern) {
  hits <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
  if (length(hits) == 0) stop(sprintf("Column matching /%s/ not found.", pattern))
  hits[1]
}

# safe sqrt on non-negative
ssqrt <- function(x) sqrt(pmax(x, 0))

# compute metrics on one data frame
compute_triangle_metrics <- function(df_raw, freq_tag) {
  
  # map columns (robust to slight name differences)
  col_time   <- find_col(df_raw, "timestamp|time_index")
  col_fx_x_f <- find_col(df_raw, "forecast[_\\.]*gbp.?eur")
  col_fx_x_a <- find_col(df_raw, "actual[_\\.]*gbp.?eur")
  
  col_fx_y_f <- find_col(df_raw, "forecast[_\\.]*usd.?eur")
  col_fx_y_a <- find_col(df_raw, "actual[_\\.]*usd.?eur")
  
  col_fx_z_f <- find_col(df_raw, "forecast[_\\.]*usd.?gbp")
  col_fx_z_a <- find_col(df_raw, "actual[_\\.]*usd.?gbp")
  
  col_rho_f  <- find_col(df_raw, "rho[_\\.]*usd.?eur[_\\.]*usd.?gbp|rho")
  
  # optional fit_failed flags if present
  fit_gb_col <- grep("fit[_\\.]*failed[_\\.]*gbp.?eur", names(df_raw), ignore.case = TRUE, value = TRUE)
  fit_usd_e  <- grep("fit[_\\.]*failed[_\\.]*usd.?eur", names(df_raw), ignore.case = TRUE, value = TRUE)
  fit_usd_g  <- grep("fit[_\\.]*failed[_\\.]*usd.?gbp", names(df_raw), ignore.case = TRUE, value = TRUE)
  
  df <- df_raw %>%
    transmute(
      freq = freq_tag,
      tstamp = .data[[col_time]],
      h_x_f = as.numeric(.data[[col_fx_x_f]]),   # GBPEUR forecast
      h_x_a = as.numeric(.data[[col_fx_x_a]]),   # GBPEUR realised
      h_y_f = as.numeric(.data[[col_fx_y_f]]),   # USDEUR forecast
      h_y_a = as.numeric(.data[[col_fx_y_a]]),   # USDEUR realised
      h_z_f = as.numeric(.data[[col_fx_z_f]]),   # USDGBP forecast
      h_z_a = as.numeric(.data[[col_fx_z_a]]),   # USDGBP realised
      rho_fit = as.numeric(.data[[col_rho_f]]),
      fit_failed_x = if (length(fit_gb_col)) as.integer(.data[[fit_gb_col[1]]]) else 0L,
      fit_failed_y = if (length(fit_usd_e))  as.integer(.data[[fit_usd_e[1]]])  else 0L,
      fit_failed_z = if (length(fit_usd_g))  as.integer(.data[[fit_usd_g[1]]])  else 0L
    ) %>%
    filter((fit_failed_x + fit_failed_y + fit_failed_z) == 0)
  
  # --- triangle calculations (correct sign & correlation formula) --------------
  df <- df %>%
    mutate(
      # implied GBPEUR variance from y,z and fitted rho (minus sign)
      h_x_tri = h_y_f + h_z_f - 2 * rho_fit * ssqrt(h_y_f * h_z_f),
      
      # PRIMARY triangle-consistency metric: implied vs direct forecast
      MARE_fc  = ifelse(h_x_f > 0, abs(h_x_tri - h_x_f) / h_x_f, NA_real_),
      
      # OPTIONAL diagnostics
      MARE_tri_vs_real = ifelse(h_x_a > 0, abs(h_x_tri - h_x_a) / h_x_a, NA_real_),
      MARE_fc_vs_real  = ifelse(h_x_a > 0, abs(h_x_f   - h_x_a) / h_x_a, NA_real_),
      
      # variance-based implied correlation (correct y,z,x order)
      rho_var = (h_y_f + h_z_f - h_x_f) / (2 * ssqrt(h_y_f) * ssqrt(h_z_f)),
      rho_var = pmax(-1, pmin(1, rho_var)),
      d_rho   = rho_var - rho_fit
    )
  
  # add rolling window id of length 200 (1-based)
  df <- df %>%
    mutate(row_id = row_number(),
           window200 = (row_id - 1) %/% 200 + 1L)
  
  # per-window summaries
  per_win <- df %>%
    group_by(freq, window200) %>%
    summarise(
      n = n(),
      MARE_fc_mean   = mean(MARE_fc, na.rm = TRUE),
      MARE_fc_median = median(MARE_fc, na.rm = TRUE),
      MARE_fc_p95    = quantile(MARE_fc, 0.95, na.rm = TRUE),
      MARE_tri_vs_real_mean = mean(MARE_tri_vs_real, na.rm = TRUE),
      MARE_fc_vs_real_mean  = mean(MARE_fc_vs_real,  na.rm = TRUE),
      rho_gap_mean = mean(d_rho, na.rm = TRUE),
      rho_gap_rmse = sqrt(mean(d_rho^2, na.rm = TRUE)),
      .groups = "drop"
    )
  
  list(rows = df, win = per_win)
}

# -------- main ---------------------------------------------------------------

# pick non-summary files from current working directory
files <- list.files(pattern = "^tri_step1_.*_gjr_tdf10_1step\\.csv$", full.names = TRUE)
files <- files[!grepl("summary", files, ignore.case = TRUE)]
if (length(files) == 0) stop("No matching tri_step1_* files found in working directory.")

message(sprintf("Found %d files in current directory.", length(files)))

# extract frequency tag
extract_freq <- function(path) {
  m <- str_match(basename(path), "tri_step1_(1m|5m|15m|30m|1h)_")[,2]
  ifelse(is.na(m), "NA", m)
}

results <- map(files, function(f) {
  freq <- extract_freq(f)
  message("Processing: ", basename(f), "  [freq=", freq, "]")
  df_raw <- suppressMessages(read_csv(f, show_col_types = FALSE))
  out <- compute_triangle_metrics(df_raw, freq_tag = freq)
  
  # write outputs directly into current working directory
  per_row_path <- file.path(getwd(), str_replace(basename(f), "\\.csv$", "_triangle_rows.csv"))
  per_win_path <- file.path(getwd(), str_replace(basename(f), "\\.csv$", "_triangle_window200_summary.csv"))
  
  write_csv(out$rows, per_row_path)
  write_csv(out$win,  per_win_path)
  
  list(freq = freq, rows = out$rows, win = out$win,
       per_row_path = per_row_path, per_win_path = per_win_path)
})

# combine summaries across all files
all_rows <- bind_rows(map(results, "rows"))
all_win  <- bind_rows(map(results, "win"))

write_csv(all_rows, file.path(getwd(), "ALL_triangle_rows_combined.csv"))
write_csv(all_win,  file.path(getwd(), "ALL_triangle_window200_summary.csv"))

# console summary
overall <- all_rows %>%
  group_by(freq) %>%
  summarise(
    N = n(),
    MARE_fc_mean   = mean(MARE_fc, na.rm = TRUE),
    MARE_fc_median = median(MARE_fc, na.rm = TRUE),
    MARE_fc_p95    = quantile(MARE_fc, 0.95, na.rm = TRUE),
    MARE_tri_vs_real_mean = mean(MARE_tri_vs_real, na.rm = TRUE),
    MARE_fc_vs_real_mean  = mean(MARE_fc_vs_real,  na.rm = TRUE),
    rho_gap_mean = mean(d_rho, na.rm = TRUE),
    rho_gap_rmse = sqrt(mean(d_rho^2, na.rm = TRUE)),
    .groups = "drop"
  )

print(overall)
message("Done. All outputs saved in the current working directory.")




