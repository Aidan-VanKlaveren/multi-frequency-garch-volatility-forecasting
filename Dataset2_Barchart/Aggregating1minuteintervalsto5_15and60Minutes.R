# ---- Libraries ----
library(tidyverse)
library(lubridate)
library(cli)
library(readr)
library(stringr)

# =========================================
#           DATA LOADING & PREP
# =========================================

## 1) List csvs
csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)

## 2) Extract the 6-letter pair from filename (e.g., "USDEUR_...csv" -> "USDEUR")
get_pair <- function(fname) {
  nm <- toupper(basename(fname))
  sub("^([A-Z]{6}).*$", "\\1", nm)
}

pairs <- purrr::map_chr(csv_files, get_pair)
unique_pairs <- unique(pairs)

## 3) Build one 1-minute table per pair (dedupe by first column = timestamp)
for (p in unique_pairs) {
  pf <- csv_files[pairs == p]
  df <- pf %>%
    purrr::map_dfr(readr::read_csv, show_col_types = FALSE) %>%
    arrange(across(1)) %>%
    distinct(across(1), .keep_all = TRUE)
  assign(paste0(p, "1M"), df, envir = .GlobalEnv)
}
rm(df, pf, p)

# =========================================
#        RETURNS-ONLY AGGREGATION
# =========================================

# Pick timestamp + close/last column (case-insensitive)
.pick_cols_close <- function(df) {
  nms <- names(df); ln <- tolower(nms)
  ts    <- nms[str_detect(ln, "time|stamp|datetime")][1]
  if (is.na(ts)) stop("No timestamp-like column found.")
  close <- nms[str_detect(ln, "^last$|(^|_)(last|close)($|_)")][1]
  if (is.na(close)) stop("No Close/Last column found.")
  list(ts = ts, close = close)
}

# Robust time parser (mdy/ymd/dmy with HM/HMS; UTC)
.as_time <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  suppressWarnings(parse_date_time(
    x,
    orders = c("mdy HM","mdy HMS","ymd HM","ymd HMS","dmy HM","dmy HMS"),
    tz = "UTC", exact = FALSE
  ))
}

# Build minute-level closes and 1m returns for a pair
build_minute_data <- function(df) {
  C <- .pick_cols_close(df)
  # Minute-level closes (last tick in each minute)
  w_min <- df %>%
    transmute(
      minute = floor_date(.as_time(.data[[C$ts]]), "1 minute"),
      close  = suppressWarnings(as.numeric(.data[[C$close]]))
    ) %>%
    filter(!is.na(minute)) %>%
    arrange(minute) %>%
    group_by(minute) %>%
    summarise(close = dplyr::last(close), .groups = "drop") %>%
    arrange(minute)
  
  # 1-minute log returns r_t = log(C_{t+1}/C_t)
  w_ret <- w_min %>%
    mutate(close_lead = dplyr::lead(close),
           r1m       = ifelse(is.na(close) | is.na(close_lead),
                              NA_real_, log(close_lead / close)))
  list(w_min = w_min, w_ret = w_ret)
}

# Aggregate from 1m returns into m-minute bins (sum of r1m)
agg_from_wret <- function(w_ret, pair, minutes = 5L) {
  unit_str <- if (minutes == 1L) "1 minute" else paste0(minutes, " minutes")
  w_ret %>%
    mutate(
      bin_start = floor_date(minute, unit_str),
      bin_end   = bin_start + dminutes(minutes),
      bin       = bin_start
    ) %>%
    group_by(bin_start, bin_end, bin) %>%
    summarise(
      pair        = pair,
      n_1m        = sum(!is.na(r1m)),         # number of 1m returns summed (0/1 for 1M)
      expected_n  = minutes,                  # 1 / 5 / 15 / 30 / 60
      log_return  = sum(r1m, na.rm = FALSE),  # NA if any r1m is NA
      .groups     = "drop"
    ) %>%
    select(pair, bin_start, bin_end, bin, n_1m, expected_n, log_return)
}

# =========================================
#        RUN FOR ALL *1M* DATA FRAMES
# =========================================
obj_names <- ls(envir = .GlobalEnv)
pair_dfs  <- obj_names[str_detect(obj_names, "^[A-Z]{6}1M$")]

if (length(pair_dfs) == 0) {
  cli::cli_alert_warning("No in-memory data frames like 'USDEUR1M' found.")
} else {
  dir.create("agg_out", showWarnings = FALSE)
  
  for (nm in pair_dfs) {
    pair <- sub("1M$", "", nm)
    df   <- get(nm, envir = .GlobalEnv)
    
    # Build minute data once
    md <- build_minute_data(df)
    
    # Aggregate returns to 1/5/15/30/60 minutes (uniform schema)
    agg_1   <- agg_from_wret(md$w_ret, pair,  1L)
    agg_5   <- agg_from_wret(md$w_ret, pair,  5L)
    agg_15  <- agg_from_wret(md$w_ret, pair, 15L)
    agg_30  <- agg_from_wret(md$w_ret, pair, 30L)
    agg_60  <- agg_from_wret(md$w_ret, pair, 60L)
    
    # Write files (overwrites if exist)
    write_csv(agg_1,  file.path("agg_out", paste0(pair, "1M.csv")))
    write_csv(agg_5,  file.path("agg_out", paste0(pair, "5M.csv")))
    write_csv(agg_15, file.path("agg_out", paste0(pair, "15M.csv")))
    write_csv(agg_30, file.path("agg_out", paste0(pair, "30M.csv")))
    write_csv(agg_60, file.path("agg_out", paste0(pair, "60M.csv")))
    
    cli::cli_alert_success(
      "{pair}: wrote {nrow(agg_1)} (1m), {nrow(agg_5)} (5m), {nrow(agg_15)} (15m), {nrow(agg_30)} (30m), {nrow(agg_60)} (60m)."
    )
  }
}

# (Later for modeling) keep complete bins:
# d1  <- readr::read_csv("agg_out/USDEUR1M.csv",  show_col_types = FALSE) |> dplyr::filter(n_1m == expected_n)  # n_1m = 1
# d5  <- readr::read_csv("agg_out/USDEUR5M.csv",  show_col_types = FALSE) |> dplyr::filter(n_1m == expected_n)
# d15 <- readr::read_csv("agg_out/USDEUR15M.csv", show_col_types = FALSE) |> dplyr::filter(n_1m == expected_n)
# d30 <- readr::read_csv("agg_out/USDEUR30M.csv", show_col_types = FALSE) |> dplyr::filter(n_1m == expected_n)
# d60 <- readr::read_csv("agg_out/USDEUR60M.csv", show_col_types = FALSE) |> dplyr::filter(n_1m == expected_n)
