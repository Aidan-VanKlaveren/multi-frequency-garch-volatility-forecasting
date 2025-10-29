# plot_triangle_summaries.R
# ------------------------------------------------------------
# Requires: tidyverse
# install.packages("tidyverse")  # if needed
library(tidyverse)

# 1) locate files
files <- list.files(
  pattern = "^tri_step1_.*_gjr_tdf10_1step_triangle_window200_summary\\.csv$",
  full.names = TRUE
)
if (length(files) == 0) stop("No matching *_triangle_window200_summary.csv files found.")

# 2) helper: find the rolling-window column
find_window_col <- function(nms) {
  win <- nms[grepl("window", tolower(nms))]
  if (length(win) == 0) return(NA_character_)
  win[1]
}

# 3) read & combine
read_one <- function(f) {
  freq <- stringr::str_match(basename(f), "tri_step1_(1m|5m|15m|30m|1h)_")[,2]
  df <- suppressMessages(readr::read_csv(f, show_col_types = FALSE))
  wcol <- find_window_col(names(df))
  if (is.na(wcol)) stop(paste("No 'window*' column found in", f))
  df <- df %>%
    rename(window200 = all_of(wcol)) %>%
    mutate(freq = factor(freq, levels = c("1m","5m","15m","30m","1h")))
  df
}
dat <- purrr::map_dfr(files, read_one)

# 4) metrics to plot (keep only those present)
metrics <- c(
  "MARE_fc_mean",
  "MARE_fc_median",
  "MARE_fc_p95",
  "MARE_tri_vs_real_mean",
  "MARE_fc_vs_real_mean",
  "rho_gap_mean",
  "rho_gap_rmse"
)
metrics <- intersect(metrics, names(dat))

long <- dat %>%
  pivot_longer(all_of(metrics), names_to = "metric", values_to = "value")

# 5) output folder
outdir <- "plots_triangle_summary"
dir.create(outdir, showWarnings = FALSE)

# 6) one plot per metric (colored by frequency)
save_metric_plot <- function(m) {
  df <- filter(long, metric == m) %>% arrange(freq, window200)
  p <- ggplot(df, aes(x = window200, y = value, color = freq)) +
    geom_line(linewidth = 0.7, alpha = 0.95) +
    labs(
      title = paste0(m, " by rolling window (size 200)"),
      x = "Rolling window index",
      y = m,
      color = "Frequency"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  # reference lines
  if (grepl("^MARE", m)) {
    p <- p + geom_hline(yintercept = 1, linetype = "dotted")
  }
  if (grepl("^rho_gap", m)) {
    p <- p + geom_hline(yintercept = 0, linetype = "dotted")
  }
  
  ggsave(file.path(outdir, paste0(m, "_by_window.png")),
         p, width = 9, height = 5, dpi = 300)
}
invisible(purrr::walk(metrics, save_metric_plot))

# 7) one plot per frequency (faceted by metric)
save_freq_panel <- function(fr) {
  df <- filter(long, freq == fr)
  p <- ggplot(df, aes(x = window200, y = value)) +
    geom_line(linewidth = 0.6) +
    facet_wrap(~ metric, scales = "free_y", ncol = 3) +
    labs(
      title = paste0("Frequency = ", fr, " — metrics by rolling window (size 200)"),
      x = "Rolling window index",
      y = NULL
    ) +
    theme_minimal(base_size = 12)
  ggsave(file.path(outdir, paste0("freq_", fr, "_panels.png")),
         p, width = 10, height = 7, dpi = 300)
}
invisible(purrr::walk(levels(dat$freq), save_freq_panel))

message("Saved plots to: ", normalizePath(outdir))

