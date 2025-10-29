library(readr)
library(dplyr)
library(rugarch)

# ---- Load & make log returns from 'Last' ----
file_path <- "USDEUR_202507211600-202508071559.csv"

raw <- read_csv(file_path, show_col_types = FALSE)

dat <- raw %>%
  transmute(
    Timestamp  = as.POSIXct(Time, format = "%m/%d/%Y %H:%M", tz = "UTC"),
    Last       = as.numeric(Last)
  ) %>%
  arrange(Timestamp) %>%
  distinct(Timestamp, .keep_all = TRUE) %>%
  mutate(
    log_return = c(NA_real_, diff(log(Last)))
  ) %>%
  filter(is.finite(log_return))

stopifnot(nrow(dat) > 300)   # sanity

x <- dat$log_return  # series to fit

# ---- GJR-GARCH specs ----
spec_norm <- ugarchspec(
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model         = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "norm"
)

spec_t_free <- ugarchspec(
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model         = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std"   # Student-t with df (shape) estimated
)

spec_t_df4 <- ugarchspec(
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model         = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std",
  fixed.pars         = list(shape = 4)     # df fixed at 4
)

# ---- Fit (joint MLE) ----
set.seed(1)
fit_norm   <- ugarchfit(spec_norm,   data = x, solver = "gosolnp",
                        fit.control = list(stationarity = 1),
                        solver.control = list(trace = 0))

fit_t_free <- ugarchfit(spec_t_free, data = x, solver = "gosolnp",
                        fit.control = list(stationarity = 1),
                        solver.control = list(trace = 0))

fit_t_df4  <- ugarchfit(spec_t_df4,  data = x, solver = "gosolnp",
                        fit.control = list(stationarity = 1),
                        solver.control = list(trace = 0))

# ---- Quick checks ----
cat("\n=== Coeff names (norm) ===\n");       print(names(coef(fit_norm)))
cat("\n=== Coeff names (t free) should include 'shape' ===\n"); print(names(coef(fit_t_free)))
cat("\nEstimated df (shape) in t free:\n");  print(coef(fit_t_free)["shape"])

cat("\n=== LogLik / AIC / BIC ===\n")
tab_ic <- rbind(
  norm   = c(LogLik = likelihood(fit_norm),   t(infocriteria(fit_norm))),
  t_free = c(LogLik = likelihood(fit_t_free), t(infocriteria(fit_t_free))),
  t_df4  = c(LogLik = likelihood(fit_t_df4),  t(infocriteria(fit_t_df4)))
)
print(round(tab_ic, 4))

cat("\n=== Convergence details ===\n")
print(list(
  norm   = fit_norm@fit$details[c("convergence","message","iter","evaluations")],
  t_free = fit_t_free@fit$details[c("convergence","message","iter","evaluations")],
  t_df4  = fit_t_df4@fit$details[c("convergence","message","iter","evaluations")]
))

cat("\n=== Robust SE table (t free) ===\n"); print(fit_t_free@fit$matcoef)

# ---- Tiny forecast sanity check (next 5 sigmas) ----
fc_t <- ugarchforecast(fit_t_free, n.ahead = 5)
cat("\nNext 5 sigma^2 (t free):\n"); print(as.numeric(sigma(fc_t))^2)

# ---- Fit (joint MLE) ----
set.seed(2)
fit_norm   <- ugarchfit(spec_norm,   data = x, solver = "gosolnp",
                        fit.control = list(stationarity = 1),
                        solver.control = list(trace = 0))

fit_t_free <- ugarchfit(spec_t_free, data = x, solver = "gosolnp",
                        fit.control = list(stationarity = 1),
                        solver.control = list(trace = 0))

fit_t_df4  <- ugarchfit(spec_t_df4,  data = x, solver = "gosolnp",
                        fit.control = list(stationarity = 1),
                        solver.control = list(trace = 0))

# ---- Quick checks ----
cat("\n=== Coeff names (norm) ===\n");       print(names(coef(fit_norm)))
cat("\n=== Coeff names (t free) should include 'shape' ===\n"); print(names(coef(fit_t_free)))
cat("\nEstimated df (shape) in t free:\n");  print(coef(fit_t_free)["shape"])

cat("\n=== LogLik / AIC / BIC ===\n")
tab_ic <- rbind(
  norm   = c(LogLik = likelihood(fit_norm),   t(infocriteria(fit_norm))),
  t_free = c(LogLik = likelihood(fit_t_free), t(infocriteria(fit_t_free))),
  t_df4  = c(LogLik = likelihood(fit_t_df4),  t(infocriteria(fit_t_df4)))
)
print(round(tab_ic, 4))

cat("\n=== Convergence details ===\n")
print(list(
  norm   = fit_norm@fit$details[c("convergence","message","iter","evaluations")],
  t_free = fit_t_free@fit$details[c("convergence","message","iter","evaluations")],
  t_df4  = fit_t_df4@fit$details[c("convergence","message","iter","evaluations")]
))

cat("\n=== Robust SE table (t free) ===\n"); print(fit_t_free@fit$matcoef)

cat("\n=== Robust SE table (t df 4) ===\n"); print(fit_t_df4@fit$matcoef)

cat("\n=== Robust SE table (norm) ===\n"); print(fit_norm@fit$matcoef)

# ---- Tiny forecast sanity check (next 5 sigmas) ----
fc_t <- ugarchforecast(fit_t_free, n.ahead = 5)
cat("\nNext 5 sigma^2 (t free):\n"); print(as.numeric(sigma(fc_t))^2)
