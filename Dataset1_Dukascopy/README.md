# Dataset 1 — GBPAUD (Multi-Frequency GARCH)

R scripts and results for the **rolling-window GJR-GARCH forecasting** and **Extreme Value Theory tail modelling** applied to the **GBPAUD foreign-exchange pair** across multiple high-frequency sampling intervals.  
This dataset underpins **Tables 5.11–5.12** and associated figures in the thesis.

---

## 📁 Overview

| Script | Purpose |
|:-------|:---------|
| `01_garch_optimised_training_windows.R` | Tunes GARCH(1,1) rolling-window sizes via multi-start optimization and parallel refitting across 5 M, 15 M, 30 M, and 1 H data. Produces MSE/MAE/RMSE vs window plots to select optimal training horizons. |
| `02_garch_optimal_testset_forecasting.R` | Re-runs forecasts on the fixed test set using the optimal windows from validation. Generates **non-overlapping, multi-step** sGARCH(1,1) (Gaussian) forecasts with multi-start fitting, aligned to minute marks (e.g. 00, 30). Outputs per-horizon CSVs (`test_{freq}_{step}step.csv`). |
| `03_garch_pareto_distribution.R` | Fits **AR(1)–GARCH(1,1)** with **Student-t** innovations to training data, extracts standardized residuals, and performs **Extreme Value Theory** tail modelling using **Generalized Pareto Distributions (GPD)** for both upper and lower tails. Includes mean-excess diagnostics and ξ/β stability plots. |
| `04_summary_tables_and_dm_tests.R` | Consolidates test-set CSVs, builds summary and aggregated comparison tables, computes MSE/MAE/RMSE across nested frequencies, and applies **Diebold–Mariano tests** for predictive-accuracy differences. Produces comparative plots (e.g. 1 H vs 30 M vs 15 M). |

---

## 🧩 Data Sources

All scripts operate on **GBPAUD Bid/Ask OHLCV** data exported from **BarChart**, covering  
`2022-01-01 – 2025-01-02` with synchronized timestamps across the following intervals:


Each script merges Bid + Ask to mid-quotes, engineers log-returns and volatility features, and performs the same **train / validation / test** split with  
`test_start_date = "2024-06-01 00:00:00 UTC"`.

---

## ⚙️ Pipeline Summary

1. **Rolling-Window Optimisation**  
   - Sequentially refits `sGARCH(1,1)` models with multi-start (`gosolnp`) parallelization.  
   - Evaluates window sizes per frequency using MSE, RMSE, MAE, and 95% coverage.  
   - Selects windows: 5 M → 2400 obs, 15 M → 800, 30 M → 400, 1 H → 200.

2. **Test-Set Forecasting**  
   - Uses selected windows for non-overlapping multi-step forecasts (`n_ahead = 1…12`).  
   - Saves results as `test_{freq}_{step}step.csv` containing both `summary.*` and nested `details.*` components.

3. **GPD Tail Modelling (EVT)**  
   - Fits AR(1)–GARCH(1,1) t-models on training residuals.  
   - Estimates tail thresholds (e.g. 5th / 95th percentiles), fits GPD via `extRemes::fevd`,  
     and derives 1 % tail-quantiles for risk diagnostics.  
   - Outputs include QQ-plots, mean-excess plots, and ξ/β stability grids.

4. **Summary & Evaluation**  
   - Reads all `test_*.csv` files.  
   - Aggregates multi-step forecasts to match higher-frequency targets (e.g. 6× 5 M → 30 M).  
   - Computes comparative metrics and **Diebold–Mariano tests** across base cases:  
     - 1 H vs 30 M vs 15 M  (1 H benchmark)  
     - 30 M vs 15 M vs 5 M  (30 M benchmark)  
     - 15 M vs 5 M       (15 M benchmark)

---

## 📊 Key Outputs

| Type | Example Files / Objects | Description |
|:-----|:------------------------|:-------------|
| Forecast Results | `test_5m_1step.csv`, `test_30m_2step.csv`, `test_1h_1step.csv` | Multi-step rolling forecasts with summary & details sub-frames. |
| Validation Results | In-memory `results_{freq}` data frames | Window-size tuning metrics and 3-panel performance plots. |
| EVT Diagnostics | Mean-excess plots, GPD fit plots, stability plots | Tail-risk estimation for both sides of the distribution. |
| Summary Tables | `summary_table`, `forecast_errors`, `dm_results` | Aggregated performance and DM test outputs. |
| Figures | ggplot objects (e.g. MSE/RMSE/MAE vs Window; 7-Day Zoom Forecast Plot) | Visualization used in thesis Figures 5.xx. |

---

## 🧠 Notes for Re-running or Extension

- Ensure all Dukascopy CSVs are present and timestamp-aligned.  
- Parallelization is configured via `doFuture` / `multisession` (reduce cores if needed).  
- For extending to other pairs or ν-values (Student-t df), replicate the pipeline:  
  - Stage A → Window optimisation  
  - Stage B → Test-set multi-step forecast  
  - Stage C → Tail diagnostics (GPD)  
  - Stage D → Summary + DM tests  
- Figures and tables correspond to Dataset 1 sections within the Results chapter.

---

**Author:** Aidan Van Klaveren  
**Supervisors:** Prof. Pavel Shevchenko • Prof. Gareth W. Peters • Prof. Stefan Trück  
**Period:** 2022 – 2025 (GBPAUD Study)

---
