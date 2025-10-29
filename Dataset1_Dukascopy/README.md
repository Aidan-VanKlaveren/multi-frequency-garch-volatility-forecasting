# Dataset 1 — GBPAUD  
R scripts for rolling-window GARCH variance forecasting and model comparison using high-frequency GBPAUD data.

---

## Overview
This folder contains the code used to generate the **Dataset 1** results in the thesis (GBPAUD foreign exchange series).  
It covers:

- Rolling-window GARCH forecasting across multiple window sizes.  
- Multi-step variance forecast evaluation (MAE, MSE, RMSE).  
- Training-window optimization and performance diagnostics.  
- GPD tail-fit diagnostics for residuals (extreme value analysis).  
- Summary table builders and figure generation.

---

## Data expectations
- Input: pre-aggregated GBPAUD minute-level log-return CSVs (`GBPAUD1M.csv`, etc.).  
- Columns expected:  
  `Timestamp, log_return` (numeric).  
- Scripts assume the data have already been cleaned, synchronized, and trimmed to complete-minute intervals.  
- Period of analysis: **rolling test windows throughout 2025** (matching Dataset 2).

---

## Key scripts and what they produce

### A) Rolling-window optimization and forecasting
- **`GARCHOptimisingTrainingWindows.R`**  
  Tests multiple rolling window sizes (e.g., 50 / 100 / 200) to identify the window length minimizing forecast errors.  
  Produces diagnostics for each window configuration.  
  **Thesis mapping:** Section **5.1.3**, Figures **5.1–5.4**, Tables **5.2–5.5**.

- **`GARCH_Test_Set_Forecasting_Using_Optimal_Training_Windows.R`**  
  Runs the final test-set forecasting using the *optimal* window lengths determined above.  
  Generates performance metrics (MAE, MSE, RMSE, QLIKE, coverage).  
  **Thesis mapping:** Section **5.1.2** and **5.1.4**, main forecast diagnostics.

---

### B) Multi-step forecasting and evaluation
- **`SummaryTablesForMultiStepForecasting.R`**  
  Aggregates the forecast results into summary tables for different step-ahead horizons and sampling frequencies.  
  **Thesis mapping:** Tables **5.6–5.10** and related discussion on multi-step performance.

---

### C) Tail diagnostics (Extreme Value Theory)
- **`GarchParetoUsingGPDApproximation.R`**  
  Fits **Generalized Pareto Distributions (GPD)** to standardized GARCH residuals to study tail heaviness and threshold stability.  
  Produces QQ plots, mean excess functions, and threshold stability plots.  
  **Thesis mapping:** **Appendix A**, Figures **A.1–A.20**, Tables **A.1–A.5**.

---

## Common settings
- Model: **AR(0)–GARCH(1,1)** with Normal innovations (Student-t used in later datasets).  
- Forecast type: **rolling-window**, **multi-start optimization** using `gosolnp`.  
- Evaluation metrics: **MAE, MSE, RMSE, and **coverage (95%)**.  
- Frequencies used: 5m, 15m, 30m, and 60m intervals (aligned minute bins).  
- Periodization: rolling test windows over 2025 with window sizes tuned via cross-validation.

---

## Typical run order

1. **Load and preprocess data**  
   - Place GBPAUD time-series CSVs in the working directory.  

2. **Optimize window size**  
   - Run `GARCHOptimisingTrainingWindows.R` to test 50 / 100 / 200 observation windows.

3. **Perform forecasting using optimal windows**  
   - Run `GARCH_Test_Set_Forecasting_Using_Optimal_Training_Windows.R`.  
   - Generates the baseline forecast files and summary metrics.

4. **Compile results**  
   - Run `SummaryTablesForMultiStepForecasting.R` to aggregate results into tables for the thesis.

5. **(Optional) Tail-risk diagnostics**  
   - Run `GarchParetoUsingGPDApproximation.R` to evaluate GPD tail fits and residual distributions.

---

## Repro tips
- All scripts assume consistent timestamp alignment and UTC timezone.  
- Random seeds are set internally for reproducible `gosolnp` multi-start fits.  
- Ensure the working directory contains all required CSV files before running any script.

---

## Questions?
If you’re reproducing or extending Dataset 1 (e.g., testing additional FX pairs or comparing alternative GARCH specifications), start with the A/B forecasting scripts, then run the tail analysis (C) for validation.
