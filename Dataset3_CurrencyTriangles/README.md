# Dataset 2 — Currency Triangles

R scripts for constructing and analysing **cross-currency triangle consistency** using rolling-window **GJR–GARCH(1,1)** forecasts with Student-t innovations.  
All scripts are self-contained and reproduce the variance-triangle diagnostics and MARE–ρ plots appearing in the final thesis submission.

---

## Overview

The pipeline evaluates whether **variance forecasts across the GBPEUR, USDEUR, and USDGBP legs** of the FX triangle are internally consistent, and how their correlation structure evolves over time.  
All models are fitted on **aligned, orientation-corrected log-return series** using 200-observation rolling windows and refitted every 60 minutes.

---

## Scripts

### 1. `GJRGARCHCurrencyTrianglesRefitEvery60Min1StepForecasts.R`
Generates 1-step GJR–GARCH(1,1) forecasts for each triangle leg.

- **Inputs:**  
  - `{PAIR}_{FREQ}.csv` for each of GBPEUR, USDEUR, USDGBP  
    (frequencies = 1M)
- **Model:**  
  - GJR–GARCH(1,1) with Student-t(df = 10), no mean term  
  - Rolling training window = 200 observations  
  - Refit cadence = 60 minutes
- **Outputs (per frequency):**
  - `tri_step1_<freq>_gjr_tdf10_1step.csv` – detailed per-observation results  
  - `tri_step1_summary_<freq>_gjr_tdf10_1step.csv` – summary metrics (MSE, MAE, RMSE, coverage)

These CSVs are the **base forecasts** used for all downstream triangle and MARE analysis.

---

### 2. `TriangleFormulationFromGJRGARCHForecasts.R`
Derives implied-variance relationships and MARE consistency metrics.

- **Inputs:** `tri_step1_<freq>_gjr_tdf10_1step.csv`
- **Outputs:**
  - `tri_step1_<freq>_gjr_tdf10_1step_triangle_rows.csv`  
  - `tri_step1_<freq>_gjr_tdf10_1step_triangle_window200_summary.csv`
  - `ALL_triangle_rows_combined.csv`  
  - `ALL_triangle_window200_summary.csv`
- **Calculations:**
  - Implied GBPEUR variance  
    \( h_x^{tri} = h_y^{f} + h_z^{f} - 2\,\rho_{yz}^{fit}\sqrt{h_y^{f}h_z^{f}} \)
  - Mean Absolute Relative Error (MARE) between implied and direct forecasts
  - Rolling-window metrics: MARE mean/median/p95, ρ-gap mean/RMSE

This script produces the intermediate CSVs used in both the figure sets below.

---

### 3. `PlotsForMAREandRho.R`
Creates the final summary figures included in the thesis.

- **Inputs:** all `*_triangle_window200_summary.csv` files  
- **Outputs:** stored in `/plots_triangle_summary/`
  - `MARE_fc_mean_by_window.png`
  - `MARE_fc_median_by_window.png`
  - `MARE_fc_p95_by_window.png`
  - `MARE_fc_vs_real_mean_by_window.png`
  - `MARE_tri_vs_real_mean_by_window.png`
  - `rho_gap_mean_by_window.png`
  - `rho_gap_rmse_by_window.png`
  - `freq_<freq>_panels.png` (faceted summary per frequency)
- **Purpose:** visualises triangle-implied vs direct forecast errors and correlation stability over time.

All of these PNGs appear directly in the **Currency Triangle** results section of the thesis.

---

## Execution order

1. `GJRGARCHCurrencyTrianglesRefitEvery60Min1StepForecasts.R`  
2. `TriangleFormulationFromGJRGARCHForecasts.R`  
3. `PlotsForMAREandRho.R`

Each script can be run independently once its required input files are present.

---

## Notes

- All timestamps use **UTC** alignment; rolling windows = 200 obs.  
- Student-t shape fixed at df = 10.  
- Pair orientation automatically corrected so that GBPEUR ≈ USDEUR − USDGBP.  
- Figure scales are log-variance for stability.  
- Scripts require only CRAN packages (`tidyverse`, `rugarch`, `glue`, `cli`).

---

## Questions?

If re-running experiments or extending to other currency sets, adjust:
- `pairs <- c(x="GBPEUR", y="USDEUR", z="USDGBP")`
- `freqs <- c("1M","5M","15M","30M","60M")`
- and rerun the pipeline in the order above.

