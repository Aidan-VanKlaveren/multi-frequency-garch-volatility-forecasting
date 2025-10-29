# Multi-Frequency GARCH Modelling for High-Frequency Volatility Forecasting

This repository contains the R code supporting the Master of Research thesis:

> **"Multi-Frequency GARCH Modelling for High-Frequency Volatility Forecasting in Foreign Exchange Markets"**  
> *Aidan Van Klaveren, Macquarie University (2025)*  
> Supervisors: Prof. Pavel Shevchenko, Prof. Gareth W. Peters, and Prof. Stefan Trück  

---

## 📘 Summary

This project investigates how **sampling frequency** influences the performance and internal coherence of GARCH-based volatility forecasting models in high-frequency foreign exchange (FX) markets.  
Multiple intraday intervals (1-, 5-, 15-, 30-minute, and 1-hour) are evaluated using **rolling-window, walk-forward estimation** frameworks.

The thesis compares several GARCH-family specifications — including **GARCH(1,1)**, **EGARCH**, **TGARCH**, and **GJR–GARCH** with **Student-t innovations** — across multiple datasets to assess forecasting accuracy, multi-step variance aggregation, and cross-frequency consistency.

Forecast accuracy is measured using **MSE**, **RMSE**, **MAE**, **Coverage**, **QLIKE**, and **Quasi-Deviance**, with robustness checks via **Diebold–Mariano (DM)** tests.  
Additionally, higher-frequency forecasts are **aggregated to approximate lower-frequency realised variances**, providing insight into how fine-scale information transfers across time horizons.

A **triangular exchange-rate diagnostic** (GBPEUR–USDEUR–USDGBP) is used to test internal coherence under the no-arbitrage assumption via **MARE** and **correlation-gap metrics**.

---

## 🧩 Repository Structure

## 📂 Section Descriptions

### **Dataset 1 – Dukascopy**
Implements the initial **rolling-window GARCH estimation and evaluation** on GBP/AUD high-frequency data sourced from **Dukascopy**.  

### **Dataset 2 – Barchart**
Focuses on **GJR–GARCH modelling** using **Barchart FX data**.  
Includes analysis of innovation distributions and model stability over time.  

### **Dataset 3 – Currency Triangles**
Tests **internal consistency and no-arbitrage coherence** across currency triangles (GBPEUR–USDEUR–USDGBP). 

---
