# Stock Price Modelling using Geometric Brownian Motion

## Overview

This project applies Geometric Brownian Motion (GBM) to model and forecast Tesla (TSLA) stock prices. Using 10 years of historical data (2014-2024), the analysis estimates drift and volatility parameters, validates model assumptions through statistical testing, and generates Monte Carlo simulations to forecast future price paths.

## Methodology

### Parameter Estimation

GBM assumes stock prices follow the stochastic differential equation:

**dS = μS dt + σS dW**

where μ is the drift (expected return), σ is the volatility, and dW is a Wiener process.

Parameters were estimated using:
- **Method of Moments:** Direct calculation from sample statistics
- **Maximum Likelihood Estimation:** Optimisation of the log-likelihood function

### Key Results

| Parameter | Estimate |
|-----------|----------|
| Drift (μ) | 49.8% annualised |
| Volatility (σ) | 56.2% annualised |
| Sample Size | 2,764 trading days |

### Model Validation

Statistical tests assessed whether log returns satisfy GBM assumptions:

| Test | Result |
|------|--------|
| Shapiro-Wilk | Rejected normality (p < 0.05) |
| Jarque-Bera | Rejected normality (p < 0.05) |
| Kolmogorov-Smirnov | Rejected normality (p < 0.05) |

The Q-Q plot and kurtosis analysis revealed fat tails (leptokurtosis), indicating extreme price movements occur more frequently than GBM predicts.

### Monte Carlo Simulation

- **In-sample:** 100 simulated paths compared against actual 2014-2024 prices
- **Out-of-sample:** 1-year forecast with 90% confidence intervals
- **Error metrics:** MAE, RMSE, and MAPE calculated to assess simulation accuracy

## Key Findings

1. **High volatility:** Tesla exhibits annualised volatility of 56%, reflecting its growth stock characteristics
2. **Fat tails:** Returns show excess kurtosis, meaning extreme movements are more common than normal distribution assumes
3. **Model limitations:** GBM's constant volatility assumption is violated — real prices exhibit volatility clustering
4. **Forecast uncertainty:** The 90% confidence interval for 1-year forecasts spans a wide range, reflecting inherent unpredictability

## Tools Used

- **Language:** R
- **Packages:** quantmod, tidyverse, moments, tseries, knitr
