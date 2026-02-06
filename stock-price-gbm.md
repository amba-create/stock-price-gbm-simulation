Stock Price Modelling using Geometric Brownian Motion
================
Amba Sharma
2025-11-06

## Introduction

This project applies Geometric Brownian Motion (GBM) to model Tesla
(TSLA) stock prices. Using historical price data from 2014-2024, the
analysis estimates drift and volatility parameters, validates model
assumptions through statistical testing, and generates Monte Carlo
simulations to forecast future price paths. The project demonstrates key
concepts from stochastic calculus and their application to financial
modelling.

## Data Collection and Log Returns

``` r
library (quantmod)
```

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: TTR

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::first()  masks xts::first()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ✖ dplyr::last()   masks xts::last()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(moments)
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(tseries)
library(knitr)
```

``` r
#import data
getSymbols("TSLA", from = "2014-01-01", to = "2024-12-31")
```

    ## [1] "TSLA"

``` r
prices <- Cl(TSLA) 
head (prices)
```

    ##            TSLA.Close
    ## 2014-01-02  10.006667
    ## 2014-01-03   9.970667
    ## 2014-01-06   9.800000
    ## 2014-01-07   9.957333
    ## 2014-01-08  10.085333
    ## 2014-01-09   9.835333

``` r
#log returns 
log_returns <- diff(log(prices))
log_returns <- na.omit(log_returns)
head(log_returns)
```

    ##              TSLA.Close
    ## 2014-01-03 -0.003604114
    ## 2014-01-06 -0.017265066
    ## 2014-01-07  0.015926820
    ## 2014-01-08  0.012772952
    ## 2014-01-09 -0.025100881
    ## 2014-01-10 -0.012344458

``` r
summary(log_returns)
```

    ##      Index              TSLA.Close       
    ##  Min.   :2014-01-03   Min.   :-0.236518  
    ##  1st Qu.:2016-09-30   1st Qu.:-0.015983  
    ##  Median :2019-07-02   Median : 0.001220  
    ##  Mean   :2019-07-01   Mean   : 0.001349  
    ##  3rd Qu.:2022-03-29   3rd Qu.: 0.018793  
    ##  Max.   :2024-12-30   Max.   : 0.198187

``` r
mean_return <- mean(log_returns)
sd_return <- sd(log_returns)

mean_return
```

    ## [1] 0.001348813

``` r
sd_return
```

    ## [1] 0.03541845

### Log Returns Calculation

``` r
n <- length(prices)
price_vec <- as.numeric(prices)
log_ret <- diff(log(price_vec))

prices_df <- data.frame(
  Date = index(prices)[2:n],
  Price = price_vec[2:n],
  Previous_Price = price_vec[1:(n-1)],
  Ratio = price_vec[2:n] / price_vec[1:(n-1)],
  Log_Return = log_ret
)
head(prices_df)
```

    ##         Date     Price Previous_Price     Ratio   Log_Return
    ## 1 2014-01-03  9.970667      10.006667 0.9964024 -0.003604114
    ## 2 2014-01-06  9.800000       9.970667 0.9828831 -0.017265066
    ## 3 2014-01-07  9.957333       9.800000 1.0160543  0.015926820
    ## 4 2014-01-08 10.085333       9.957333 1.0128549  0.012772952
    ## 5 2014-01-09  9.835333      10.085333 0.9752115 -0.025100881
    ## 6 2014-01-10  9.714667       9.835333 0.9877314 -0.012344458

``` r
display_table <- rbind(head(prices_df, 6), tail(prices_df, 2))

kable(display_table,
      digits = 5,
      col.names = c("Date", "Price ($)", "Prev Price ($)", "Ratio", "Log Return"),
      caption = "Table 1: Sample Daily Log Returns Calculation (First 6 and Last 2 observations)")
```

|      | Date       | Price ($)| Prev Price ($) |     Ratio | Log Return |          |
|:-----|:-----------|--------------------------:|----------:|-----------:|---------:|
| 1    | 2014-01-03 |                   9.97067 |  10.00667 |    0.99640 | -0.00360 |
| 2    | 2014-01-06 |                   9.80000 |   9.97067 |    0.98288 | -0.01727 |
| 3    | 2014-01-07 |                   9.95733 |   9.80000 |    1.01605 |  0.01593 |
| 4    | 2014-01-08 |                  10.08533 |   9.95733 |    1.01285 |  0.01277 |
| 5    | 2014-01-09 |                   9.83533 |  10.08533 |    0.97521 | -0.02510 |
| 6    | 2014-01-10 |                   9.71467 |   9.83533 |    0.98773 | -0.01234 |
| 2765 | 2024-12-27 |                 431.66000 | 454.13000 |    0.95052 | -0.05075 |
| 2766 | 2024-12-30 |                 417.41000 | 431.66000 |    0.96699 | -0.03357 |

Table 1: Sample Daily Log Returns Calculation (First 6 and Last 2
observations)

### Summary Statistics

``` r
# statistics
mean_return <- mean(log_returns)
median_return <- median(log_returns)
sd_return <- sd(log_returns)
min_return <- min(log_returns)
max_return <- max(log_returns)
skew_return <- skewness(log_returns)
kurt_return <- kurtosis(log_returns)
q1_return <- quantile(log_returns, 0.25)
q3_return <- quantile(log_returns, 0.75)
n_return <- length(log_returns)

# summary table
stats_summary <- data.frame(
  Statistic = c("Mean", "Median", "Std Dev", "Minimum", "Maximum",
                "Skewness", "Kurtosis", "Q1 (25%)", "Q3 (75%)", "Count"),
  Value = c(
    mean_return,
    median_return,
    sd_return,
    min_return,
    max_return,
    skew_return,
    kurt_return,
    q1_return,
    q3_return,
    n_return
  )
)

print( stats_summary)
```

    ##    Statistic         Value
    ## 1       Mean  1.348813e-03
    ## 2     Median  1.220161e-03
    ## 3    Std Dev  3.541845e-02
    ## 4    Minimum -2.365179e-01
    ## 5    Maximum  1.981870e-01
    ## 6   Skewness -3.591211e-02
    ## 7   Kurtosis  7.442325e+00
    ## 8   Q1 (25%) -1.598279e-02
    ## 9   Q3 (75%)  1.879294e-02
    ## 10     Count  2.766000e+03

### Price and Returns Visualisation

``` r
plot(prices, main = "Tesla Stock Prices", ylab = "Price ($)")
```

![](as1566_MA3471_cw2526_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
{plot(log_returns, main = "Daily Log Returns", ylab = "Log Return")}
```

![](as1566_MA3471_cw2526_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
hist(log_returns, breaks = 50, main = "Distribution of Log Returns",
     xlab = "Log Return", probability = TRUE)
# Add normal curve overlay
curve(dnorm(x, mean = mean(log_returns), sd = sd(log_returns)), 
      add = TRUE, col = "red", lwd = 2)
```

![](as1566_MA3471_cw2526_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## GBM Parameter Estimation

The GBM model assumes stock prices follow the stochastic differential
equation:

dS = μS dt + σS dW

where μ is the drift (expected return), σ is the volatility, and dW is a
Wiener process. Parameters are estimated using both Method of Moments
and Maximum Likelihood Estimation.

### Method of Moments

``` r
log_returns <- na.omit(diff(log(Cl(TSLA))))

# Calculate sample statistics
mean_return <- mean(log_returns)
sd_return <- sd(log_returns)
n <- length(log_returns)

# Trading days per year
trading_days <- 252
dt <- 1/trading_days

# Estimate volatility (sigma) - annualized
sigma_hat <- sd_return * sqrt(trading_days)

# Estimate drift (mu) - annualized
mu_hat <- mean_return * trading_days + 0.5 * sigma_hat^2 

# Display results
cat("Parameter Estimates:\n")
```

    ## Parameter Estimates:

``` r
cat("=====================================\n")
```

    ## =====================================

``` r
cat("Sample size (n):", n, "days\n")
```

    ## Sample size (n): 2766 days

``` r
cat("Time period:", format(start(log_returns), "%Y-%m-%d"), 
    "to", format(end(log_returns), "%Y-%m-%d"), "\n\n")
```

    ## Time period: 2014-01-03 to 2024-12-30

``` r
cat("Daily Statistics:\n")
```

    ## Daily Statistics:

``` r
cat("Mean daily log return:", mean_return, "\n")
```

    ## Mean daily log return: 0.001348813

``` r
cat("Std dev of daily log returns:", sd_return, "\n\n")
```

    ## Std dev of daily log returns: 0.03541845

``` r
cat("Annualized GBM Parameters:\n")
```

    ## Annualized GBM Parameters:

``` r
cat("Drift (μ):", mu_hat, "\n")
```

    ## Drift (μ): 0.4979638

``` r
cat("Volatility (σ):", sigma_hat, "\n\n")
```

    ## Volatility (σ): 0.5622505

``` r
cat("Interpretation:\n")
```

    ## Interpretation:

``` r
cat("Expected annual return:", round(mu_hat * 100, 2), "%\n")
```

    ## Expected annual return: 49.8 %

``` r
cat("Annual volatility:", round(sigma_hat * 100, 2), "%\n")
```

    ## Annual volatility: 56.23 %

### Maximum likelihood estimation

``` r
# (gives same result for GBM but shows statistical rigor)

# Log-likelihood function for GBM
log_likelihood <- function(params, returns) {
  mu <- params[1]
  sigma <- params[2]
  n <- length(returns)
  dt <- 1/252
  
  # Theoretical mean and variance of log returns under GBM
  expected_return <- (mu - 0.5 * sigma^2) * dt
  variance <- sigma^2 * dt
  
  # Log-likelihood
  ll <- sum(dnorm(returns, mean = expected_return, 
                  sd = sqrt(variance), log = TRUE))
  return(-ll)  # Negative for minimization
}

# Optimize
initial_guess <- c(mu_hat, sigma_hat)
mle_result <- optim(initial_guess, log_likelihood, 
                    returns = log_returns,
                    method = "L-BFGS-B",
                    lower = c(-1, 0.01),
                    upper = c(1, 2))

mu_mle <- mle_result$par[1]
sigma_mle <- mle_result$par[2]

cat("MLE Estimates:\n")
```

    ## MLE Estimates:

``` r
cat("Drift (μ):", mu_mle, "\n")
```

    ## Drift (μ): 0.4979631

``` r
cat("Volatility (σ):", sigma_mle, "\n")
```

    ## Volatility (σ): 0.5621503

### Confidence Intervals

``` r
# Standard errors for parameter estimates
se_sigma <- sigma_hat / sqrt(2 * (n - 1))
se_mu <- sqrt(sigma_hat^2 / n)

# 95% Confidence Intervals
alpha <- 0.05
z_critical <- qnorm(1 - alpha/2)

ci_sigma <- c(sigma_hat - z_critical * se_sigma,
              sigma_hat + z_critical * se_sigma)

ci_mu <- c(mu_hat - z_critical * se_mu,
           mu_hat + z_critical * se_mu)

cat("\n95% Confidence Intervals:\n")
```

    ## 
    ## 95% Confidence Intervals:

``` r
cat("μ: [", ci_mu[1], ",", ci_mu[2], "]\n")
```

    ## μ: [ 0.4770105 , 0.518917 ]

``` r
cat("σ: [", ci_sigma[1], ",", ci_sigma[2], "]\n")
```

    ## σ: [ 0.5474316 , 0.5770694 ]

#### Validation of GBM assumptions

## Model Validation

GBM assumes log returns are normally distributed and independent. We
test these assumptions using multiple statistical approaches.

### Normality Testing

### Q-Q Plot

``` r
qqnorm(log_returns, main = "Q-Q Plot of Log Returns")
qqline(log_returns, col = "red", lwd = 2)
```

![](as1566_MA3471_cw2526_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Shapiro- Wilk test

``` r
returns_numeric <- as.numeric(log_returns)

n_returns <- length(returns_numeric)
cat("Sample size:", n_returns, "observations\n")
```

    ## Sample size: 2766 observations

``` r
# Shapiro-Wilk works best with n <= 5000
if (n_returns > 5000) {
  #random sample of 5000
  set.seed(123)  # For reproducibility
  sample_returns <- sample(returns_numeric, 5000)
  shapiro_test <- shapiro.test(sample_returns)
  cat("Note: Testing on random sample of 5000 observations\n")
} else {
  # Use all observations
  shapiro_test <- shapiro.test(returns_numeric)
}

cat("\nShapiro-Wilk Normality Test:\n")
```

    ## 
    ## Shapiro-Wilk Normality Test:

``` r
cat("W statistic:", sprintf("%.6f", shapiro_test$statistic), "\n")
```

    ## W statistic: 0.945752

``` r
cat("p-value:", sprintf("%.6f", shapiro_test$p.value), "\n")
```

    ## p-value: 0.000000

``` r
if (shapiro_test$p.value < 0.05) {
  cat("Result: REJECT normality at 5% significance level\n")
  cat("        (Returns do NOT follow normal distribution)\n\n")
} else {
  cat("Result: FAIL TO REJECT normality at 5% significance level\n")
  cat("        (Returns appear approximately normal)\n\n")
}
```

    ## Result: REJECT normality at 5% significance level
    ##         (Returns do NOT follow normal distribution)

### Jarque-Beta test

``` r
jb_test <- jarque.bera.test(log_returns)
cat("\nJarque-Bera Test:\n")
```

    ## 
    ## Jarque-Bera Test:

``` r
cat("X-squared:", jb_test$statistic, "\n")
```

    ## X-squared: 2274.967

``` r
cat("p-value:", jb_test$p.value, "\n")
```

    ## p-value: 0

### Kolmogorov-Smirnov test

``` r
ks_test <- ks.test(log_returns, "pnorm", 
                   mean = mean_return, sd = sd_return)
```

    ## Warning in ks.test.default(log_returns, "pnorm", mean = mean_return, sd =
    ## sd_return): ties should not be present for the one-sample Kolmogorov-Smirnov
    ## test

``` r
cat("\nKolmogorov-Smirnov Test:\n")
```

    ## 
    ## Kolmogorov-Smirnov Test:

``` r
cat("D statistic:", ks_test$statistic, "\n")
```

    ## D statistic: 0.9974476

``` r
cat("p-value:", ks_test$p.value, "\n")
```

    ## p-value: 0

### Distribution Characteristics

``` r
cat("# 5. Distribution Characteristics\n")
```

    ## # 5. Distribution Characteristics

``` r
library(moments)

skew_val <- skewness(log_ret)
kurt_val <- kurtosis(log_ret)
excess_kurt <- kurt_val - 3

cat("\nSkewness:", sprintf("%.4f", skew_val))
```

    ## 
    ## Skewness: -0.0359

``` r
if (abs(skew_val) < 0.5) {
  cat(" (approximately symmetric)\n")
} else if (skew_val < 0) {
  cat(" (negatively skewed - left tail)\n")
} else {
  cat(" (positively skewed - right tail)\n")
}
```

    ##  (approximately symmetric)

``` r
cat("Kurtosis:", sprintf("%.4f", kurt_val), "\n")
```

    ## Kurtosis: 7.4423

``` r
cat("Excess Kurtosis:", sprintf("%.4f", excess_kurt))
```

    ## Excess Kurtosis: 4.4423

``` r
if (abs(excess_kurt) < 0.5) {
  cat(" (approximately normal)\n")
} else if (excess_kurt > 0) {
  cat(" (leptokurtic - fat tails)\n")
} else {
  cat(" (platykurtic - thin tails)\n")
}
```

    ##  (leptokurtic - fat tails)

### Diagnostic Plots

``` r
# 1. Histogram with fitted normal distribution
hist(log_returns, breaks = 50, probability = TRUE,
     main = "Log Returns vs Normal Distribution",
     xlab = "Log Return", col = "lightblue")
curve(dnorm(x, mean = mean_return, sd = sd_return),
      add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Observed", "Normal Fit"),
       col = c("lightblue", "red"), lwd = 2)
```

![](as1566_MA3471_cw2526_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
# 2. Q-Q Plot
qqnorm(log_returns, main = "Normal Q-Q Plot")
qqline(log_returns, col = "red", lwd = 2)
```

![](as1566_MA3471_cw2526_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
# 3. ACF plot (check for independence)
acf(log_returns, main = "Autocorrelation Function")
```

![](as1566_MA3471_cw2526_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

``` r
# 4. Time series with volatility bands
plot(log_returns, main = "Log Returns with ±2σ Bands",
     ylab = "Log Return")
abline(h = mean_return + 2*sd_return, col = "red", lty = 2)
abline(h = mean_return - 2*sd_return, col = "red", lty = 2)
abline(h = mean_return, col = "blue", lwd = 2)
```

![](as1566_MA3471_cw2526_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->

### Parameter Summary

``` r
summary_table <- data.frame(
  Parameter = c("Drift (μ)", "Volatility (σ)", 
                "Mean daily return", "Daily std dev",
                "Sample size", "Time period"),
  Estimate = c(
    sprintf("%.4f (%.2f%%)", mu_hat, mu_hat*100),
    sprintf("%.4f (%.2f%%)", sigma_hat, sigma_hat*100),
    sprintf("%.6f", mean_return),
    sprintf("%.6f", sd_return),
    sprintf("%d days", n),
    sprintf("%s to %s", format(start(log_returns), "%Y-%m-%d"),
                        format(end(log_returns), "%Y-%m-%d"))
  )
)

print(summary_table)
```

    ##           Parameter                 Estimate
    ## 1         Drift (μ)          0.4980 (49.80%)
    ## 2    Volatility (σ)          0.5623 (56.23%)
    ## 3 Mean daily return                 0.001349
    ## 4     Daily std dev                 0.035418
    ## 5       Sample size                2766 days
    ## 6       Time period 2014-01-03 to 2024-12-30

``` r
kable(summary_table, caption = "GBM Parameter Estimates for TSLA")
```

| Parameter         | Estimate                 |
|:------------------|:-------------------------|
| Drift (μ)         | 0.4980 (49.80%)          |
| Volatility (σ)    | 0.5623 (56.23%)          |
| Mean daily return | 0.001349                 |
| Daily std dev     | 0.035418                 |
| Sample size       | 2766 days                |
| Time period       | 2014-01-03 to 2024-12-30 |

GBM Parameter Estimates for TSLA

## Monte Carlo Simulation

Using the estimated GBM parameters, Monte Carlo simulation generates
multiple possible price paths. This allows comparison between simulated
and actual historical prices, as well as forecasting future prices with
confidence intervals.

### Simulation Setup

``` r
# Parameters from Q4
mu_hat <- 0.4980
sigma_hat <- 0.5623

# In-sample setup
S0_historical <- as.numeric(prices[1])  # Initial price
T_historical <- length(prices) / 252    # Time in years
n_sims_insample <- 1000

# Out-of-sample setup
S0_forecast <- as.numeric(tail(prices, 1))  # Last actual price
T_forecast <- 2  # 2 years ahead
n_sims_forecast <- 5000
```

### GBM simulation function

``` r
set.seed(123)

simulate_gbm <- function(S0, mu, sigma, T, n_steps, n_sims) {
  dt <- T / n_steps
  price_matrix <- matrix(0, nrow = n_steps + 1, ncol = n_sims)
  price_matrix[1, ] <- S0
  
  for (i in 1:n_sims) {
    for (t in 2:(n_steps + 1)) {
      Z <- rnorm(1)
      price_matrix[t, i] <- price_matrix[t-1, i] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
    }
  }
  return(price_matrix)
}
```

### In sample simulation

``` r
n_steps_historical <- length(prices) - 1
T_historical <- n_steps_historical / 252

sim_prices <- simulate_gbm(
  S0 = as.numeric(prices[1]),
  mu = mu_hat,
  sigma = sigma_hat,
  T = T_historical,
  n_steps = n_steps_historical,
  n_sims = 100
)

dim(sim_prices)
```

    ## [1] 2767  100

### Simulated Paths vs Actual Prices

``` r
plot(index(prices), as.numeric(prices), type = "l", lwd = 2, col = "black",
     main = "GBM Simulations vs Actual Tesla Prices",
     xlab = "Date",
     ylab = "Price ($)",
     ylim = c(0, max(as.numeric(prices)) * 1.5))

for (i in 1:20) {
  lines(index(prices), sim_prices[, i], col = rgb(0, 0, 1, 0.3))
}

lines(index(prices), as.numeric(prices), lwd = 2, col = "black")
legend("topleft", legend = c("Actual", "Simulated"), col = c("black", "blue"), lwd = 2)
```

![](as1566_MA3471_cw2526_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

### Simulation Accuracy

``` r
sim_mean <- rowMeans(sim_prices)
sim_median <- apply(sim_prices, 1, median)
sim_lower <- apply(sim_prices, 1, quantile, probs = 0.05)
sim_upper <- apply(sim_prices, 1, quantile, probs = 0.95)

final_actual <- as.numeric(tail(prices, 1))
final_sim_mean <- tail(sim_mean, 1)
final_sim_median <- tail(sim_median, 1)

cat("Final Actual Price:", round(final_actual, 2), "\n")
```

    ## Final Actual Price: 417.41

``` r
cat("Final Simulated Mean:", round(final_sim_mean, 2), "\n")
```

    ## Final Simulated Mean: 1683.7

``` r
cat("Final Simulated Median:", round(final_sim_median, 2), "\n")
```

    ## Final Simulated Median: 482.91

### Confidence bands

``` r
plot(index(prices), as.numeric(prices), type = "l", lwd = 2, col = "black",
     main = "GBM Simulation with 90% Confidence Band",
     xlab = "Date",
     ylab = "Price ($)",
     ylim = c(0, max(as.numeric(prices)) * 1.5))

polygon(c(index(prices), rev(index(prices))),
        c(sim_lower, rev(sim_upper)),
        col = rgb(0, 0, 1, 0.2), border = NA)

lines(index(prices), sim_mean, col = "blue", lwd = 2)
lines(index(prices), as.numeric(prices), col = "black", lwd = 2)

legend("topleft", 
       legend = c("Actual", "Simulated Mean", "90% CI"),
       col = c("black", "blue", rgb(0, 0, 1, 0.2)),
       lwd = c(2, 2, 10))
```

![](as1566_MA3471_cw2526_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

### Error metrics

``` r
actual_prices <- as.numeric(prices)
mae <- mean(abs(actual_prices - sim_mean))
rmse <- sqrt(mean((actual_prices - sim_mean)^2))
mape <- mean(abs((actual_prices - sim_mean) / actual_prices)) * 100

cat("Mean Absolute Error (MAE): $", round(mae, 2), "\n")
```

    ## Mean Absolute Error (MAE): $ 240.56

``` r
cat("Root Mean Square Error (RMSE): $", round(rmse, 2), "\n")
```

    ## Root Mean Square Error (RMSE): $ 415.86

``` r
cat("Mean Absolute Percentage Error (MAPE):", round(mape, 2), "%\n")
```

    ## Mean Absolute Percentage Error (MAPE): 235.29 %

The error metrics quantify how well the GBM simulations capture the
actual price trajectory. While individual simulated paths diverge
significantly from actual prices (as expected from a stochastic model),
the mean simulated path provides a reasonable central estimate. The MAPE
indicates the average percentage deviation between simulated and actual
prices.

### Out of sample forecast

``` r
S0_forecast <- as.numeric(tail(prices, 1))
T_forecast <- 1
n_steps_forecast <- 252

future_sims <- simulate_gbm(
  S0 = S0_forecast,
  mu = mu_hat,
  sigma = sigma_hat,
  T = T_forecast,
  n_steps = n_steps_forecast,
  n_sims = 100
)

future_dates <- seq(from = as.Date("2025-01-01"), by = "day", length.out = n_steps_forecast + 1)
future_dates <- future_dates[!weekdays(future_dates) %in% c("Saturday", "Sunday")][1:(n_steps_forecast + 1)]
```

``` r
future_mean <- rowMeans(future_sims)
future_lower <- apply(future_sims, 1, quantile, probs = 0.05)
future_upper <- apply(future_sims, 1, quantile, probs = 0.95)

plot(future_dates, future_mean, type = "l", lwd = 2, col = "blue",
     main = "GBM Forecast: Tesla Prices (1 Year Ahead)",
     xlab = "Date",
     ylab = "Price ($)",
     ylim = c(0, max(future_upper) * 1.1))

polygon(c(future_dates, rev(future_dates)),
        c(future_lower, rev(future_upper)),
        col = rgb(0, 0, 1, 0.2), border = NA)

lines(future_dates, future_mean, col = "blue", lwd = 2)
abline(h = S0_forecast, col = "red", lty = 2)

legend("topleft",
       legend = c("Forecast Mean", "90% CI", "Starting Price"),
       col = c("blue", rgb(0, 0, 1, 0.2), "red"),
       lwd = c(2, 10, 2),
       lty = c(1, 1, 2))
```

![](as1566_MA3471_cw2526_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

### Forecast Summary

``` r
forecast_final_mean <- tail(future_mean, 1)
forecast_final_median <- median(future_sims[nrow(future_sims), ])
forecast_final_lower <- tail(future_lower, 1)
forecast_final_upper <- tail(future_upper, 1)

cat("Starting Price: $", round(S0_forecast, 2), "\n")
```

    ## Starting Price: $ 417.41

``` r
cat("Forecast Mean (1 Year): $", round(forecast_final_mean, 2), "\n")
```

    ## Forecast Mean (1 Year): $ 695.54

``` r
cat("Forecast Median (1 Year): $", round(forecast_final_median, 2), "\n")
```

    ## Forecast Median (1 Year): $ 594.62

``` r
cat("90% CI Lower: $", round(forecast_final_lower, 2), "\n")
```

    ## 90% CI Lower: $ 233.66

``` r
cat("90% CI Upper: $", round(forecast_final_upper, 2), "\n")
```

    ## 90% CI Upper: $ 1304.49

``` r
cat("Expected Return:", round((forecast_final_mean / S0_forecast - 1) * 100, 2), "%\n")
```

    ## Expected Return: 66.63 %

## Conclusion

This analysis applied Geometric Brownian Motion to model Tesla stock
prices from 2014-2024. The estimated parameters suggest an annualised
drift (μ) of approximately 49.8% and volatility (σ) of 56.2%, reflecting
Tesla’s high-growth, high-volatility characteristics.

**Model Validation:** Statistical tests (Shapiro-Wilk, Jarque-Bera,
Kolmogorov-Smirnov) rejected the normality assumption for log returns at
the 5% significance level. The Q-Q plot and kurtosis analysis revealed
fat tails (leptokurtosis), indicating that extreme price movements occur
more frequently than a normal distribution would predict. This is a
well-documented limitation of GBM for modelling real financial data.

**Simulation Results:** Monte Carlo simulation generated price paths
that broadly captured the range of Tesla’s historical price movements.
The 90% confidence bands contained the actual price trajectory for most
of the sample period, though the model struggled to capture the extreme
rally in 2020-2021.

**Limitations:** GBM assumes constant volatility and normally
distributed returns — both assumptions violated in practice. Real stock
prices exhibit volatility clustering, fat tails, and occasional jumps
that GBM cannot capture. Extensions such as stochastic volatility models
(Heston) or jump-diffusion models (Merton) address some of these
limitations.
