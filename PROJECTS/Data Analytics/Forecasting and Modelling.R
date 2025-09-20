---
title: "Financial Data Analytics Project"
author: "Syed Yaseer Rahman"
date: "04/10/2025"
format:
  html:
    code-fold: true
    code-tools: true
    toc: true
    toc-depth: 3
    number-sections: true
bibliography: references.bib
execute:
  echo: true
  warning: false
  message: false
---

```{r setup}
#| label: load-packages
#| include: false

# Load required packages
library(tsfe)
library(tidyverse)
library(tidyquant)
library(fpp2)
library(forecast)
library(rugarch)
library(vars)
library(dplyr)
library(DT)  
library(moments)
library(zoo)
library(stats)
library(tseries)
library(knitr)
library(broom)   
library(ggplot2)
library(tidyr)
library(tibble)
library(lmtest)
library(kableExtra)
library(vars)
library(gridExtra)
library(FinTS)


# Set default theme for ggplot2
theme_set(theme_minimal())
```

# Introduction 

## Research Questions and Project Overview

This report investigates three connected research questions on stock index behavior, predictability, and global linkages, focusing on the Russell 2000 and its relationship with the S&P 500 and FTSE 100:

1.  **Exploratory Analysis:** Do smaller US stocks (Russell 2000) behave as financial theory predicts?
    -   Validates asset pricing assumptions and informs risk modelling.
2.  **Time Series Modelling:** Can statistical models (ARIMA) forecast stock market performance?
    -   Builds on return structure analysis to capture and forecast short-term trends.
3.  **Advanced Analysis:** Is there a long-term connection between major stock markets?
    -   Assesses long-run relationships, informing diversification, risk management, and global investment strategies.

These questions form a structured investigation — from exploratory analysis to formal modelling and long-run market integration.

## Background

The Russell 2000 index tracks smaller U.S. companies and is considered a key indicator of the small-cap sector, which empirically shows greater volatility, lower liquidity and distinct cyclical patterns compared to large-cap indices such as the S&P 500. Understanding its return behaviour, forecastibility, and global linkages (e.g. FTSE 100) is crucial in a connected market.

## Research Objectives

This project aims to: - Analyse the stylised facts and characteristics of Russell 2000 returns to assess consistency with financial theory. - Develop an ARIMA model to forecast small-cap stock performance and assess its predictive power. - Investigate the long-run equillibrium relationship between major indices to evaluate the degree of international market interdependence.

## Literature Review

Stock returns exhibit key stylised facts — volatility clustering, non-normality, and autocorrelation — essential for risk modelling and well-established in financial literature [@cont2001empirical; @tsay2010analysis]. The Russell 2000, tracking small-cap U.S. stocks, shows stronger market-specific dynamics and higher return variability [@fama1993common].

For forecasting, ARIMA models offer a transparent benchmark for capturing return dynamics [@box2015time; @hyndman2018forecast], while GARCH models address volatility more directly [@bollerslev1986generalized]. STL decomposition enhances trend analysis but is not applied here [@cleveland1990stl].

To assess global market linkages, Johansen’s cointegration method tests for long-run equillibrium [@johansen1995likelihood], supported by VAR modelling of short-run spillovers [@sims1980macroeconomics]. Evidence highlights interdependence among indices, especially the S&P 500’s global influence [@forbes2002no], with future research pointing towards machine learning applications in asset pricing [@gu2020empirical].

## Project Workflow

This analysis followed this approximate timeline:

-   **Week 1:** Selected dataset and research questions; completed exploratory analysis
-   **Week 2:** Developed time series models and refined research approach
-   **Week 3:** Conducted advanced analysis and integrated findings
-   **Week 4:** Finalized report and prepared video presentation

# Data and Methods

## Data Description

We analyse three major stock indices to examine return behavior and long-term relationships:

-   **Russell 2000** (`^RUT`): Small-cap U.S. companies\
-   **S&P 500** (`^GSPC`): Large-cap U.S. companies\
-   **FTSE 100** (`^FTSE`): Major U.K. companies

Data were sourced from Yahoo Finance via the `tidyquant` package, using adjusted closing prices to account for dividends and stock splits. The dataset spans 2015–2024 at daily frequency, capturing key events like the 2020 COVID-19 crash. High-frequency data enables analysis of short-run dynamics, volatility clustering, and mean-reversion.

```{r load-raw-data}
#| label: load-raw-data
#| message: false
#| warning: false
#| fig-cap: "Downloading and previewing adjusted price data for major indices"

# Load required packages

# Download adjusted closing prices for each index
russell <- tq_get("^RUT", from = "2015-01-01", to = "2024-12-31")
sp500   <- tq_get("^GSPC", from = "2015-01-01", to = "2024-12-31")
ftse100 <- tq_get("^FTSE", from = "2015-01-01", to = "2024-12-31")

# Combine all indices for display
prices_combined <- list(
  "Russell 2000" = russell,
  "S&P 500" = sp500,
  "FTSE 100" = ftse100
)

# Display interactive tables for each index
DT::datatable(
  dplyr::bind_rows(
    russell %>% mutate(Index = "Russell 2000"),
    sp500 %>% mutate(Index = "S&P 500"),
    ftse100 %>% mutate(Index = "FTSE 100")
  ),
  caption = "Table: Adjusted Daily Prices for Russell 2000, S&P 500, and FTSE 100 (2015-2024)",
  options = list(pageLength = 5, scrollX = TRUE)
)



```

## Data Preparation

Daily log returns are calculated from adjusted closing prices for the Russell 2000, S&P 500, and FTSE 100. Log returns stabilise variance and support stationarity, essential for time series modelling and forecasting [@tsay2010analysis; @cont2001empirical]. Adjusted prices are retained for cointegration and long-run analysis [@hamilton1994time]. Prices were converted into time series objects (252 trading days/year) for modelling. The Russell 2000 supports small-cap return analysis, while the S&P 500 and FTSE 100 enable broader time series and cointegration analysis.

Three structured datasets support the analysis:

-   **combined_returns**: Wide-format returns for multivariate analysis, volatility modelling, and market interdependencies [@bollerslev1986generalized; @forbes2002no].\
-   **returns_long**: Long-format returns for visualising stylised facts like volatility clustering and distributional features [@cont2001empirical; @andersen2001distribution].\
-   **combined_prices**: Wide-format adjusted prices for cointegration and VAR modeling [@sims1980macroeconomics; @hamilton1994time].

These datasets enable systematic analysis of short-term dynamics and long-run relationships, following financial econometrics best practices [@box2015time; @hyndman2018forecast].

```{r prepare-returns}
#| label: prepare-returns
#| message: false
#| warning: false
#| echo: true
#| cache: true
#| fig-cap: "Calculating, summarising daily returns for major indices"

# Calculate log returns (% change)
russell_returns <- russell %>%
  arrange(date) %>%
  mutate(return = 100 * (log(adjusted) - log(lag(adjusted)))) %>%
  drop_na()

sp500_returns <- sp500 %>%
  arrange(date) %>%
  mutate(return = 100 * (log(adjusted) - log(lag(adjusted)))) %>%
  drop_na()

ftse100_returns <- ftse100 %>%
  arrange(date) %>%
  mutate(return = 100 * (log(adjusted) - log(lag(adjusted)))) %>%
  drop_na()

# Prepare long-format for plotting
russell_long <- russell_returns %>% mutate(index = "Russell 2000")
sp500_long   <- sp500_returns %>% mutate(index = "S&P 500")
ftse100_long <- ftse100_returns %>% mutate(index = "FTSE 100")

returns_long <- bind_rows(russell_long, sp500_long, ftse100_long) %>%
  dplyr::select(date, return, index)

# Convert to time series objects
russell_return_ts <- ts(russell_returns$return, frequency = 252)
sp500_return_ts   <- ts(sp500_returns$return, frequency = 252)
ftse100_return_ts <- ts(ftse100_returns$return, frequency = 252)

# Generate Summary Statistics for All Three
russell_stats <- summary(russell_returns$return)
sp500_stats   <- summary(sp500_returns$return)
ftse100_stats <- summary(ftse100_returns$return)

summary_table <- data.frame(
  Statistic   = names(russell_stats),
  Russell2000 = round(as.numeric(russell_stats), 3),
  SP500       = round(as.numeric(sp500_stats), 3),
  FTSE100     = round(as.numeric(ftse100_stats), 3)
)

# Display Table Cleanly in HTML Output
knitr::kable(summary_table, 
             caption = "Table: Summary Statistics of Daily Returns (%) for Russell 2000, S&P 500, and FTSE 100 (2015–2024)")



```

```{r merge-returns}
#| label: merge-returns
#| message: false
#| warning: false
#| fig-cap: "Merging Daily Return Series by Date"

# Load required library

# Merge the return series by date
combined_returns <- full_join(
  russell_returns[, c("date", "return")],
  sp500_returns[, c("date", "return")],
  by = "date", suffix = c("_russell", "_sp500")
) %>%
  full_join(ftse100_returns[, c("date", "return")], by = "date") %>%
  rename(return_ftse = return) %>%
  drop_na()

# Display as interactive table
DT::datatable(combined_returns,
              caption = "Table: Merged Daily Returns for Russell 2000, S&P 500, and FTSE 100",
              options = list(pageLength = 5, scrollX = TRUE))


```

```{r combine-prices}
#| label: combine-prices
#| message: false
#| warning: false
#| fig-cap: "Combined Adjusted Prices for S&P 500, FTSE 100, and Russell 2000"

# Combine adjusted price series for cointegration analysis
combined_prices <- sp500 %>%
  dplyr::select(date, adjusted) %>%
  rename(sp500 = adjusted) %>%
  inner_join(
    ftse100 %>% dplyr::select(date, adjusted) %>% rename(ftse100 = adjusted),
    by = "date"
  ) %>%
  inner_join(
    russell %>% dplyr::select(date, adjusted) %>% rename(russell = adjusted),
    by = "date"
  )

# Log transform for further modelling
log_prices <- combined_prices %>%
  mutate(
    sp500 = log(sp500),
    ftse100 = log(ftse100),
    russell = log(russell)
  )

# Display combined price data as interactive table
DT::datatable(
  combined_prices,
  caption = "Table: Combined Adjusted Prices for S&P 500, FTSE 100, and Russell 2000 (2015-2024)",
  options = list(pageLength = 5, scrollX = TRUE)
)


```

## Methodology

> As George Box famously stated, *“All models are wrong, but some are useful.”* This philosophy underpins the approach to this project: models are simplifications meant to capture key patterns in data, not the full complexity of financial markets.

### Exploratory Analysis Methods

To examine whether small-cap U.S. stocks (Russell 2000) align with financial theory, we explore key stylised facts such as volatility clustering, non-normality, and autocorrelation in returns, as outlined by [@cont2001empirical] and supported by volatility models like [@bollerslev1986generalized]. While focusing on the Russell 2000, comparisons with the S&P 500 and FTSE 100 contextualise whether these behaviors are unique to small-cap stocks or indicative of broader market dynamics.

Methods include:

-   **Time Series Plot of Returns**: Identifies volatility shifts and trends.

-   **Histogram of Returns**: Assesses distributional shape, checking for fat tails and asymmetry.

-   **QQ Plot**: Compares empirical distributions to normality.

-   **Summary Statistics**: Provides mean, standard deviation, skewness, and kurtosis.

-   **Volatility Clustering**: Plots squared returns to reveal persistence in volatility.

-   **Autocorrelation Function (ACF)**: Tests memory in returns and squared returns, expecting weak ACF in raw returns but significant persistence in squared returns.

### Time Series Modelling Methods

To evaluate the predictive power of statistical models, we apply the ARIMA framework to the Russell 2000, S&P 500, and FTSE 100 indices. ARIMA models are widely used in financial econometrics to capture temporal dependencies and forecast return dynamics [@box2015time; @hyndman2018forecast]. Despite high noise in return series [@cont2001empirical], ARIMA offers a structured benchmark for short-term forecasting.

Our approach follows the **Box-Jenkins methodology** [@box2015time], which includes:

-   **Identification**: ACF/PACF plots guide autoregressive and moving average term selection.

-   **Estimation**: Parameters estimated via maximum likelihood.

-   **Validation**: Models are evaluated using residual diagnostics (Ljung-Box test), model selection criteria (AIC/BIC), and forecast accuracy against a baseline random walk.

-   **Forecasting**: 21-day out-of-sample forecasts are generated using the selected models for each index.

While more advanced models (e.g., GARCH) capture volatility, ARIMA serves as a transparent baseline, facilitating comparison across indices and providing a structured framework for evaluating statistical forecasting in equity markets.

### Advanced Analysis Methods : Cointegration and Long-Run Dynamics

To examine the interconnection among major stock indices — S&P 500, FTSE 100, and Russell 2000 — we apply a structured time series framework building on short-run return dynamics.

1.  **Cointegration and Error Correction Modeling**\
    We test for long-run equillibrium using Johansen’s cointegration procedure on log price series. A Vector Error Correction Model (VECM) captures short-run deviations while preserving long-run relationships [@johansen1995likelihood; @sims1980macroeconomics]. Prior to this, short-run dynamics, including volatility clustering, autocorrelation, and stationarity, were assessed via residual diagnostics, ACF/PACF, and stationarity tests.

2.  **Granger Causality Analysis**\
    We use Granger causality tests on log returns to evaluate directional predictability and short-run influence between markets [@forbes2002no].

3.  **VAR Modeling and Impulse Response Functions**\
    A Vector Autoregression (VAR) model is estimated to examine shock transmission across indices. Impulse Response Functions (IRFs) and Forecast Error Variance Decomposition (FEVD) quantify tranmission of shocks.

> *Note: The S&P 500 is used as the impulse variable in IRF and FEVD analyses to reflect its global market leadership.*

This integrated approach captures both long-term equillibrium and short-term transmission dynamics, essential for global portfolio diversification and risk management [@forbes2002no].

# Results

## Exploratory Analysis Results - stylised Facts and Stationarity Testing

We apply the diagnostic tools to examine key stylised facts in the **Russell 2000**, to assess whether its return behaviour aligns with financial theory.

### Time Series of Returns

```{r returns-lineplot}
#| label: returns-lineplot
#| fig-cap: "Daily Log Returns: Russell 2000 vs. FTSE 100 & S&P 500"
#| message: false
#| warning: false
#| dependson: prepare-returns

ggplot() +
  # Plot FTSE 100 and S&P 500 with lower prominence
  geom_line(
    data = subset(returns_long, index %in% c("FTSE 100", "S&P 500")),
    aes(x = date, y = return, color = index),
    alpha = 0.6, linewidth = 0.8
  ) +
  # Highlight Russell 2000
  geom_line(
    data = subset(returns_long, index == "Russell 2000"),
    aes(x = date, y = return, color = index),
    linewidth = 1.2
  ) +
  labs(
    title = "Daily Log Returns: Russell 2000 vs. FTSE 100 & S&P 500",
    x = "Date", y = "Log Return (%)", color = "Index"
  ) +
  scale_color_manual(
    values = c(
      "Russell 2000" = "darkgreen",
      "S&P 500" = "red",
      "FTSE 100" = "yellow"
    )
  ) +
  theme_minimal(base_size = 12)


```

> **Interpretation:**\
> The Russell 2000 shows greater return variability and more pronounced swings than the S&P 500 and FTSE 100, reflecting higher idiosyncratic risk. Clustering of large returns aligns with expected small-cap volatility and known patterns in financial time series.

------------------------------------------------------------------------

### Histogram and Distribution Shape

```{r histogram-russell}
#| label: histogram-russell
#| message: false
#| warning: false
#| fig-cap: "Histogram of Daily Log Returns for Russell 2000"


# Polished Histogram of Russell 2000 Daily Log Returns
ggplot(russell_long, aes(x = return)) +
  geom_histogram(
    bins = 40,
    fill = "steelblue",
    color = "black",  # outline for contrast
    alpha = 0.8
  ) +
  geom_vline(
    aes(xintercept = mean(return, na.rm = TRUE)),
    color = "red",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  labs(
    title = "Histogram of Daily Log Returns for Russell 2000",
    x = "Daily Log Return (%)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 12)


```

> **Interpretation:**\
> The Russell 2000’s return distribution is fat-tailed and negatively skewed, implying a higher likelihood of extreme losses—typical of small-cap stocks with greater market sensitivity and lower liquidity. These non-normal features align with stylised facts and caution against assuming normality in risk assessment.

------------------------------------------------------------------------

### Summary Statistics and Moments

```{r summary-stats-russell}
#| label: summary-stats-russell
#| message: false
#| warning: false

library(moments)

# Summary statistics for Russell 2000 daily log returns
data.frame(
  Index    = "Russell 2000",
  Mean     = mean(russell_returns$return),
  SD       = sd(russell_returns$return),
  Skewness = skewness(russell_returns$return),
  Kurtosis = kurtosis(russell_returns$return)
)


```

> **Interpretation:**\
> The Russell 2000 shows return characteristics consistent with financial theory — higher volatility (1.45%), negative skew (-0.88), and excess kurtosis (14.22) — reflecting small-cap sensitivity to shocks and lower liquidity. In comparison, the S&P 500 and FTSE 100 show milder departures from normality ([Appendix A1](#a1-summary-statistics-sp-500-ftse-100)), underscoring the amplified risk profile of small-cap stocks.

------------------------------------------------------------------------

### QQ Plot of Normality

```{r}
#| label: qqplot
#| fig-cap: "QQ Plot of Russell 2000 Returns"
#| message: false
#| warning: false


qqnorm(russell_returns$return,
       main = "QQ Plot of Russell 2000 Returns",
       xlab = "Theoretical Quantiles (Normal Distribution)",
       ylab = "Sample Quantiles (Russell 2000 Returns)")

qqline(russell_returns$return, col = "red", lwd = 2)



```

> **Interpretation**:\
> The QQ plot shows clear departures from the normal distribution, particularly in the tails, indicating fat tails and leptokurtosis in Russell 2000 returns. This confirms non-normality and reinforces earlier evidence from the histogram and descriptive statistics. 

### Volatility Clustering

```{r}
#| label: squared-returns
#| fig-cap: "Squared daily returns of major indices showing volatility clustering"
#| message: false
#| warning: false

# Compute squared log returns (%)
russell_sq <- russell_returns %>%
  mutate(squared_return = return^2, index = "Russell 2000")

sp500_sq <- sp500_returns %>%
  mutate(squared_return = return^2, index = "S&P 500")

ftse100_sq <- ftse100_returns %>%
  mutate(squared_return = return^2, index = "FTSE 100")

# Combine into one long-format dataframe
squared_long <- bind_rows(russell_sq, sp500_sq, ftse100_sq) %>%
  dplyr::select(date, squared_return, index)

# Plot all three squared return series
ggplot(squared_long, aes(x = date, y = squared_return, color = index)) +
  geom_line(linewidth = 0.6) +
  labs(
    title = "Volatility Clustering: Squared Daily Returns",
    x = "Date",
    y = expression("Squared Return"),
    color = "Index"
  ) +
  scale_color_manual(values = c(
    "Russell 2000" = "green",   # forest green
    "S&P 500" = "red",          # red
    "FTSE 100" = "yellow"       # yellow
  )) +
  theme_minimal()

# 21-day rolling variance

russell_rolling <- russell_returns %>%
  mutate(rolling_var = rollapply(return, width = 21, FUN = var, fill = NA, align = "right"),
         index = "Russell 2000")

sp500_rolling <- sp500_returns %>%
  mutate(rolling_var = rollapply(return, width = 21, FUN = var, fill = NA, align = "right"),
         index = "S&P 500")

ftse100_rolling <- ftse100_returns %>%
  mutate(rolling_var = rollapply(return, width = 21, FUN = var, fill = NA, align = "right"),
         index = "FTSE 100")

rolling_long <- bind_rows(russell_rolling, sp500_rolling, ftse100_rolling) %>%
  dplyr::select(date, rolling_var, index)

# Plot
ggplot(rolling_long, aes(x = date, y = rolling_var, color = index)) +
  geom_line() +
  labs(
    title = "21-Day Rolling Variance of Daily Returns",
    x = "Date",
    y = "Rolling Variance",
    color = "Index"
  ) +
  theme_minimal()

```

> **Interpretation**:\
> The squared returns plot shows clear volatility clustering, with the Russell 2000 experiencing more frequent and intense variance spikes, consistent with time-varying volatility in small-cap equities.
>
> The 21-day rolling variance further highlights sustained periods of elevated volatility following market shocks—most notably during the 2020 COVID-19 crash—with the Russell 2000 exhibiting more persistent variance relative to the S&P 500 and FTSE 100.

------------------------------------------------------------------------

### Autocorrelation and Partial Autocorrelation of Returns and Squared Returns

```{r acf-pacf-russell}
#| label: acf-pacf-russell
#| fig-cap: "ACF and PACF of Returns and Squared Returns for Russell 2000"
#| message: false
#| warning: false

# Set up 2x2 grid layout
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 2, 2, 0))

# Top row: ACF & PACF of returns
acf(russell_returns$return,
    main = "Russell 2000: ACF of Returns",
    lag.max = 20)

pacf(russell_returns$return,
     main = "Russell 2000: PACF of Returns",
     lag.max = 20)

# Bottom row: ACF & PACF of squared returns
acf(russell_returns$return^2,
    main = "Russell 2000: ACF of Squared Returns",
    lag.max = 20)

pacf(russell_returns$return^2,
     main = "Russell 2000: PACF of Squared Returns",
     lag.max = 20)

# Add small side labels for each row
mtext("Returns", side = 2, line = 0.5, outer = TRUE, at = 0.75, cex = 0.8, font = 2)
mtext("Squared Returns", side = 2, line = 0.5, outer = TRUE, at = 0.25, cex = 0.8, font = 2)
```

> **Interpretation**:\
> The Russell 2000 shows low autocorrelation in returns, consistent with weak-form efficiency, but strong persistence in squared returns, reflecting volatility clustering and time-varying variance. Similar patterns are observed for the S&P 500 and FTSE 100 (See [Appendix A2](#a2-autocorrelation-sp500), [A3](#a3-autocorrelation-ftse100) for details), underscoring volatility clustering as a common feature of equity returns. The PACF plots highlight short-term dependence structures, supporting the modelling of conditional heteroscedasticity.

------------------------------------------------------------------------

### ADF Test for Stationarity

```{r adf-test-russell}
#| label: adf-test-russell
#| fig-cap: "ADF Test for Stationarity of Russell 2000 Returns"
#| warning: false
#| message: false

# Run ADF test on processed log returns (not raw price data)
adf_result_russell <- adf.test(russell_returns$return)
adf_result_russell

# Print interpretation
if (adf_result_russell$p.value < 0.05) {
  cat("\nResult: Reject null hypothesis — returns are stationary.\n")
} else {
  cat("\nResult: Fail to reject null — returns may not be stationary.\n")
}
```

> **Interpretation:**\
> The ADF test rejects the null of a unit root for the Russell 2000 (test statistic = -13.017, p = 0.01), confirming stationarity — a key assumption in volatility and risk modelling. Similar results for the S&P 500 and FTSE 100 ([Appendix A4](#appendix-a4)) confirm stationarity across indices, though normality and homoscedasticity remain absent.

### Kwiatkowski–Phillips–Schmidt–Shin (KPSS) Test

```{r kpss-test}
#| label: kpss-test
#| message: false
#| warning: false

# KPSS Test for Russell 2000
kpss_result_russell <- kpss.test(russell_returns$return)
kpss_result_russell


```

> **Interpretation:**\
> The KPSS test for the Russell 2000 (statistic = 0.028, p ≥ 0.1) supports level stationarity, aligning with weak-form efficiency and return modeling assumptions. Similar findings for the S&P 500 and FTSE 100 ([Appendix A5](#a5-kpss-tests)) confirm stationarity across indices.

------------------------------------------------------------------------

### Summary

The Russell 2000 exhibits return characteristics consistent with financial theory. Its returns are fat-tailed and negatively skewed, indicating frequent extreme events and downside risk [@cont2001empirical; @fama1993common]. Volatility clustering — large returns following large returns — suggests conditional heteroscedasticity, aligning with GARCH-type dynamics [@bollerslev1986generalized], supported by squared returns, rolling variance, and ACF analysis. Stationarity tests (ADF and KPSS) confirm returns fluctuate around a stable mean [@box2015time; @hamilton1994time]. Overall, the Russell 2000’s return behavior aligns with theoretical expectations regarding distribution, volatility, and time series properties.

------------------------------------------------------------------------

## Time Series Modeling Results

### Model Identification (ACF & PACF)

To align with the ARIMA modeling framework, we re-plot the ACF and PACF using the return series in time series format (`ts()`), which incorporates the time structure and ensures compatibility with the model fitting functions.

```{r arima-identification-ts}
#| label: arima-identification-ts
#| fig-cap: "ACF and PACF of Russell 2000 Returns (using ts object)"
#| message: false
#| warning: false

# We use the ts object for consistency with ARIMA modeling,
# but convert it to numeric for clearer lag labelling on the x-axis.
par(mfrow = c(1, 2))

acf(as.numeric(russell_return_ts), main = "Russell 2000: ACF", lag.max = 20)
pacf(as.numeric(russell_return_ts), main = "Russell 2000: PACF", lag.max = 20)
```

------------------------------------------------------------------------

### Model Estimation and Selection

```{r arima-estimation}
#| label: arima-estimation
#| message: false
#| warning: false

# Estimate ARIMA model
fit_arima <- auto.arima(russell_return_ts, seasonal = FALSE)


# Display model coefficients neatly
kable(tidy(fit_arima), caption = "ARIMA(5,0,4) Model Coefficients for Russell 2000")

# Display AIC and BIC neatly
model_metrics <- data.frame(
  Metric = c("AIC", "BIC"),
  Value = c(AIC(fit_arima), BIC(fit_arima))
)

kable(model_metrics, caption = "Model Selection Metrics")

# Display accuracy measures neatly
kable(accuracy(fit_arima), caption = "Training Set Error Measures")


```

```{r arima-manual}
#| label: arima-manual
#| message: false
#| warning: false

# Fit ARIMA(1,0,1) manually based on ACF/PACF
fit_manual <- Arima(russell_return_ts, order = c(1, 0, 1))


# Display model coefficients 
kable(tidy(fit_manual), caption = "ARIMA(1,0,1) Model Coefficients for Russell 2000")

# Display AIC & BIC 
model_metrics_manual <- data.frame(
  Metric = c("AIC", "BIC"),
  Value  = c(AIC(fit_manual), BIC(fit_manual))
)

kable(model_metrics_manual, caption = "Model Selection Metrics")

# Display accuracy measures 
kable(accuracy(fit_manual), caption = "Training Set Error Measures")

```

------------------------------------------------------------------------

### Residual Diagnostics and Ljung-Box Test

```{r arima_residuals_clean}
#| label: arima_residuals_clean
#| fig-cap: "Residual Diagnostics and Ljung-Box Tests for ARIMA Models"
#| message: false
#| warning: false

# Auto ARIMA model: ARIMA(5,0,4)
checkresiduals(fit_arima, main = "Residuals from ARIMA(5,0,4) - Auto")

# Ljung-Box test for auto model
auto_test <- Box.test(residuals(fit_arima), lag = 10, type = "Ljung-Box")

# Manual ARIMA model: ARIMA(1,0,1)
checkresiduals(fit_manual, main = "Residuals from ARIMA(1,0,1) - Manual")

# Ljung-Box test for manual model
manual_test <- Box.test(residuals(fit_manual), lag = 10, type = "Ljung-Box")

# Create clean results table
ljung_box_results <- data.frame(
  Model = c("ARIMA(5,0,4)", "ARIMA(1,0,1)"),
  Statistic = c(auto_test$statistic, manual_test$statistic),
  DF = c(auto_test$parameter, manual_test$parameter),
  `p-value` = c(auto_test$p.value, manual_test$p.value)
)

# Display as neat table
kable(ljung_box_results, caption = "Ljung-Box Test Results (lag=10)")

```

> **Interpretation:**\
> For the Russell 2000, ARIMA(5,0,4) was preferred over ARIMA(1,0,1) due to cleaner residuals and superior diagnostics. For the S&P 500 and FTSE 100 ([Appendix A6](#a6-arima-sp500) and [Appendix A7](#a7-arima-ftse100)), ARIMA(1,0,1) models were selected for balancing interpretability and residual adequacy. Model selection across indices prioritised diagnostic robustness, parsimony, and statistical fit.

------------------------------------------------------------------------

### Forecasting with ARIMA

```{r arima-forecast-visual}
#| label: arima-forecast-visual
#| fig-cap: "21-Day ARIMA(5,0,4) Forecast with Highlighted Prediction Region"
#| message: false
#| warning: false

# Generate 21-day forecast using the selected ARIMA(5,0,4) model
forecast_russell <- forecast(fit_arima, h = 21)

# Create data frame for actual and forecasted returns
actual_returns <- data.frame(
  Index = (length(russell_return_ts) - 49):(length(russell_return_ts)),
  Return = tail(russell_return_ts, 50),
  Type = "Actual"
)

forecast_df <- data.frame(
  Index = (length(russell_return_ts) + 1):(length(russell_return_ts) + 21),
  Return = as.numeric(forecast_russell$mean),
  Lower = as.numeric(forecast_russell$lower[, 2]),
  Upper = as.numeric(forecast_russell$upper[, 2]),
  Type = "Forecast"
)

# Combine actual and forecasted data
plot_df <- bind_rows(
  actual_returns,
  dplyr::select(forecast_df, Index, Return, Type)

)

# Plot with improved labelling and confidence ribbon
ggplot(plot_df, aes(x = Index, y = Return, color = Type)) +
  geom_line(size = 1) +
 geom_ribbon(
  data = forecast_df,
  aes(x = Index, ymin = Lower, ymax = Upper),
  fill = "lightblue", alpha = 0.5
)+
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  scale_fill_manual(values = c("Confidence Interval" = "lightblue")) +
  labs(
    title = "21-Day ARIMA(5,0,4) Forecast with Actual Russell 2000 Returns",
    x = "Observed Index",
    y = "Daily Return (%)",
    color = "Type",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

```

> **Interpretation:**\
> The ARIMA(5,0,4) model for the Russell 2000 provided adequate forecasts, though actual returns were more volatile — typical of small-cap stocks. Similar patterns for the S&P 500 and FTSE 100 using ARIMA(1,0,1) models ([Appendix A8](#a8-forecasts)), showed stable forecast means but volatile returns. This aligns with the efficient market hypothesis, confirming limited short-term predictability despite model adequacy.

------------------------------------------------------------------------

### Forecast Accuracy and Benchmark Comparison

```{r forecast-evaluation}
#| label: forecast-evaluation
#| message: false
#| warning: false

# Generate 21-day forecasts
forecast_arima <- forecast(fit_arima, h = 21)
baseline_model <- rwf(russell_return_ts, h = 21)

# Compare forecast accuracy
arima_accuracy <- accuracy(forecast_arima)
baseline_accuracy <- accuracy(baseline_model)

# Create combined results 
accuracy_results <- data.frame(
  Model = c("ARIMA(5,0,4)", "Random Walk"),
  ME = c(arima_accuracy[1,"ME"], baseline_accuracy[1,"ME"]),
  RMSE = c(arima_accuracy[1,"RMSE"], baseline_accuracy[1,"RMSE"]),
  MAE = c(arima_accuracy[1,"MAE"], baseline_accuracy[1,"MAE"]),
  MAPE = c(arima_accuracy[1,"MAPE"], baseline_accuracy[1,"MAPE"]),
  MASE = c(arima_accuracy[1,"MASE"], baseline_accuracy[1,"MASE"]),
  ACF1 = c(arima_accuracy[1,"ACF1"], baseline_accuracy[1,"ACF1"])
)

# Display results
kable(accuracy_results, caption = "Forecast Accuracy Comparison: ARIMA(5,0,4) vs Random Walk")


```

> **Interpretation**:\
> ARIMA models outperformed the random walk benchmark across all indices (RMSE, MAE). The Russell 2000's ARIMA(5,0,4) showed the largest gains, reflecting its higher volatility. For the S&P 500 and FTSE 100 ([Appendix A9](#a9-forecast-accuracy)),
ARIMA(1,0,1) improved forecasts modestly, consistent with large-cap market stability. These results affirm ARIMA's usefulness for short-term forecasting, particularly in volatile settings.

------------------------------------------------------------------------

### Summary

ARIMA models provide valuable structure for modelling stock returns but are inherently limited by the noisy and weakly predictable nature of financial markets [@cont2001empirical; @andersen2001distribution]. They capture autocorrelation and mean reversion patterns effectively [@box2015time; @tsay2010analysis], aiding short-term forecasting.

Across the Russell 2000, S&P 500, and FTSE 100, ARIMA models generated consistent short-term forecasts but offered only modest gains over simpler benchmarks like the random walk, especially in more efficient markets.

Overall, ARIMA remains a useful tool for describing return dynamics and improving short-term forecasts in specific contexts, though its predictive power is fundamentally constrained — consistent with established finance literature [@hyndman2018forecast; @hamilton1994time].

------------------------------------------------------------------------

## Advanced Analysis Results : Exploring Cross-Market Dynamics

This section investigates long-run linkages and short-run dynamics across the Russell 2000, S&P 500, and FTSE 100, building on prior return behaviour analysis.

### Log Prices and Return Comparison

```{r log-price-plot}
#| label: log-price-plot
#| message: false
#| warning: false
#| fig-cap: "Log Prices of S&P 500, FTSE 100, and Russell 2000"

log_prices <- combined_prices %>%
  mutate(
    sp500 = log(sp500),
    ftse100 = log(ftse100),
    russell = log(russell)
  )

log_long <- pivot_longer(log_prices, cols = -date, names_to = "index", values_to = "log_price")

ggplot(log_long, aes(x = date, y = log_price, color = index)) +
  geom_line() +
  labs(title = "Log Prices of Equity Indices", x = "Date", y = "Log Price") +
  theme_minimal()

```

> **Interpretation:**\
> The log price series for all three indices show similar upward trends, though with varying degrees of fluctuation. This visual similarity raises the question of whether the markets may share a stable long-term relationship — something cointegration testing can evaluate formally.

------------------------------------------------------------------------

### Return Behavior and Volatility Clustering

```{r return-volatility}
#| label: return-volatility
#| fig-cap: "Log Returns of S&P 500, FTSE 100, and Russell 2000"
#| message: false
#| warning: false



# Pivot combined_returns for plotting
returns_long <- pivot_longer(
  combined_returns,
  cols = -date,
  names_to = "index",
  values_to = "return"
)

ggplot(returns_long, aes(x = as.Date(date), y = return, color = index)) +
  geom_line(alpha = 0.7) +
  labs(
    title = "Log Returns of Equity Indices",
    x = "Date",
    y = "Return (%)"
  ) +
  theme_minimal()


```

> **Interpretation:**\
> All three indices show volatility clustering, with large shocks followed by further large movements, especially during events like the 2020 market turmoil. Smaller-cap equities, like the Russell 2000, exhibit higher return variability, reflecting greater sensitivity to risk. Recognising these short-run dynamics is crucial for accurate long-run relationship analysis, as they can affect cointegration tests if not properly accounted for.

------------------------------------------------------------------------

### Stationarity Test of Price Series (ADF)

```{r adf-tests}
#| label: adf-tests
#| message: false
#| warning: false
#| fig-cap: "ADF Test Results for Log Price Series"

# Run and store ADF test results
adf_sp500   <- adf.test(na.omit(log_prices$sp500))
adf_ftse100 <- adf.test(na.omit(log_prices$ftse100))
adf_russell <- adf.test(na.omit(log_prices$russell))

# Create summary table
adf_results <- tibble::tibble(
  Index = c("S&P 500", "FTSE 100", "Russell 2000"),
  `Test Statistic` = round(c(adf_sp500$statistic, adf_ftse100$statistic, adf_russell$statistic), 4),
  `P-Value` = signif(c(adf_sp500$p.value, adf_ftse100$p.value, adf_russell$p.value), 4),
  `Hypothesis Result` = ifelse(
    c(adf_sp500$p.value, adf_ftse100$p.value, adf_russell$p.value) < 0.05,
    "Reject H0 (Stationary)",
    "Fail to Reject H0 (Non-Stationary)"
  )
)

# Display table
kable(adf_results, caption = "ADF Test Results for Log Price Series", align = 'lccc')

```

> **Interpretation**:\
> The ADF tests fail to reject the null hypothesis of a unit root for all three log price series, indicating non-stationarity at conventional significance levels. This confirms a key prerequisite for cointegration analysis, allowing us to test for long-run equillibrium relationships across the indices.

------------------------------------------------------------------------

### Johansen Cointegration Test

```{r johansen-test-run}
#| label: johansen-test-run
#| message: false
#| warning: false
#| results: 'hide'

jtest <- ca.jo(log_prices[, c("sp500", "ftse100", "russell")],
               type = "trace", K = 2, ecdet = "const")


summary(jtest)

```


```{r johansen-results-clean}
#| label: johansen-results-clean
#| message: false
#| warning: false
#| fig-cap: "Johansen Trace Test Results: Trace Statistic vs Critical Values"

# Extract test statistics and critical values
trace_stat <- jtest@teststat
critical_val <- jtest@cval

# Create summary table
johansen_results <- data.frame(
  Rank = rownames(critical_val),
  Trace_Statistic = trace_stat,
  Critical_10pct = critical_val[, 1],
  Critical_5pct = critical_val[, 2],
  Critical_1pct = critical_val[, 3]
)

# Display as table
kable(johansen_results, caption = "Johansen Trace Test Results for Cointegration")

```

> **Interpretation:**\
> The Johansen test confirms at least one cointegrating vector, indicating a stable long-run equillibrium linking the S&P 500, FTSE 100, and Russell 2000. This reflects US-UK market integration despite short-term return fluctuations.

------------------------------------------------------------------------

### Vector Error Correction Model (VECM)

```{r vecm-model}
#| label: vecm-model
#| message: false
#| warning: false
#| results: 'hide'

vecm <- cajorls(jtest, r = 1)
summary(vecm$rlm)

# -- Extract residuals and match corresponding dates for plotting --
adjustment <- as.numeric(residuals(vecm$rlm)[, 1])
n_adj <- length(adjustment)
date_vec <- tail(log_prices$date, n_adj)

```

------------------------------------------------------------------------

### Visualising the Adjustment Term

```{r vecm-residuals-all-lineplot}
#| label: vecm-residuals-all-lineplot
#| message: false
#| warning: false
#| fig-width: 10
#| fig-height: 8


# Extract residuals for each index from the VECM model
residuals_all <- as.data.frame(residuals(vecm$rlm))

# Rename columns
colnames(residuals_all) <- c("sp500", "ftse100", "russell")

# Add matching dates
n_adj <- nrow(residuals_all)
residuals_all$Date <- tail(log_prices$date, n_adj)

# Convert to long format
residuals_long <- pivot_longer(
  residuals_all,
  cols = c(sp500, ftse100, russell),
  names_to = "Index",
  values_to = "Adjustment"
)

# Capitalize index names
residuals_long$Index <- recode(residuals_long$Index,
  "sp500" = "S&P 500",
  "ftse100" = "FTSE 100",
  "russell" = "RUSSELL 2000"
)

# Plot
ggplot(residuals_long, aes(x = Date, y = Adjustment)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ Index, ncol = 1, scales = "free_y") +
  labs(
    title = "Cointegration Adjustment Terms Over Time",
    subtitle = "Residuals from VECM for S&P 500, FTSE 100, and Russell 2000",
    x = "Date",
    y = "VECM Adjustment Term (Residual)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 13, face = "bold"))
```

> **Interpretation:**\ 
>The VECM confirms a long-run relationship among indices, with short-lived deviations. The Russell 2000 shows the highest adjustment volatility, reflecting greater shock sensitivity. The S&P 500 adjusts least, acting as a dominant market driver, while the FTSE 100 corrects most efficiently — highlighting distinct adjustment roles.

### Granger Causality Testing

```{r granger-causality}
#| label: granger-causality
#| message: false
#| warning: false
#| results: 'hide'


# Granger causality between all pairs (returns are stationary)
# Russell ↔ S&P 500
grangertest(return_sp500 ~ return_russell, order = 2, data = combined_returns)
grangertest(return_russell ~ return_sp500, order = 2, data = combined_returns)

# FTSE 100 ↔ S&P 500
grangertest(return_sp500 ~ return_ftse, order = 2, data = combined_returns)
grangertest(return_ftse ~ return_sp500, order = 2, data = combined_returns)

# FTSE 100 ↔ Russell 2000
grangertest(return_russell ~ return_ftse, order = 2, data = combined_returns)
grangertest(return_ftse ~ return_russell, order = 2, data = combined_returns)
```

```{r granger-summary-clean}
#| label: granger-summary-clean
#| message: false
#| warning: false

# Create summary table
granger_results <- data.frame(
  Pair = c("S&P 500 → Russell 2000", 
           "Russell 2000 → S&P 500", 
           "S&P 500 → FTSE 100", 
           "FTSE 100 → S&P 500", 
           "FTSE 100 → Russell 2000", 
           "Russell 2000 → FTSE 100"),
  F_Statistic = c(4.0325, 9.4738, 11.5250, 37.4710, 7.4719, 40.7980),
  P_Value = c(0.0178500, 0.0000797, 0.000104, 2.2e-16, 0.005819, 2.2e-16),
  Causality = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
)

# Display table 
kable(granger_results, caption = "Granger Causality Test Summary (Lag=2)", 
      format.args = list(big.mark = ","), 
      align = c("l", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
  column_spec(1, width = "3in") %>%
  column_spec(2, width = "2in") %>%
  column_spec(3, width = "2in") %>%
  column_spec(4, width = "1.5in")
```

> **Interpretation:**\
> Granger causality results show significant bidirectional relationships among the S&P 500, Russell 2000, and FTSE 100. The S&P 500 drives both intra-US and transatlantic linkages, while the Russell 2000 and FTSE 100 also exhibit predictive connections. These results confirm an integrated global market with cross-market shock transmission.

### Impulse Response Analysis (VAR)

```{r var-model}
#| label: var-model
#| message: false
#| warning: false
#| results: hide

# Prepare data for VAR (drop date, remove NA)
combined_returns_var <- combined_returns %>% 
  dplyr::select(-date) %>% 
  na.omit()

# VAR Lag Selection
var_lag <- VARselect(combined_returns_var, lag.max = 10, type = "const")

# Estimate VAR
var_model <- VAR(
  y = combined_returns_var,
  p = 2,
  type = "const"
)

# Stability Check
roots(var_model)
plot(roots(var_model))

```

```{r var-lag-summary}
#| label: var-lag-summary
#| message: false
#| warning: false

# Create Criteria Table
criteria_table <- as.data.frame(t(var_lag$criteria))
criteria_table <- tibble::rownames_to_column(criteria_table, var = "Criterion")

# Create Selected Lag Table
selected_lag <- as.data.frame(var_lag$selection)
selected_lag <- tibble::rownames_to_column(selected_lag, var = "Criterion")
colnames(selected_lag)[2] <- "Selected_Lag"


# Display Tables
kable(criteria_table, caption = "VAR Lag Selection Criteria Values across Lags")

kable(selected_lag, caption = "VAR Lag Selection based on Information Criteria")


```


```{r irf-analysis}
#| label: irf-analysis
#| message: false
#| warning: false

# Compute IRFs with bootstrapped confidence intervals
irf_result <- irf(
  var_model,



impulse = c("return_sp500"),
  response = c("return_sp500", "return_ftse", "return_russell"),
  n.ahead = 10,
  boot = TRUE,
  runs = 100,
  ci = 0.95
)
# NOTE: While IRFs could be computed for each index as the impulse,
# we focus on using S&P 500 as the shock variable because it acts as a 
# dominant global market leader. This aligns with both theoretical reasoning 
# and empirical findings (e.g., minimal adjustment in VECM, Granger causality). 
# Including all impulse variables would increase complexity and was deemed
# beyond the scope of this project
```

```{r irf-plot}
#| label: irf-plot
#| message: false
#| warning: false

# Plot the IRFs to visualize the impact of shocks
plot(irf_result)

```

>**Interpretation:**\
>The IRF illustrates the dynamic response of each index to a one-unit shock in S&P 500 returns. The initial impact is strongest on the S&P 500 itself, with diminishing effects over time. Cross-market responses (FTSE 100, Russell 2000) are visible but smaller, reflecting spillover dynamics.

```{r fevd-analysis}
#| label: fevd-analysis
#| message: false
#| warning: false
#| fig-width: 8
#| fig-height: 6
#| results: hide

# Compute FEVD for the VAR model
fevd_result <- fevd(var_model, n.ahead = 10)

# View FEVD for each variable (Console Only - Hidden in HTML)
fevd_result$return_sp500
fevd_result$return_ftse
fevd_result$return_russell

```

```{r fevd-plot, fig.width=10, fig.height=8}
#| label: fevd-plot
#| message: false
#| warning: false

# Plot FEVD with customized colours
plot(fevd_result, col = c("steelblue", "darkgreen", "firebrick"))


```

>**Interpretation:**\
>  - *S&P 500* returns are the dominant source of shocks, influencing both *Russell 2000* and *FTSE 100* returns over the forecast horizon.
- *Russell 2000* returns are largely driven by their own innovations but show increasing sensitivity to *S&P 500* shocks over time.
- *FTSE 100* returns exhibit the highest cross-market dependence, with substantial forecast error variance explained by *S&P 500* movements.
These findings are consistent with a U.S.-centric spillover structure, where the *S&P 500* acts as a key transmitter of global shocks.
------------------------------------------------------------------------

### Summary

Our analysis confirms a long-run equillibrium among the S&P 500, FTSE 100, and Russell 2000, with the Johansen test identifying at least one significant cointegrating vector — supporting persistent global market integration over episodic contagion [@forbes2002no].

VECM results show that deviations are mainly corrected by the FTSE 100 and Russell 2000, while the S&P 500 acts as a weakly exogenous anchor, consistent with structural VAR theory [@sims1980macroeconomics].

Short-run IRFs and FEVD reveal asymmetric spillovers: S&P 500 shocks strongly influence other indices, while reverse effects are muted — reflecting a U.S.-led hierarchical structure [@cont2001empirical; @bollerslev1986generalized].

Investment implications include reduced long-run diversification due to cointegration and the need to account for dominant market effects — particularly from the U.S. — in risk management and portfolio design [@tsay2010analysis; @hamilton1994time].

Overall, the results highlight dynamic short-run transmission within a stable long-run equillibrium, underscoring the value of econometric modelling in global investment strategy.

# Discussion

## Integration of Findings

The findings across all analysis stages form a cohesive narrative of small-cap return dynamics and global equity market interconnectedness. The exploratory analysis confirms that the Russell 2000 exhibits characteristics like volatility clustering and weak serial correlation, providing a foundation for advanced modelling.

ARIMA time series modeling demonstrated modest, statistically significant forecasting ability over a 21-day horizon, indicating that systematic return patterns can be captured despite market noise, offering practical value for forecasting and strategy.

Cointegration and structural VAR analyses revealed a strong long-term relationship between major indices, highlighting the S&P 500's dominance in driving global market movements. Impulse response and variance decomposition analyses emphasise the directional influence of U.S. markets, with limited impact from smaller or international indices.

Together, these insights show how small-cap dynamics feed into broader market patterns, illustrating the value of combining statistical models with economic theory for risk assessment and cross-border investment strategies.

## Practical Implications

The analyses offer key insights for applied finance and cross-market equity research. The Russell 2000’s high volatility and non-normal returns highlight the need for distributional diagnostics when modelling small-cap risk.

ARIMA models show value in short-term forecasting but are best suited as baseline tools given their univariate limitations amid broader macroeconomic influences.

Cointegration between the Russell 2000, S&P 500, and FTSE 100 reveals shared long-run fundamentals and limits diversification benefits, while offering opportunities for mean-reversion strategies during short-term divergences.

The S&P 500’s dominant role in transmitting shocks underscores its importance as a leading indicator for global portfolio management and risk control, particularly in stress periods when market interconnectedness amplifies volatility.

## Limitations

This study, while methodologically sound, faces several limitations:

Daily returns omit intraday dynamics, potentially missing short-term volatility and market reaction lags. ARIMA and VECM assume linearity, normality, and parameter stability—often unrealistic in financial time series.

Volatility, though evident (see [Appendix A10](#a10-arch-lm-tests)), is not explicitly modeled; the absence of GARCH limits insight into time-varying risk.

 Structural differences across indices (small-cap Russell 2000 vs. large-cap S&P 500 and FTSE 100), along with currency effects and asynchronous trading hours, may bias cross-market comparisons.

Excluding macroeconomic variables limits causal interpretation and may introduce omitted variable bias in long-run models.

## Future Research

Several extensions could enhance this analysis. ARCH LM tests ([Appendix A10](#a10-arch)), confirm significant conditional heteroscedasticity, motivating GARCH models for better volatility modelling. Future work could explore non-linear frameworks (e.g., Threshold VAR, Markov Switching) to capture structural breaks and asymmetric shock responses [@hamilton1994time].

Incorporating macroeconomic variables (e.g., interest rates, inflation) may improve long-run explanatory power. Additionally, leveraging high-frequency data or machine learning methods (e.g., LSTM, volatility clustering models) offers potential for improved forecasting accuracy and capturing latent financial dynamics [@gu2020empirical]. 

Recent reviews of the Value-at-Risk (VaR) literature, such as [@quinn2021developing], provide valuable insights into evolving risk modelling practices and emerging research themes, supporting more informed market risk assessment and financial decision-making.

# References {.unnumbered}

::: {#refs}
:::

## Cited Works

The following key works inform this analysis:

-   For Exploratory Analysis: [@cont2001empirical; @tsay2010analysis]  
-   For Time Series Modeling: [@box2015time; @hyndman2018forecast]
-   For Advanced Analysis: [@johansen1995likelihood; @forbes2002no]  

# Appendix {.unnumbered}

## AI Tool Usage Disclosure

In accordance with course requirements, I disclose the following AI tool usage:

- **Tools Used:**  
  ChatGPT (OpenAI)

- **How They Were Used:**  
  - Assisted in generating R code for technical tasks such as word count calculation excluding code, references, and figures for reporting purposes.  
  - Provided guidance on Quarto syntax, chunk formatting, and best practices for clean HTML output.  
  - Supported in restructuring and refining methodological explanations to improve clarity, technical accuracy, and academic presentation within the report.  
  - Helped draft concise interpretations of econometric model outputs (e.g., ARIMA, GARCH, VECM) in a way suitable for academic reporting.

- **Example Prompts:**  
  - "Generate R code to calculate word count in a .qmd file excluding code chunks, references, and figures."  
  - "How to display model interpretation text below plots in Quarto HTML output?"  
  
- **Verification Process:**  
  - All AI-generated content was carefully reviewed and validated against the output of my R code and analysis.  
  - Generated R code was tested in posit Cloud to ensure correct functionality.  
  - Methodological descriptions and model interpretations were cross-checked with relevant academic sources and project results for accuracy.

## Additional Analyses

### A1. Summary Statistics – S&P 500 & FTSE 100 {#a1-summary-statistics-sp-500-ftse-100}

```{r summary-sp500-ftse100}
#| label: summary-sp500-ftse100
#| message: false
#| warning: false


# Summary statistics for S&P 500
data.frame(
  Index    = "S&P 500",
  Mean     = mean(sp500_returns$return),
  SD       = sd(sp500_returns$return),
  Skewness = skewness(sp500_returns$return),
  Kurtosis = kurtosis(sp500_returns$return)
)

# Summary statistics for FTSE 100
data.frame(
  Index    = "FTSE 100",
  Mean     = mean(ftse100_returns$return),
  SD       = sd(ftse100_returns$return),
  Skewness = skewness(ftse100_returns$return),
  Kurtosis = kurtosis(ftse100_returns$return)
)


```

### A2. Autocorrelation: S&P 500 {#a2-autocorrelation-sp500}

```{r autocorrelation-sp500}
#| label: autocorrelation-sp500
#| message: false
#| warning: false

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# ACF - Returns
acf(sp500_returns$return, main = "", lag.max = 20)
mtext("S&P 500: ACF of Returns", side = 3, line = 1, font = 2)

# PACF - Returns
pacf(sp500_returns$return, main = "", lag.max = 20)
mtext("S&P 500: PACF of Returns", side = 3, line = 1, font = 2)

# ACF - Squared Returns
acf(sp500_returns$return^2, main = "", lag.max = 20)


# PACF - Squared Returns
pacf(sp500_returns$return^2, main = "", lag.max = 20)

```

### A3. Autocorrelation: FTSE 100 {#a3-autocorrelation-ftse100}

```{r autocorrelation-ftse100}
#| label: autocorrelation-ftse100
#| message: false
#| warning: false

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# ACF - Returns
acf(ftse100_returns$return, main = "", lag.max = 20)
mtext("FTSE 100: ACF of Returns", side = 3, line = 1, font = 2)

# PACF - Returns
pacf(ftse100_returns$return, main = "", lag.max = 20)
mtext("FTSE 100: PACF of Returns", side = 3, line = 1, font = 2)

# ACF - Squared Returns
acf(ftse100_returns$return^2, main = "", lag.max = 20)


# PACF - Squared Returns
pacf(ftse100_returns$return^2, main = "", lag.max = 20)


```

### A4. Augmented Dickey-Fuller (ADF) Tests – S&P 500 and FTSE 100 {#appendix-a4}
```{r adf-appendix}
#| label: adf-appendix
#| fig-cap: "ADF Test for Stationarity of S&P 500 and FTSE 100 Returns"
#| warning: false
#| message: false


# ADF Tests on cleaned log returns
adf_sp500 <- adf.test(sp500_returns$return)
adf_ftse  <- adf.test(ftse100_returns$return)

# Display test results
adf_sp500
adf_ftse

# Optional: Interpretation helpers
if (adf_sp500$p.value < 0.05) {
  cat("\n S&P 500: Returns are stationary.\n")
} else {
  cat("\n S&P 500: Returns may not be stationary.\n")
}

if (adf_ftse$p.value < 0.05) {
  cat("\n FTSE 100: Returns are stationary.\n")
} else {
  cat("\n FTSE 100: Returns may not be stationary.\n")
}
```

### A5. Kwiatkowski–Phillips–Schmidt–Shin (KPSS) Tests – S&P 500 and FTSE 100 {#a5-kpss-tests}

```{r kpss- S&P 500 and FTSE 100}
#| label: kpss- S&P 500 and FTSE 100
#| message: false
#| warning: false


# KPSS Tests for S&P 500 and FTSE 100
kpss_sp500 <- kpss.test(sp500_returns$return)
kpss_ftse  <- kpss.test(ftse100_returns$return)

# Display results
kpss_sp500
kpss_ftse

```

### A6 ARIMA and Residual Diagnostics S&P 500 {#a6-arima-sp500}

```{r arima-residual-sp500}
#| label: arima-residual-sp500
#| message: false
#| warning: false

# Estimate models
fit_arima_sp500 <- auto.arima(sp500_return_ts, seasonal = FALSE)
fit_manual_sp500 <- arima(sp500_return_ts, order = c(1, 0, 1))

# Display summaries
cat("Auto ARIMA (S&P 500):\n")
summary(fit_arima_sp500)
cat("\nManual ARIMA(1,0,1) (S&P 500):\n")
summary(fit_manual_sp500)

# Plot residual diagnostics
par(mfrow = c(2, 3))
checkresiduals(fit_arima_sp500, main = "Auto ARIMA (S&P 500)")
checkresiduals(fit_manual_sp500, main = "Manual ARIMA(1,0,1) (S&P 500)")

# Ljung-Box Tests
cat("\nLjung-Box Test (Auto ARIMA):\n")
print(Box.test(residuals(fit_arima_sp500), lag = 10, type = "Ljung-Box"))
cat("\nLjung-Box Test (Manual ARIMA):\n")
print(Box.test(residuals(fit_manual_sp500), lag = 10, type = "Ljung-Box"))

```

### A7 ARIMA and Residual Diagnostics FTSE 100 {#a7-arima-ftse100}

```{r arima-residual-ftse100}
#| label: arima-residual-ftse100
#| message: false
#| warning: false


# Estimate models
fit_arima_ftse <- auto.arima(ftse100_return_ts, seasonal = FALSE)
fit_manual_ftse <- arima(ftse100_return_ts, order = c(1, 0, 1))

# Display summaries
cat("Auto ARIMA (FTSE 100):\n")
summary(fit_arima_ftse)
cat("\nManual ARIMA(1,0,1) (FTSE 100):\n")
summary(fit_manual_ftse)

# Plot residual diagnostics
par(mfrow = c(2, 3))
checkresiduals(fit_arima_ftse, main = "Auto ARIMA (FTSE 100)")
checkresiduals(fit_manual_ftse, main = "Manual ARIMA(1,0,1) (FTSE 100)")

# Ljung-Box Tests
cat("\nLjung-Box Test (Auto ARIMA):\n")
print(Box.test(residuals(fit_arima_ftse), lag = 10, type = "Ljung-Box"))
cat("\nLjung-Box Test (Manual ARIMA):\n")
print(Box.test(residuals(fit_manual_ftse), lag = 10, type = "Ljung-Box"))

```

### A8 21-Day Forecasts with Actual Returns: S&P 500 and FTSE 100 {#a8-forecasts}

```{r appendix-a8-forecast}
#| label: appendix-a8-forecast
#| fig-cap: "21-Day Forecasts with Actual Returns: S&P 500 and FTSE 100"
#| fig-width: 9
#| fig-height: 5
#| message: false
#| warning: false


# ---- Forecast S&P 500 ----
forecast_sp500 <- forecast(fit_manual_sp500, h = 21)

actual_sp500_tail <- tail(sp500_return_ts, 50)

actual_df_sp500 <- data.frame(
  Index = (length(sp500_return_ts) - 49):(length(sp500_return_ts)),
  Return = as.numeric(actual_sp500_tail),
  Type = "Actual"
)

forecast_df_sp500 <- data.frame(
  Index = (length(sp500_return_ts) + 1):(length(sp500_return_ts) + 21),
  Return = as.numeric(forecast_sp500$mean),
  Lower = as.numeric(forecast_sp500$lower[, 2]),
  Upper = as.numeric(forecast_sp500$upper[, 2]),
  Type = "Forecast"
)

plot_df_sp500 <- bind_rows(
  actual_df_sp500,
  dplyr::select(forecast_df_sp500, Index, Return, Type)
)

plot_sp500 <- ggplot(plot_df_sp500, aes(x = Index, y = Return, color = Type)) +
  geom_line(size = 1) +
  geom_ribbon(data = forecast_df_sp500,
              aes(x = Index, ymin = Lower, ymax = Upper),
              fill = "lightblue", alpha = 0.5) +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "steelblue")) +
  scale_fill_manual(values = c("Confidence Interval" = "lightblue")) +
  labs(
    title = "ARIMA(1,0,1) 21-Day Forecast vs Actual Returns: S&P 500",
    x = "Observation Index",
    y = "Daily Return (%)",
    color = "Type",
    fill = ""
  ) +
  theme_minimal(base_size = 12)

# ---- Forecast FTSE 100 ----
forecast_ftse <- forecast(fit_manual_ftse, h = 21)

actual_ftse_tail <- tail(ftse100_return_ts, 50)

actual_df_ftse <- data.frame(
  Index = (length(ftse100_return_ts) - 49):(length(ftse100_return_ts)),
  Return = as.numeric(actual_ftse_tail),
  Type = "Actual"
)

forecast_df_ftse <- data.frame(
  Index = (length(ftse100_return_ts) + 1):(length(ftse100_return_ts) + 21),
  Return = as.numeric(forecast_ftse$mean),
  Lower = as.numeric(forecast_ftse$lower[, 2]),
  Upper = as.numeric(forecast_ftse$upper[, 2]),
  Type = "Forecast"
)

plot_df_ftse <- bind_rows(
  actual_df_ftse,
  dplyr::select(forecast_df_ftse, Index, Return, Type)
)

plot_ftse <- ggplot(plot_df_ftse, aes(x = Index, y = Return, color = Type)) +
  geom_line(size = 1) +
  geom_ribbon(data = forecast_df_ftse,
              aes(x = Index, ymin = Lower, ymax = Upper),
              fill = "lightblue", alpha = 0.5) +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "steelblue")) +
  scale_fill_manual(values = c("Confidence Interval" = "lightblue")) +
  labs(
    title = "ARIMA(1,0,1) 21-Day Forecast vs Actual Returns: FTSE 100",
    x = "Observation Index",
    y = "Daily Return (%)",
    color = "Type",
    fill = ""
  ) +
  theme_minimal(base_size = 12)

# ---- Display Both Plots ----
gridExtra::grid.arrange(plot_sp500, plot_ftse, nrow = 2)


```

### A9 – Forecast Accuracy and Benchmark Comparison: S&P 500 and FTSE 100 {#a9-forecast-accuracy}

```{r appendix-forecast-evaluation}
#| label: appendix-forecast-evaluation
#| message: false
#| warning: false

# --- S&P 500 Forecast Accuracy ---
# Generate 21-day forecasts using selected ARIMA(1,0,1) model
forecast_sp500 <- forecast(fit_manual_sp500, h = 21)

# Generate 21-day baseline forecasts (Random Walk)
baseline_sp500 <- rwf(sp500_return_ts, h = 21)

# Compare forecast accuracy
arima_accuracy_sp500 <- accuracy(forecast_sp500)
baseline_accuracy_sp500 <- accuracy(baseline_sp500)

# Display results
cat("ARIMA(1,0,1) Forecast Accuracy: S&P 500\n")
print(arima_accuracy_sp500)
cat("\nBaseline (Random Walk) Forecast Accuracy: S&P 500\n")
print(baseline_accuracy_sp500)

# --- FTSE 100 Forecast Accuracy ---
# Generate 21-day forecasts using selected ARIMA(1,0,1) model
forecast_ftse <- forecast(fit_manual_ftse, h = 21)

# Generate 21-day baseline forecasts (Random Walk)
baseline_ftse <- rwf(ftse100_return_ts, h = 21)

# Compare forecast accuracy
arima_accuracy_ftse <- accuracy(forecast_ftse)
baseline_accuracy_ftse <- accuracy(baseline_ftse)

# Display results
cat("\n\nARIMA(1,0,1) Forecast Accuracy: FTSE 100\n")
print(arima_accuracy_ftse)
cat("\nBaseline (Random Walk) Forecast Accuracy: FTSE 100\n")
print(baseline_accuracy_ftse)

```

### A10 – ARCH Tests for Residual Heteroskedasticity {#a10-arch}

```{r arch-tests}
#| label: arch-tests
#| message: false
#| warning: false

# --- Russell 2000: ARIMA(5,0,4)
fit_arima <- arima(russell_return_ts, order = c(5, 0, 4))
cat("ARCH Test – Russell 2000 (ARIMA(5,0,4)):\n")
print(ArchTest(residuals(fit_arima), lags = 12))

# --- S&P 500: ARIMA(1,0,1)
fit_manual_sp500 <- arima(sp500_return_ts, order = c(1, 0, 1))
cat("\nARCH Test – S&P 500 (ARIMA(1,0,1)):\n")
print(ArchTest(residuals(fit_manual_sp500), lags = 12))

# --- FTSE 100: ARIMA(1,0,1)
fit_manual_ftse <- arima(ftse100_return_ts, order = c(1, 0, 1))
cat("\nARCH Test – FTSE 100 (ARIMA(1,0,1)):\n")
print(ArchTest(residuals(fit_manual_ftse), lags = 12))

```

## Technical Implementation Notes

- **Code Organization:**  
  Code chunks were structured by analysis sections (Data Preparation, Exploratory Analysis, Time Series Modelling, Advanced Analysis) with descriptive labels for clarity and navigation.

- **Reproducibility Measures:**  
  All data cleaning, transformation, and modelling steps were performed within R code chunks to ensure fully reproducible results upon rendering the `.qmd` file.

- **Package Management:**  
  Required packages were loaded at the start of the document, with `tidyverse`, `knitr`, and other relevant libraries explicitly called.

- **Performance Considerations:**  
  Chunk options were set to suppress unnecessary warnings and messages for cleaner output. Code was vectorized where possible, and computationally heavy models were carefully scoped to relevant sections only.


## References Explained

### Section 1: Exploratory Analysis

-   Q1 (Stock market behavior): Cont (2001) on empirical properties of returns
-   Q2 (Earnings patterns): Foster (1984) on quarterly earnings analysis
-   Q3 (Exchange rate volatility): Andersen et al. (2001) on volatility distributions

Section 2: Time Series Modeling

-   Q1 (ARIMA prediction): Box et al. (2015) on time series analysis
-   Q2 (Smoothing techniques): Cleveland et al. (1990) on decomposition
-   Q3 (GARCH modeling): Bollerslev (1986) on GARCH models

Section 3: Advanced Analysis

-   Q1 (Market connections): Forbes & Rigobon (2002) on market interdependence
-   Q2 (Machine learning): Gu et al. (2020) on ML in finance
-   Q3 (VAR models): Sims (1980) on VAR analysis

# References

## Section 1: Exploratory Analysis References

1.  Cont, R. (2001). "Empirical properties of asset returns: stylised facts and statistical issues." Quantitative Finance, 1, 223–236. • URL: https://www.tandfonline.com/doi/abs/10.1080/713665670
2.  Foster, G. (1984). "Quarterly accounting data: Time-series properties and predictive-ability results." Accounting Review, 59(1), 1–21. • URL: https://www.jstor.org/stable/246028
3.  Andersen, T. G., Bollerslev, T., Diebold, F. X., & Labys, P. (2001). "The distribution of realized exchange rate volatility." Journal of the American Statistical Association, 96(453), 42–55. • URL: https://www.tandfonline.com/doi/abs/10.1198/016214501750332965

## Section 2: Time Series Modeling References

4.  Box, G. E. P., Jenkins, G. M., Reinsel, G. C., & Ljung, G. M. (2015). "Time series analysis: forecasting and control." Journal of Business & Economic Statistics. • URL: https://www.wiley.com/en-se/Time+Series+Analysis%3A+Forecasting+and+Control%2C+5th+Edition-p-9781118675021
5.  Cleveland, R. B., Cleveland, W. S., McRae, J. E., & Terpenning, I. (1990). "STL: A seasonal-trend decomposition procedure based on loess." Journal of Official Statistics, 6(1), 3–73. • URL: https://www.wessa.net/download/stl.pdf
6.  Bollerslev, T. (1986). "Generalized autoregressive conditional heteroskedasticity." Journal of Econometrics, 31(3), 307–327. • URL: https://public.econ.duke.edu/\~boller/Published_Papers/joe_86.pdf

## Section 3: Advanced Analysis References

7.  Forbes, K. J., & Rigobon, R. (2002). "No contagion, only interdependence: measuring stock market comovements." The Journal of Finance, 57(5), 2223–2261. • URL: https://onlinelibrary.wiley.com/doi/abs/10.1111/0022-1082.00494
8.  Gu, S., Kelly, B., & Xiu, D. (2020). "Empirical asset pricing via machine learning." The Review of Financial Studies, 33(5), 2223–2273. • URL: https://academic.oup.com/rfs/article/33/5/2223/5542000
9.  Sims, C. A. (1980). "Macroeconomics and reality." Econometrica, 48(1), 1–48. • URL: https://www.jstor.org/stable/1912017

```{r word-count}
#| label: word-count
#| echo: false
#| message: false
#| warning: false

library(stringr)
lines <- readLines("FIN7028 Rahman_SyedYaseer.qmd")

# Remove code chunks
in_chunk <- FALSE
clean_lines <- c()
for (line in lines) {
  if (grepl("^```", line)) {
    in_chunk <- !in_chunk
  } else if (!in_chunk) {
    clean_lines <- c(clean_lines, line)
  }
}

# Remove References section
ref_start <- which(grepl("# References", clean_lines))
if (length(ref_start) > 0) {
  clean_lines <- clean_lines[1:(ref_start - 1)]
}

text <- paste(clean_lines, collapse = " ")
word_count <- str_count(text, "\\w+")

word_count  



```
