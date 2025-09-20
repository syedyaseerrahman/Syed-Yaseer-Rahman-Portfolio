# Data Analytics

This section applies econometric and time series methods to financial data, combining regression modelling, volatility forecasting, and structural break analysis in R with Quarto-based reporting.

## Econometric Modelling
- Multi-factor regressions extending CAPM with currency exposures.  
- Diagnostics: Breusch–Pagan (heteroscedasticity), Durbin–Watson and Breusch–Godfrey (autocorrelation), Ramsey RESET, Jarque–Bera, VIF (multicollinearity), Cook’s Distance.  
- Structural break tests using Chow and time-dummy models.  
- Regularisation with Ridge regression to improve model stability.  
- Tools: R (`car`, `lmtest`, `sandwich`, `strucchange`, `glmnet`).  

## Time Series & Risk Modelling
- Forecasting with ARIMA, ETS, SMA/WMA smoothing, including residual checks and accuracy metrics (RMSE, MAE, AIC/BIC).  
- Volatility modelling with GARCH-family processes.  
- VAR/VECM for spillovers and Johansen cointegration for long-run dynamics.  
- Machine learning benchmarks using reinforcement learning and SVR.  
- Tools: R (`forecast`, `rugarch`, `urca`, `ggplot2`, `caret`), Quarto for reproducible reporting.  

## Files
- `Econometric Modelling.R` – econometric regression workflow.  
- `Econometric Modelling Report.pdf` – detailed econometric analysis.  
- `Time Series & Risk Modelling QUARTO` – Quarto source for forecasting and volatility modelling.  
