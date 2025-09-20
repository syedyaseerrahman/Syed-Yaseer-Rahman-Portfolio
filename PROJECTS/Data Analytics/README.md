# Data Analytics – Econometric Modelling

This project investigates the **foreign exchange exposure of Hikma Pharmaceuticals (LSE: HIK)** using advanced econometric and time series methods, combining R scripting, Quarto reporting, and Bloomberg data.

## Scope
- Analysed ~2,500 daily observations (2014–2024) of Hikma’s stock, FTSE 100, and major FX pairs (USD, EUR, CHF, SAR, JOD, EGP, CNY vs GBP).  
- Measured stock excess returns under **CAPM and multi-factor FX risk models**, accounting for **structural breaks** (Brexit, COVID-19).  

## Methods
- **Regression Models:** OLS, robust regression (HC SE), polynomial terms, Ridge regression, time-dummy models.  
- **Diagnostics:** Breusch–Pagan, Jarque–Bera, RESET, Durbin–Watson, Breusch–Godfrey, VIF, Cook’s Distance, Chow Test.  
- **Evaluation:** R², Adjusted R², AIC/BIC, MSE across competing models.  

## Tools
- **R Packages:** `lm`, `car`, `lmtest`, `sandwich`, `strucchange`, `moments`, `caret`, `glmnet`, `MASS`, `ggplot2`, `gridExtra`.  
- **Quarto:** Reproducible analytics with integrated code, plots, and diagnostics.  
- **Bloomberg / Excel:** Data extraction, validation, and supplementary checks.  

## Results
- CAPM market factor was statistically significant in explaining Hikma returns.  
- FX exposures varied; JOD/GBP and EGP/GBP were significant, USD/GBP was not.  
- Robust time-dummy model delivered **Adjusted R² ≈ 0.66**, outperforming baseline OLS and Ridge.  
- Structural break tests confirmed shifts in risk exposures post-Brexit and COVID-19.  

## Deliverables
- `Econometric Modelling.R` – complete modelling workflow in R.  
- `Econometric Modelling Report.pdf` – detailed analysis and interpretation.  
- `FIN7028.qmd` / `.html` – Quarto automated report with code and outputs.  

