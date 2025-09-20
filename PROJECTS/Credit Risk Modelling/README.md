# Credit Risk Modelling

This project applies quantitative credit risk models to evaluate default probabilities, loss distributions, and bond investment suitability for a peer group of corporates.  

## Technical Scope
- **Financial Statement Analysis:** Extracted leverage, liquidity, and coverage ratios for peer firms and benchmarked against sector medians.  
- **Altman Z-Score:** Implemented in Excel to derive solvency indicators, highlighting relative default likelihood.  
- **Merton Structural Model:** Built in Excel (with VBA macros) to estimate firm-level probability of default (PD) using equity volatility and capital structure inputs. Integrated stress testing of volatility and debt levels to simulate shifts in creditworthiness under adverse scenarios.  
- **Ratings Transition Analysis:** Constructed a transition matrix in Excel to model migration probabilities across rating categories, consistent with Basel III requirements.  
- **CreditMetrics Framework:** Applied portfolio-level credit risk assessment by combining transition probabilities, exposure at default (EAD), and loss given default (LGD) to generate loss distribution estimates.  
- **Simulation & Sensitivity:** Used Monte Carlo techniques (via Excel/VBA and R) to simulate correlated defaults, scenario shocks, and portfolio value-at-risk (Credit VaR) at multiple confidence levels.  
- **Reporting & Automation:** Structured outputs into a credit report and Quarto-generated analytics summary, linking model outputs to sector-level investment recommendations.  

## Tools & Technologies
- **Excel / VBA:** Primary modelling environment for Altman Z and Merton structural frameworks, risk scenario automation.  
- **R & Quarto:** Used for loss distribution simulations, CreditMetrics portfolio analysis, and automated report generation.  
- **Monte Carlo Simulation Engines:** Applied for portfolio VaR estimation under correlated credit migrations.  

## Files
- `Credit Report Mock Up.docx` – structured credit risk report  
- `Altman Z Score.xlsx` – solvency model (Excel)  
- `Merton Model.xlsm` – structural PD model with embedded stress testing  
- `HTML Presentation.html` – executive summary of results  

---

This project demonstrates applied use of **Altman Z, Merton structural modelling, transition matrices, and CreditMetrics portfolio techniques** with **Excel, VBA, R, and Monte Carlo simulation** — aligning with industry standards for credit risk assessment under **Basel III / IFRS 9**.
