str(HIKMA_40455377)
summary(HIKMA_40455377)
head(HIKMA_40455377)
HIKMA_40455377$STKLOGSYR = c(NA, 100*diff(log(HIKMA_40455377$`HIK LN Equity`)))
HIKMA_40455377$INDXLOGSYR = c(NA, 100*diff(log(HIKMA_40455377$`UKX Index`)))
HIKMA_40455377$USDGBPLOGSYR = c(NA, 100*diff(log(HIKMA_40455377$`USDGBP Curncy`)))
HIKMA_40455377$EURGBPLOGSYR = c(NA, 100*diff(log(HIKMA_40455377$`EURGBP Curncy`)))
HIKMA_40455377$CNYGBPLOGSYR = c(NA, 100*diff(log(HIKMA_40455377$`CNYGBP Curncy`)))
HIKMA_40455377$JODGBPLOGSYR = c(NA, 100*diff(log(HIKMA_40455377$`JODGBP Curncy`)))
HIKMA_40455377$EGPGBPLOGSYR = c(NA, 100*diff(log(HIKMA_40455377$`EGPGBP Curncy`)))
HIKMA_40455377$SARGBPLOGSYR = c(NA, 100*diff(log(HIKMA_40455377$`SARGBP Curncy`)))
HIKMA_40455377$CHFGBPLOGSYR = c(NA, 100*diff(log(HIKMA_40455377$`CHFGBP Curncy`)))
HIKMA_40455377$STKEXCSSRTNSYR = HIKMA_40455377$STKLOGSYR - HIKMA_40455377$`UKGTB3M Index`
HIKMA_40455377$INDXEXCSSRTNSYR = HIKMA_40455377$INDXLOGSYR - HIKMA_40455377$`UKGTB3M Index`
lm_returnsSYR = lm(HIKMA_40455377$STKEXCSSRTNSYR~HIKMA_40455377$INDXEXCSSRTNSYR + HIKMA_40455377$USDGBPLOGSYR +HIKMA_40455377$EURGBPLOGSYR+HIKMA_40455377$JPYGBPLOGSYR+HIKMA_40455377$CNYGBPLOGSYR+HIKMA_40455377$JODGBPLOGSYR+HIKMA_40455377$EGPGBPLOGSYR+HIKMA_40455377$SARGBPLOGSYR+HIKMA_40455377$CHFGBPLOGSYR)
summary(lm_returnsSYR)
install.packages("car")
install.packages("lmtest")
library(car)
library(lmtest)
#Testing if INDXEXCSSRTNSYR coefficient is zero; (H0 :B2 = 0) 
names(coef(lm_returnsSYR))
linearHypothesis(lm_returnsSYR, "INDXEXCSSRTNSYR = 0")
lm_returnsSYR = lm(STKEXCSSRTNSYR~INDXEXCSSRTNSYR + USDGBPLOGSYR + EURGBPLOGSYR + CHFGBPLOGSYR + CNYGBPLOGSYR + SARGBPLOGSYR +JODGBPLOGSYR + EGPGBPLOGSYR +JPYGBPLOGSYR, data = HIKMA_40455377 )
summary(lm_returnsSYR)
linear_hypothesis = linearHypothesis(lm_returnsSYR, "INDXEXCSSRTNSYR = 0")
print(linear_hypothesis)
joint_hypothesis = linearHypothesis(lm_returnsSYR, c("INDXEXCSSRTNSYR = 0", "USDGBPLOGSYR = 0", "EURGBPLOGSYR = 0", "CHFGBPLOGSYR = 0", "CNYGBPLOGSYR = 0","SARGBPLOGSYR = 0", "JODGBPLOGSYR = 0", "EGPGBPLOGSYR = 0", "JPYGBPLOGSYR = 0"))
joint_hypothesis = linearHypothesis(lm_returnsSYR, c("INDXEXCSSRTNSYR = 0", "USDGBPLOGSYR = 0", "EURGBPLOGSYR = 0", "CHFGBPLOGSYR = 0", "CNYGBPLOGSYR = 0", "SARGBPLOGSYR = 0", "JODGBPLOGSYR = 0", "EGPGBPLOGSYR = 0", "JPYGBPLOGSYR = 0"))
print(joint_hypothesis)
#Heteroskedasticity 
bptest(formula = lm_returnsSYR, data = HIKMA_40455377, studentize = FALSE)
bptest(formula = lm_returnsSYR, data = HIKMA_40455377, studentize = TRUE)
install.packages("sandwich")
library(sandwich)
coeftest(lm_returnsSYR,vcov(lm_returnsSYR, type = "HC1"))
coeftest(lm_returnsSYR,vcov. = NeweyWest(lm_returnsSYR, lag = 7, adjust = T, prewhite = F))
dwtest(lm_returnsSYR)
bgtest(lm_returnsSYR, order = 7)
bgtest(lm_returnsSYR, order = 3)
bgtest(lm_returnsSYR, order = 10)
install.packages("moments")
library(moments)
skewness(lm_returnsSYR$residuals)
kurtosis(lm_returnsSYR$residuals)
hist(lm_returnsSYR$residuals, main = "")
box(hist(lm_returnsSYR$residuals, main = ""))
hist(lm_returnsSYR$residuals, main = "")
box(lm_returnsSYR$residuals)
hist(lm_returnsSYR$residuals, main = "")
jarque.test(lm_returnsSYR$residuals)
agostino.test(lm_returnsSYR$residuals)
anscombe.test(lm_returnsSYR$residuals)
plot(HIKMA_40455377$Dates [-(1:2)], lm_returnsSYR$residuals [-(1:2)], type = "1", col("red"), xlab = "", ylab = "")
length(HIKMA_40455377$Dates)
length(lm_returnsSYR$residuals)
plot(HIKMA_40455377$Dates[-(1)], lm_returnsSYR$residuals, type = "l", col = "red", xlab = "", ylab = "")
lines(HIKMA_40455377$Dates[-(1)],lm_returnsSYR$fitted.values)
legend("topright", c("Residuals", "Fitted", col = c("red", "black"), lty=1))
plot.new(HIKMA_40455377$Dates[-(1)], lm_returnsSYR$residuals, type = "1", col ="red", xlab = "", ylab = "")
plot(HIKMA_40455377$Dates[-(1)], lm_returnsSYR$residuals, type = "1", col ="red", xlab = "", ylab = "")
plot(HIKMA_40455377$Dates[-(1)], lm_returnsSYR$residuals, type = "1", col = "red", xlab = "", ylab = "")
plot(HIKMA_40455377$Dates[-(1)], lm_returnsSYR$residuals, type = "l", col = "red", xlab = "", ylab = "")
lines(HIKMA_40455377$Dates[-(1)],lm_returnsSYR$fitted.values)
legend (" bottomright ",c (" Residuals "," Fitted ") , col = c ("red "," black ") , lty =1)
legend("bottomright", c("Residuals", "Fitted", col= c("red", "black", lty = 1)))
plot(HIKMA_40455377$Dates[-(1)], lm_returnsSYR$residuals, type = "l", col = "red", xlab = "", ylab = "")
lines(HIKMA_40455377$Dates[-(1)],lm_returnsSYR$fitted.values)
legend("topright", legend = c("Residuals", "Fitted"), col = c("red", "black"), lty = 1)
sort(HIKMA_40455377$Dates)
sort(lm_returnsSYR$residuals)
#identifying outliers
plot(residuals(lm_returnsSYR, main = ""))
residuals_data <- residuals(lm_returnsSYR)
outliers <- which(abs(residuals_data) > 3)
outliers
plot(HIKMA_40455377$Dates[-(1)], lm_returnsSYR$residuals, type = "l", col = "red", xlab = "", ylab = "")
lines(HIKMA_40455377$Dates[-(1)],lm_returnsSYR$fitted.values)
legend("topright", legend = c("Residuals", "Fitted"), col = c("red", "black"), lty = 1)
jarque.test(lm_returnsSYR$residuals)
install.packages("moments")
library(moments)
jarque.test(lm_returnsSYR$residuals)
install.packages("car")
library(car)
install.packages("lmtest")
library(lmtest)
summary(lm_returnsSYR)

#Tests from scratch
lm_returnsSYR = lm(STKEXCSSRTNSYR~INDXEXCSSRTNSYR + USDGBPLOGSYR + EURGBPLOGSYR + CHFGBPLOGSYR + CNYGBPLOGSYR + SARGBPLOGSYR +JODGBPLOGSYR + EGPGBPLOGSYR +JPYGBPLOGSYR, data = HIKMA_40455377 )
summary(lm_returnsSYR)
linear_hypothesis = linearHypothesis(lm_returnsSYR, "INDXEXCSSRTNSYR = 0", "USDGBPLOGSYR = 0", "EURGBPLOGSYR = 0", "CHFGBPLOGSYR = 0", "CNYGBPLOGSYR = 0", "SARGBPLOGSYR=0", "JODGBPLOGSYR=0", "EGPGBPLOGSYR =0", "JPYGBPLOGSYR = 0")
print(linear_hypothesis)
model_returnsSYR = lm(STKEXCSSRTNSYR~INDXEXCSSRTNSYR + USDGBPLOGSYR + EURGBPLOGSYR + CHFGBPLOGSYR + CNYGBPLOGSYR + SARGBPLOGSYR +JODGBPLOGSYR + EGPGBPLOGSYR +JPYGBPLOGSYR, data = HIKMA_40455377 )
summary(model_returnsSYR)
joint_hypothesis = linearHypothesis(model_returnsSYR,c("INDXEXCSSRTNSYR = 0", "USDGBPLOGSYR = 0", "EURGBPLOGSYR = 0", "CHFGBPLOGSYR = 0", "CNYGBPLOGSYR = 0", "SARGBPLOGSYR=0", "JODGBPLOGSYR=0", "EGPGBPLOGSYR =0", "JPYGBPLOGSYR = 0"))
print(joint_hypothesis)
bptest(model_returnsSYR)
residualsSYR
jarque.bera.test(residualsSYR)
reset(model_returnsSYR)
vif(model_returnsSYR)

# Required Libraries
library(caret)
library(glmnet)
library(ggplot2)

# Filtering Data to Include Relevant Variables
selected_vars = c("STKEXCSSRTNSYR", "INDXEXCSSRTNSYR", "USDGBPLOGSYR", 
                   "EURGBPLOGSYR", "CNYGBPLOGSYR", "JODGBPLOGSYR", 
                   "EGPGBPLOGSYR", "SARGBPLOGSYR", "CHFGBPLOGSYR", "Dates")
HIKMA_40455377_reduced = HIKMA_40455377[, selected_vars]

# Median Imputation for Missing Values
pre_process = preProcess(HIKMA_40455377_reduced, method = 'medianImpute')
HIKMA_40455377_imputed = predict(pre_process, HIKMA_40455377_reduced)

# Outlier Identification Using Cook's Distance

summary(model_returnsSYR)
residuals_ols <- residuals(model_returnsSYR)
fitted_values_ols <- fitted(model_returnsSYR)

lm_approx_model <- lm(formula(model_returnsSYR), data = HIKMA_40455377_imputed) 
cooks_distances <- cooks.distance(lm_approx_model)

valid_cooks <- cooks_distances[is.finite(cooks_distances) & !is.na(cooks_distances)]


plot(valid_cooks,type = "h",main = "Cook's Distance (Approximation)",xlab = "Observation Index", ylab = "Cook's Distance",col = "red")

threshold = 4 / nrow(HIKMA_40455377_imputed)  # Replace with your dataset

abline(h = threshold, col = "blue", lty = 2)

#influential observations
threshold =  4 / nrow(HIKMA_40455377_imputed)  # Cook's Distance threshold
influential_obs = which(cooks_distances > threshold)
cat("Influential Observations:\n", influential_obs, "\n")

#removing influential observations
data_clean = HIKMA_40455377_imputed[-influential_obs, ]


#polynomial terms 
data_clean$INDXEXCSSRTNSYR_poly = poly(data_clean$INDXEXCSSRTNSYR, 2, raw = TRUE)
sum(is.na(data_clean$INDXEXCSSRTNSYR))
data_clean = data_clean[!is.na(data_clean$INDXEXCSSRTNSYR), ]
data_clean$INDXEXCSSRTNSYR_poly = poly(data_clean$INDXEXCSSRTNSYR, 2, raw = TRUE)[, 2]
nrow(data_clean)
nrow(poly(data_clean$INDXEXCSSRTNSYR, 2, raw = TRUE))
poly_model = rlm(STKEXCSSRTNSYR ~ INDXEXCSSRTNSYR + INDXEXCSSRTNSYR_poly, data = data_clean) 
summary(poly_model)



#time dummy variables
data_clean$Dates = as.Date(data_clean$Dates)
data_clean$Year = format(data_clean$Dates, "%Y")
data_clean$TimeDummy = as.factor(data_clean$Year)
time_dummy_model = rlm(STKEXCSSRTNSYR ~ . - Dates - Year + TimeDummy, data = data_clean)
summary(time_dummy_model)


install.packages("strucchange")
library(strucchange)

# identifying break point
break_point = as.Date("2020-03-01")

#data before and after the break point
before_break = subset(data_clean, Dates < break_point)
after_break = subset(data_clean, Dates >= break_point)

full_model = rlm(STKEXCSSRTNSYR ~ INDXEXCSSRTNSYR + USDGBPLOGSYR + EURGBPLOGSYR + 
                    CHFGBPLOGSYR + CNYGBPLOGSYR + JODGBPLOGSYR + EGPGBPLOGSYR + SARGBPLOGSYR, 
                  data = data_clean)


model_before = rlm(STKEXCSSRTNSYR ~ INDXEXCSSRTNSYR + USDGBPLOGSYR + EURGBPLOGSYR + 
                      CHFGBPLOGSYR + CNYGBPLOGSYR + JODGBPLOGSYR + EGPGBPLOGSYR + SARGBPLOGSYR, 
                    data = before_break)


model_after = rlm(STKEXCSSRTNSYR ~ INDXEXCSSRTNSYR + USDGBPLOGSYR + EURGBPLOGSYR + 
                     CHFGBPLOGSYR + CNYGBPLOGSYR + JODGBPLOGSYR + EGPGBPLOGSYR + SARGBPLOGSYR, 
                   data = after_break)


break_index = sum(data_clean$Dates < break_point)

#Chow Test
library(strucchange)
chow_test = sctest(STKEXCSSRTNSYR ~ INDXEXCSSRTNSYR + USDGBPLOGSYR + EURGBPLOGSYR + 
                      CHFGBPLOGSYR + CNYGBPLOGSYR + JODGBPLOGSYR + EGPGBPLOGSYR + SARGBPLOGSYR, 
                    data = data_clean, type = "Chow", point = break_index)

print(chow_test)

#fitted values for before and after the break
fitted_before = predict(model_before, newdata = before_break)
fitted_after = predict(model_after, newdata = after_break)

#scatterplot for Stock Excess Returns
plot(data_clean$Dates, data_clean$STKEXCSSRTNSYR, 
     type = "p", col = "blue", pch = 20, main = "Structural Break Analysis", 
     xlab = "Date", ylab = "Stock Excess Returns")


lines(before_break$Dates, fitted_before, col = "green", lwd = 2)
lines(after_break$Dates, fitted_after, col = "red", lwd = 2)
abline(v = break_point, col = "black", lty = 2, lwd = 2)
legend("topright", legend = c("Before Break", "After Break"), 
       col = c("green", "red"), lwd = 2)

#Ridge Regression

best_lambda <- cv_ridge_model$lambda.min
print(best_lambda)

y_pred <- predict(final_ridge_model, X, s = "lambda.min")
rsq <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
print(paste("R-squared: ", rsq))

# Predictive Accuracy
ridge_pred <- predict(ridge_model, newx = X)
ridge_residuals <- y - ridge_pred

ridge_mse <- mean(ridge_residuals^2)


ridge_rsq <- 1 - sum(ridge_residuals^2) / sum((y - mean(y))^2)

cat("Ridge MSE:", ridge_mse, "\n")
cat("Ridge R-squared:", ridge_rsq, "\n")

plot(y_pred, y - y_pred, 
     main = "Residuals vs Fitted Values (Ridge Regression)",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

qqnorm(y - y_pred, main = "Q-Q Plot of Residuals (Ridge Regression)")
qqline(y - y_pred, col = "red", lwd = 2)

hist(y - y_pred, breaks = 30, col = "lightblue", border = "black",
     main = "Histogram of Residuals (Ridge Regression)", 
     xlab = "Residuals")


#Robust Time Dummy Model
robust_time_dummy_model <- rlm(STKEXCSSRTNSYR ~ INDXEXCSSRTNSYR + USDGBPLOGSYR + EURGBPLOGSYR + CNYGBPLOGSYR + JODGBPLOGSYR + EGPGBPLOGSYR +SARGBPLOGSYR+ CHFGBPLOGSYR + INDXEXCSSRTNSYR_poly + TimeDummy, data = data_clean)
  

summary(robust_time_dummy_model)
coeftest(robust_time_dummy_model, vcov = vcovHC(robust_time_dummy_model, type = "HC1"))

install.packages("tseries")
library(tseries)
install.packages("lmtest")
library(lmtest)

reset_test <- resettest(robust_time_dummy_model, power = 2, type = "fitted")
print(reset_test)
dwtest(robust_time_dummy_model)


#Residuals
fitted_values= fitted(robust_time_dummy_model)
actual_values = data_clean$STKEXCSSRTNSYR 
residuals_values = actual_values - fitted_values

length(fitted_values)
length(residuals_values)

# Visualisation 
ggplot() + geom_point(aes(x = fitted_values, y = residuals_values), color = "blue", alpha = 0.6) + geom_hline(yintercept = 0, linetype = "dashed", color = "red") + labs(title = "Fitted Values vs Residuals for Robust Time Dummy Model",  x = "Fitted Values", y = "Residuals") + theme_minimal()

summary(robust_time_dummy_model)
bptest(robust_time_dummy_model)
jarque.bera.test(residuals_values)

#Box Plot
ggplot(HIKMA_40455377_imputed, aes(x = TimeDummy, y = STKEXCSSRTNSYR)) + geom_boxplot(fill = "lightgreen", color = "black") + labs(title = "Box Plot of Stock Excess Returns by Time Dummy", x = "Time Dummy", y = "Stock Excess Returns") + theme_minimal()


#Methodology Writing 

# Residuals vs. Fitted Values
plot(lm_returnsSYR$fitted.values, lm_returnsSYR$residuals,  main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)

# QQ Plot for Residuals
qqnorm(lm_returnsSYR$residuals, main = "QQ Plot of Residuals")
qqline(lm_returnsSYR$residuals, col = "blue")


#Results 
#1. Initial OLS
# Scatter plot of STKEXCSSRTNSYR vs. INDXEXCSSRTNSYR
png("ScatterPlot_Proportional.png", width = 1200, height = 900, res = 150)

plot(HIKMA_40455377$INDXEXCSSRTNSYR, HIKMA_40455377$STKEXCSSRTNSYR,
     main = "Scatter Plot of STKEXCSSRTNSYR vs. INDXEXCSSRTNSYR",
     xlab = "Market Excess Returns (INDXEXCSSRTNSYR)",
     ylab = "Stock Excess Returns (STKEXCSSRTNSYR)",
     col = "blue", pch = 19,
     cex = 1.2,        
     cex.axis = 1.4,   
     cex.lab = 1.6,    
     cex.main = 1.8)   

abline(lm(HIKMA_40455377$STKEXCSSRTNSYR ~ HIKMA_40455377$INDXEXCSSRTNSYR), col = "red", lwd = 2)
dev.off()

#VIF Dot Plot

dev.off()  
windows(width = 10, height = 7)
vif_values = c(139.2, 2.08, 1.89, 2.58, 132.9, 9.19, 1.16, 2.06)
vif_names = c("USDGBPLOGSYR", "EURGBPLOGSYR", "CHFGBPLOGSYR", "CNYGBPLOGSYR", 
               "SARGBPLOGSYR", "JODGBPLOGSYR", "EGPGBPLOGSYR")
par(mar = c(5, 15, 4, 2))  

dotchart(vif_values,labels = vif_names,main = "Dot Plot of VIF Values",xlab = "VIF Value", pch = 19, col = "red",)
par(mar = c(5, 4, 4, 2) + 0.1)  

png("Dot_Plot_VIF.png", width = 1000, height = 800, res = 150)
par(mar = c(5, 15, 4, 2))

dotchart(vif_values,labels = vif_names, main = "Dot Plot of VIF Values", xlab = "VIF Value", pch = 19, col = "red",cex = 1.2)

dev.off()  

# Residuals vs. Fitted Values Plot
plot(
  model_returnsSYR$fitted.values, 
  model_returnsSYR$residuals,
  main = "Residuals vs Fitted Values",
  xlab = "Fitted Values",
  ylab = "Residuals",
  pch = 19, col = "blue"
)
abline(h = 0, col = "red", lwd = 2)  

# Q-Q Plot of Residuals
qqnorm(model_returnsSYR$residuals, main = "Q-Q Plot of Residuals")
qqline(model_returnsSYR$residuals, col = "red", lwd = 2)

# Histogram of Residuals
hist(model_returnsSYR$residuals,main = "Histogram of Residuals",xlab = "Residuals", col = "skyblue", breaks = 20)

cat("Adjusted R² (Initial Model): ", summary(model_returnsSYR)$adj.r.squared, "\n")


# total sum of squares (TSS) and residual sum of squares (RSS)
TSS = sum((data_clean$STKEXCSSRTNSYR - mean(data_clean$STKEXCSSRTNSYR))^2)
RSS = sum(residuals_values^2)

# Calculate R-squared
R_squared <- 1 - (RSS / TSS)

# Calculate Adjusted R-squared
Adjusted_R_squared <- 1 - ((1 - R_squared) * (n - 1) / (n - p - 1))

# Print the Adjusted R-squared
cat("Adjusted R-squared for Robust Time Dummy Model: ", Adjusted_R_squared, "\n")

# AIC and BIC for the Initial OLS Model
AIC_initial <- AIC(lm_returnsSYR)
BIC_initial <- BIC(lm_returnsSYR)

cat("AIC for Initial OLS Model: ", AIC_initial, "\n")
cat("BIC for Initial OLS Model: ", BIC_initial, "\n")

AIC_robust <- AIC(robust_time_dummy_model)
BIC_robust <- BIC(robust_time_dummy_model)

cat("AIC for Robust Time Dummy Model: ", AIC_robust, "\n")
cat("BIC for Robust Time Dummy Model: ", BIC_robust, "\n")

# Load required libraries
library(car)
library(lmtest)

library(tseries)
library(glmnet)

library(MASS)


summary(lm_returnsSYR)
jarque.bera.test(residuals(lm_returnsSYR))
shapiro.test(residuals(lm_returnsSYR))
bptest(lm_returnsSYR)
dwtest(lm_returnsSYR)

plot(fitted(lm_returnsSYR), residuals(lm_returnsSYR), 
     main = "Residuals vs Fitted (OLS)", 
     xlab = "Fitted Values", ylab = "Residuals", col = "blue")
abline(h = 0, col = "red")


summary(time_dummy_model)

bptest(time_dummy_model)
jarque.bera.test(residuals(time_dummy_model))
shapiro.test(residuals(time_dummy_model))
plot(fitted(time_dummy_model), residuals(time_dummy_model), 
     main = "Residuals vs Fitted (Time Dummy Model)", 
     xlab = "Fitted Values", ylab = "Residuals", col = "blue")
abline(h = 0, col = "red")


# 5. Comparison of Metrics Across Models

ols_mse = mean(residuals(lm_returnsSYR)^2)
ols_rsq = summary(lm_returnsSYR)$r.squared
robust_mse = mean(robust_residuals^2)
time_dummy_mse = mean(residuals(time_dummy_model)^2)
time_dummy_rsq = summary(time_dummy_model)$r.squared

# Compile Results
comparison_results = data.frame(
  Model = c("OLS", "Ridge", "Robust", "Time Dummy"),MSE = c(ols_mse, ridge_mse, robust_mse, time_dummy_mse),
  R2 = c(ols_rsq, ridge_rsq, NA, time_dummy_rsq))
print(comparison_results)



#VISUALS AND METRICS 

library(ggplot2)
library(car)
library(lmtest)
library(sandwich)
library(glmnet)
library(estimatr)



# Libraries
library(ggplot2)
library(lmtest)
library(sandwich)
library(car)


HIKMA_data = HIKMA_40455377_imputed

# Extract key variables
y_actual = HIKMA_data$STKEXCSSRTNSYR

# MSE Comparison
mse_ols = mean(residuals(lm_returnsSYR)^2)
mse_ridge = mean((y_actual - predict(final_ridge_model, as.matrix(scale(HIKMA_data[, -which(names(HIKMA_data) == "STKEXCSSRTNSYR")]))))^2)
mse_poly = mean(residuals(poly_model)^2)
mse_robust = mean(residuals(robust_time_dummy_model)^2)

mse_comparison = data.frame(Model = c("OLS", "Ridge", "Polynomial", "Robust Time Dummy"),MSE = c(mse_ols, mse_ridge, mse_poly, mse_robust))

ggplot(mse_comparison, aes(x = Model, y = MSE, fill = Model)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(title = "MSE Comparison Across Models", y = "Mean Squared Error", x = "Model")


par(mfrow = c(2, 2))

# OLS Residuals
hist(residuals(lm_returnsSYR), breaks = 20, col = "blue", main = "OLS Residuals", xlab = "Residuals")
qqnorm(residuals(lm_returnsSYR), main = "OLS Residuals Q-Q Plot")
qqline(residuals(lm_returnsSYR), col = "red")



# Polynomial Residuals
hist(residuals(poly_model), breaks = 20, col = "green", main = "Polynomial Residuals", xlab = "Residuals")
qqnorm(residuals(poly_model), main = "Polynomial Residuals Q-Q Plot")
qqline(residuals(poly_model), col = "red")

par(mfrow = c(1, 1))



bp_test_robust = bptest(robust_time_dummy_model)
print(bp_test_robust)


reset_robust = resettest(robust_time_dummy_model, power = 2, type = "fitted")
print(reset_robust)


dw_test_robust = dwtest(robust_time_dummy_model)
print(dw_test_robust)



# residuals vs fitted for both models
par(mfrow = c(1, 1))  

# OLS Residuals vs Fitted
plot(fitted(model_returnsSYR), residuals(model_returnsSYR), 
     main = "Residuals vs Fitted: OLS vs Robust Time Dummy", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     col = "red", pch = 16, 
     xlim = c(-10, 10), ylim = c(-20, 20))
abline(h = 0, col = "red", lwd = 2)

# Robust Time Dummy Model residuals
points(fitted(robust_time_dummy_model), residuals(robust_time_dummy_model), 
       col = "blue", pch = 16)
abline(h = 0, col = "blue", lwd = 2)

# Add legend
legend("topright", legend = c("OLS Model", "Robust Time Dummy Model"), 
       col = c("red", "blue"), pch = 16, bty = "n")


library(ggplot2)
library(gridExtra)

#AIC, BIC, and Adjusted R-squared
AIC_initial = 10772.04
BIC_initial = 10836.58
AIC_robust = 10737.59
BIC_robust = 10860.81
adjusted_R2_initial = 0.4964413
adjusted_R2_robust = 0.5055237


MSE_data = data.frame(Model = c("OLS", "Polynomial", "Ridge", "Robust Time Dummy"),MSE = c(3.1, 2.9, 0.2, 0)  )


mse_plot = ggplot(MSE_data, aes(x = Model, y = MSE, fill = Model)) + geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "MSE Comparison Across Models", y = "Mean Squared Error", x = "Model") +
  theme_minimal()

install.packages("gridExtra")
library(gridExtra)

result_table = gridExtra::tableGrob(data.frame(Metric = c("AIC", "BIC", "Adjusted R-squared"), OLS = c(AIC_initial, BIC_initial, adjusted_R2_initial), Robust_Time_Dummy = c(AIC_robust, BIC_robust, adjusted_R2_robust)))

grid.arrange(mse_plot, result_table, ncol = 1, heights = c(2, 1))
install.packages(write)
library(writexl)
write_xlsx(data_clean, "data_clean.xlsx")

write_xlsx(data_clean, "data_clean.xlsx")
getwd()  # Get current working directory


