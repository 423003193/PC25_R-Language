# Header Information
# Name: Ashley Princess P. Urayanza
# Section: BSIT 4.9
# Date: 2026-10-30
# Purpose: Week 11 Residual Plot Analysis & Multicollinearity Audit


library(car); library(lmtest); library(sandwich)
loans <- read.csv("loan_scores.csv")
str(loans)

fit <- lm(score ~ income_k + spend_k + years_employed + zone2, data = loans)
summary(fit)

# Render 2x2 diagnostic plot window
par(mfrow = c(2, 2))
plot(fit)
par(mfrow = c(1, 1))

# Diagnostic Plot Comments:
# 1. Residuals vs Fitted: Shows a widening funnel shape, indicating non-constant variance (heteroscedasticity).
# 2. Normal Q-Q: Points closely follow the straight line, indicating normal residuals.
# 3. Scale-Location: Shows an upward trend, confirming non-constant variance.
# 4. Residuals vs Leverage: No influential points exceed Cook's distance threshold of 1.0 (max is ~0.071).

shapiro.test(resid(fit))
bptest(fit)
vif_tab <- data.frame(vif = round(vif(fit), 2))
vif_tab
max(cooks.distance(fit))

# Assumption Audit Comments:
# - Normality: MET (Shapiro-Wilk W = 0.993, p = 0.68).
# - Constant Variance: VIOLATED (Breusch-Pagan BP = 16.09, p = 0.0029; funnel plot).
# - Multicollinearity: VIOLATED (income_k VIF = 18.14 and spend_k VIF = 18.11 exceed 10).
# - Influential Points: MET (Max Cook's distance = 0.071 is far below 1.0).

# Check correlation between income and spend
cor(loans$income_k, loans$spend_k)

# Remedy 1: Drop spend_k and refit model
fit2 <- lm(score ~ income_k + years_employed + zone2, data = loans)
summary(fit2)
vif(fit2)
bptest(fit2)

# Remedy 2: Apply robust standard errors (HC3) for unequal variance
coeftest(fit2, vcov = vcovHC(fit2, type = "HC3"))

# Remedy Audit Comments:
# - Dropping spend_k eliminated multicollinearity (all VIFs ~1.0; income std error dropped from 0.84 to 0.20).
# - Breusch-Pagan still shows unequal variance (BP = 15.13, p = 0.0017), so robust standard errors (HC3) are applied for valid inference.


tapply(resid(fit), loans$group, mean)
table(loans$group, loans$zone2)
cor(loans$zone2, loans$group == "B")
tapply(loans$score, loans$group, mean)

# Bias Audit Comments:
# - Residuals: Model over-predicts for Group A (+5.85) and under-predicts for Group B (-8.53).
# - Proxy Analysis: zone2 correlates strongly with Group B (r = 0.63; 49 of 61 Group B applicants live in Zone 2).
# - Harm: The ~36 point penalty on zone2 unfairly disadvantages Group B applicants.











