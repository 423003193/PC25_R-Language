# Clear environment
rm(list = ls())

# Header Comments
# Name: Ashley Princess P. Urayanza
# Section: BSIT 4.9
# Date: 2026-10-29
# Purpose: Multiple Regression Model Building for Store Sales

# Load dataset
stores <- read.csv("store_sales.csv")

# Set "No" as baseline for promo
stores$promo <- factor(stores$promo, levels = c("No", "Yes"))

# Inspect structure and summary
str(stores)
summary(stores)

# Check correlation between ad spend and foot traffic
cor(stores$ad_spend_k, stores$foot_traffic)

m1 <- lm(sales_k ~ ad_spend_k, data = stores)
m2 <- lm(sales_k ~ ad_spend_k + foot_traffic, data = stores)
m3 <- lm(sales_k ~ ad_spend_k + foot_traffic + promo, data = stores)

summary(m3)

models <- list(m1 = m1, m2 = m2, m3 = m3)
comp <- data.frame(
  model  = names(models),
  r2     = sapply(models, function(m) summary(m)$r.squared),
  adj_r2 = sapply(models, function(m) summary(m)$adj.r.squared),
  aic    = sapply(models, AIC),
  rse    = sapply(models, sigma)
)
comp[, -1] <- round(comp[, -1], 3)
print(comp)

anova(m1, m2)
anova(m2, m3)


# Print confidence intervals
confint(m3)

# Interpretations in comments:
# 1. ad_spend_k (+1.496): Each additional PHP 1,000 of ad spend is associated with 
#    about PHP 1,500 more sales, holding foot traffic and promo status constant.

# 2. foot_traffic (+0.1235): Each additional 100 visitors is associated with 
#    about PHP 12,350 more sales (100 * 0.1235 thousand PHP), holding ad spend 
#    and promo status constant.

# 3. promoYes (+18.85): Months with a promotion (Yes) generate about PHP 18,850 
#    more sales than non-promotion months (No baseline), holding ad spend and 
#    foot traffic constant.

# 4. Confidence Interval: We are 95% confident that holding traffic and promo constant, 
#    each additional PHP 1,000 of ad spend increases sales between PHP 973 and PHP 2,019.
