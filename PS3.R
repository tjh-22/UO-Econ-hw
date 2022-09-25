## Thomas Hesser 
## EC 421 Lennon
## PS3

library(pacman)

p_load(here, ggplot2, haven, patchwork, readr, broom, skimr, dplyr,
       future, furrr, parallel, foreach, tidyverse, lfe, robustbase,
       ggthemes)

ps_df = read.csv("~/Documents/EC421_R/ps03_df.csv")

ps3_df <- ps_df

ps3_df <- na.omit()

dim(ps3_df)
summary(ps3_df)
skim(ps3_df)

## Q1a
## If this model is true, the OLS estim for this model must be consistent
## (requires contemporaneous exogeneity-- Ut uncorrelated in the same period). 
## An inconsistent OLS estim B1(hat) won't ever converge with the true B1(pop)
## even if n reaches infinity. TRUE model must be unbiased and consistent

## Q1b
## The estimated coefficient for B1 (price_prop) is 1.656 for the standard
## regression below. This means a 1 unit increase in propane price (in $ per 
## gallon) correlates with a 1.6561 increase in gas price (in $ per 1MM BTU)
## Robust reg shows this as well with less standard errors. In both cases the
## 1.6561 is of max statistical significance, but in the Robust regression,  
## the intercept goes from 2 to 3 stars (0 to 0.001) significance.

reg1 = lm(price_gas ~ price_prop, data = ps3_df)

reg_robust=felm(price_gas ~ price_prop, data = ps3_df)

summary(reg1)
summary(reg_robust, robust=T)

## Q1C
## Multiple R-squared is 0.267 and adjusted R squared is 0.263. This means
## the correlation does not have very good fit despite its statistical 
## significance (the data does not adhere closely to the line on average). So
## the price of propane is loosely correlated with gas price.

summary(reg1)

## Q1D
## The estimated coeff for B1 (price_prop) is now 2.4935 instead of 1.6565,
## meaning a 1 unit increase in propane price leads to a 2.4935 unit increase
## in gas price in the same time period. The same increase in propane price
## 1 and 2 months earlier leads to 1.5643 and -2.3631 price increases for gas,
## respectively. 

summary(ps3_df$price_gas)
summary(ps3_df$price_oil)
summary(ps3_df$price_prop)
summary(ps3_df$t_month)
skim(ps3_df$t_month)

reg2 <- lm(price_gas ~ price_oil+ price_prop+ lag(price_prop)+ 
                lag(price_prop, 2), data = ps3_df)

summary(reg2)

## Q1e
## A 1 unit increase in oil price (in $ per barrel) leads to a 0.0968
## price decrease in gas. However, the oil and one-month-ago propane
## coefficients in this regression are not statistically significant at all,
## meaning they are unlikely to be correct. The price of oil being inversely
## linked with that of gas is illogical.

## Q1f
## R^squared for the second regression is 0.2959, higher than the 0.267 of the
## first one. Adjusted R^squared changed to 0.2801 from 0.263. The standard
## errors were proportionally larger to all the variables in the second regr.
## except for the intercept, which was similar. T-values were far lower in the
## second regr, indicating more variance

## Q1g: F-test

p_load(lmtest)

waldtest(reg1, c("price_prop"), test = "F")

waldtest(reg2, c("price_oil", "price_prop", "lag(price_prop)", 
                   "lag(price_prop, 2)"), test = "F")

## F of reg1 waldtest: 66.663
## F of reg2 waldtest: 18.7
## Both had maximum possible level of significance

## Q1h
## For either model we can expect OLS to be unbiased for the coefficients
## even in the presence of auto-correlation. This is because neither of the
## models have lagged outcome variables, so OLS is merely inefficient and 
## liable to generate biased standard error estimates due to auto-correlation.

## Q1i
## The values of B1 and B3 are 3.118 and -1.329. Compared to reg2, the value
## of the coefficient before the non-lagged propane price variable (3.118) is 
## notably higher than the 2.49 it was previously was. The size of a standard
## error for that coefficient shrunk from 1.01 to 0.41 and it achieved max
## statistical significance.

reg3 <- lm(price_gas ~ price_prop+ lag(price_prop)+ 
             lag(price_prop, 2)+ lag(price_gas), data = ps3_df)

tidy(reg3)
summary(reg3)

## Q1j
## R^squared value of reg 3 is 0.8574, adjusted is 0.8542. Both are huge 
## increases over the previous R^squared values of 0.2959 with 0.2801 adjusted.
## This implies the data averages much closer to the line, which is a much  
## better fit compared to previous ones

summary(reg2)

## Q1k
## OLS will certainly produce consistent estimates for the B's in the 3rd
## model if it is not auto-correlated and the errors have contemporary 
## exogeneity. OLS is biased in regressions with lagged outcome variables
## such as this one due to 't' disturbances violating exogeneity. However, 
## it is still consistent as long as contemporary exogeneity exists and 
## will still provide consistent coefficient estimates

## Q2a
## How will auto-correlation affect the 3 models?
## Auto-correlation (disturbances correlating over time) affects OLS
## in all 3 models if present. In the first 2, a static model and a 
## dynamic model with only lagged explanatory variables, auto-correlation
## causes OLS to be inefficient and give biased standard error estimates,
## while remaining unbiased for coefficients. In the last one, which is a
## dynamic model with lagged outcome variables, auto-correlation causes OLS
## to be both biased and inconsistent for coefficients. If contemporary 
## exogeneity exists it is still consistent but auto-correlation violates
## that principle.

## Q2b

ps3_df$e <- c(NA, NA, residuals(reg3))

## Q2c
## What to look for when trying to find auto-correlation in a graph?
## Evidence of auto-correlation in a residuals plot is 'random walk'
## path of the line or particularly strong warping in one area of the plot.
## The plots of the last regression show little sign of auto-correlation
## as the lines are fairly even and do not warp near the edges or move much.

edit_df <- na.omit(ps3_df)

e3 <- edit_df$e

ggplot(edit_df, aes(x = t_month, y = e3))+
  geom_point(size = 0.5)+
  labs(x = "Month", y = "Residuals", 
       title = "Residuals Plot (Month)")+
  geom_smooth(aes(x = t_month, y = e3))

ggplot(edit_df, aes(x = lag(e3), y = e3))+
  geom_point(size = 0.5)+
  labs(x = "Lag", y = "Residuals", 
       title = "Residuals Plot (Lag)")+
  geom_smooth(aes(x = lag(e3), y = e3))

## Q2d

ps3_df$e2 <- c(NA, NA, residuals(reg2))

edit_df2 <- na.omit(ps3_df)

em2 <- edit_df2$e2

## Q2e 
## These graphs look to have much more auto-correlation than those of the 
## third model. The first one has extremely scattered data in one area of
## the graph - the center - showing a high amount of disturbances during
## those particular months. While the lag plot line is sloping upward
## and shows more variance as it gets closer to the right end of the graph,
## unlike the other lag plot which had most of the prominent residuals in
## the center yet largely had a flat line with consistent slope going through.

ggplot(edit_df2, aes(x = t_month, y = em2))+
  geom_point(size = 0.5)+
  labs(x = "Month", y = "Residuals", 
       title = "Residuals Plot (Month)")+
  geom_smooth(aes(x = t_month, y = em2))

ggplot(edit_df, aes(x = lag(em2), y = em2))+
  geom_point(size = 0.5)+
  labs(x = "Lag", y = "Residuals", 
       title = "Residuals Plot (Lag)")+
  geom_smooth(aes(x = lag(em2), y = em2))

## Q2f
## Why signs of auto-correlation in last pair of graphs but not in first?
## This is likely because the 

## Q2g
## Test for 2nd order auto-correlation using steps in slides for 3rd model.
## Breusch-Godfrey test: p-value is 0.1593, cannot fail to reject "no 
## auto-correlation"

e3

reg4 <- lm(e3 ~ price_prop+ lag(price_prop)+ 
     lag(price_prop, 2)+ lag(price_gas)+ lag(e3)+
       lag(e3, 2)+ lag(e3, 3)+ lag(e3, 4), data = edit_df)

tidy(reg3)
summary(reg3)

waldtest(reg4, c("lag(e3)", "lag(e3, 2)", "lag(e3, 3)", "lag(e3, 4)"))

## Q2h
## Assuming Ut is not auto-correlated, OLS estimates will provide consistent
## coefficient estimates for the third model. Since even a model with
## lagged outcome variables can produce consistent estimates with OLS, 
## providing contemporaneous exogeneity is not violated (covariance = 0).



