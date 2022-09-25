## Thomas Hesser PS4 EC 421 Lennon
##
## Q1a

## Treatment effects for each individual: 8, 5, 2, 3, -3, -1, 1, 0

## Q1b

## False

## Q1c

## ATE is mean of Q1A treatment effects. 15/8 = 1.875

## Q1d

## mean y1 - mean y0 = 5.5 - 3.625 = 1.875

## Q1e

## This is an ideal experiment with an imaginary, impossible dataset. 
## Therefore the average treatment effect is estimated perfectly (estimation 
## is identical to the figure calculated the other way in Q1C). Normally,
## since you can only see one of the two possible outcomes, the estimator
## would show the avg. treatment effect plus selection bias for the groups.

## Q1f

## Because there is only a treatment group and a control group. Either someone
## gets the treatment or they do not. There is no option to simultaneously
## give and not give the treatment, thereby getting data for both outcomes 
## from a single individual. So it is impossible to observe the same person 
## over the course of one experiment both with and without treatment. You
## can only have randomized groups of people with different data from each
## category (control & treatment) to compare.

## Q1g

## The fundamental problem of causal inference is that you can only observe
## one variable whilst you are unable to observe the other variable for an
## individual. (ex. if you observe y0, you can't observe y1, & vice versa).
## Since causal effect (treatment effect) is equal to the difference between
## y0 and y1, this cannot be quickly solved with a subtraction of means 
## in real life RCT, it can only be estimated, which includes selection bias.

## Q2a
## The group of people who participated in the job training program is far 
## smaller in size and poorer on average than those who did not. This implies
## the program selects for disadvantaged people, and judging its effectiveness
## by comparing its participants' incomes directly against those of the general
## population is wrong. Because there is selection bias for participants.

library(pacman)
p_load(tidyverse, here, ggplot2, haven, patchwork, readr,
       broom, skimr, dplyr, estimatr)

ps_df = here("~/Documents/EC421_R/ps_dset1.csv") %>% read_csv()

df1 <- ps_df

dim(df1)
summary(df1)
skim(df1)

pretreat_inc <- df1$personal_income_pretreat

ggplot(df1, aes(x = i_trt, y = pretreat_inc))+
  geom_point(size = 0.5)+
  labs(x = "Non-Participants/Participants", y = "Pre-treatment Income", 
       title = "Income comparison between general population and
       participants in a job training program")+
  geom_smooth(aes(x = i_trt, y = pretreat_inc))


## Q2b
## Assuming no causal effects, this regression implies taking part in the job-
## training program has an impact on your expected earnings of -$12,102.79. 
## The coefficient B2 represents the expected impact of participating in the 
## job training program, assuming the other coefficients remain constant.

posttreat_inc <- df1$personal_income_posttreat

reg1 = lm(posttreat_inc ~ education+ i_trt, data = df1)

summary(reg1)

## Q2c
## According to the DAG, the coefficient for i_trt does not represent the 
## causal effect on post-treatment income. This is because income in period 2
## is a collider for both i_trt and income in period one. Unemployment affects
## both i_trt and income in period one, therefore i_trt is not exogenous
## since it is affected by unemployment while the effect of education on
## period two income is also affected by unemployment through the shared
## mediator variable income in period one. One must get rid of both education
## and Inc1 by obtaining a random amount of income for Inc1 (cutting off the 
## path from education to Inc2) and conditioning on Inc1 to leave only the 
## path from unemployment through trt to Inc2 remaining

## Q2d
## No, you could also control for unemployment as they have identical pathways

## Q2e

reg2 = lm(posttreat_inc ~ i_trt + pretreat_inc, data = df1)

summary(reg2)

## Q2f
## We would still need to control for either education or unemployment 
## due to both of them colliding at Inc1

## Q2g
## Knowing what I know about DAG's, pretreat_inc is a collider for both
## unemployment and education. Which means it should not be controlled without
## controlling for one of those, despite providing the most logical
## coefficients when done so. This is because controlling for a collider
## creates an association between its parents (education and unemployment)
## which does not actually exist. A spurious relationship- in actuality
## they are independent of one another (in this diagram).
## Independence is shown by the extremely tiny coefficient for education in 
## the regression below:

reg3 = lm(i_unemployed ~ education, data = df1)

summary(reg3)

## Q3a
## Summary:

ps_df2 = here("~/Documents/EC421_R/instrumental_variables.csv") %>% read_csv()

df02 <- ps_df2

summary(df02)
skim(df02)

## Q3b
## Requirements (2) for a valid instrument: Must be relevant and Exogenous.
## Meaning it must correlate with endogenous variable and be uncorrelated
## with any disturbance.

## Q3c
## If wages is the outcome v and education is the explanatory (endogenous) v,
## then an instrument would need to be correlated w education (relevant) and
## un-correlated w wages (exogenous). I assume n_kids is negatively correlated
## with education and also, unfortunately, likely also negatively correlated 
## with wages as well, which wouldn't make it a valid instrument because it is
## only relevant but not exogenous.

## Q3d
## The coefficient for n_kids is -0.47242, which proves the potential IV is
## relevant/correlated with to education. The coefficient shows that each kid
## is linked to a -0.4742 lower amount of years in education on average. The 
## figure is highly statistically significant due to low P-values and high
## t and F-statistics.

reg4 = lm(education ~ n_kids, data = df02)

summary(reg4)

## Q3e
## 2SLS regression below:

summary(reg4)

df02$eduhat <- reg4$fitted.values

reg5 = lm(wage ~ eduhat, data = df02)

summary(reg5)

## Test:

robust1 <- iv_robust(wage ~ education | n_kids, data = df02) 

summary(robust1)

## Same coefficient value for education (0.333) and intercept (1.7123) as reg5,
## 2SLS done correctly, the errors and t-values etc. are slightly different but
## no drastic changes from the heteroskedasticity-robust estimator

## Q3f
## OLS method:

reg7 = lm(wage ~ n_kids, data = df02)

summary(reg7)

## Coeff from 1st stage: reg4 = -0.4724. Coeff from Reduced: reg7 = -0.1573.
## -0.1573/-0.4724 = 0.333. The normal OLS provided the same number as the
## 2SLS process. Also: as seen below, a straightforward regression of wage on
## education gives coefficient 0.5414, proving that this approach is biased 
## due to Endogeneity.

reg6 = lm(wage ~ education, data = df02)

summary(reg6)

