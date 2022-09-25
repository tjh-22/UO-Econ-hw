## Project EC 421, May 22 2021
## Thomas Hesser (solo)

## Q01 A: Summarize & describe data. Include distribution, rough frequency
## of values, max/min, 3 informative plots

library(pacman)
p_load(tidyverse, here, ggplot2, haven, patchwork, readr, broom, skimr, dplyr)

ps_df = here("~/Documents/EC421_R/proj-data.csv") %>% read_csv()

proj_df <- ps_df

proj_df = read.csv("proj-data.csv")

dim(proj_df)
summary(proj_df)
skim(proj_df)

## Variables chosen: OUTCOME = Personal Income, other variables are Ability, 
## Soc. Econ. Index, *Urban, *Male. '*' indicates binary variables.

## The purpose of this is to see how well the 'Ability' stat predicts income,
## and also how well the Socioeconomic Index predicts it- to measure social 
## mobility among other things such as the general suitability of the 
## 'Ability' stat. As well as how much Ability and Socioeconomic Index are
## correlated- possible further research there to see if Socio. affects 
## Ability or if they are unrelated. General purpose: to see how random the
## income an individual obtains is or how much it is determined by external
## factors. The latter outcome implies low social mobility

## Distr. of data:

## personal_income mean: 6.8754, median: 4.800
## ability mean: 88.0, median: 201.1
## socioeconomic_index mean: 39.71 median: 36.93
## i_urban mean: 0.6122, median: 1.00
## i_male mean: 0.5239, median: 1.00

## This means 'Personal Income' is skewed right slightly by (likely) a  
## small number of high-income people, thus the mean is higher than the median.
## 'Ability', conversely, is skewed  left by low-ability people, so mean is  
## much lower than median (likely by a small number of severely disabled people 
## deviating far from the mean in the opposite direction of the small number of  
## very high-income people). 'Socioeconomic Index' has a lower limit of 0 but 
## the highest observed amount is 80.53, neither of which are unique, suggesting
## hard lower and upper limits. The median is close to the mean, suggesting 
## fairly even distribution to either side. Slightly skewed left by before-
## mentioned poor.'Urban' is a binary variable with a mean of 0.611 but a median 
## of 1. Showing 61.2% of people surveyed live in urban areas. Naturally if a  
## majority of one group is sampled for a binary variable and that group is 
## assigned the value 1, the median will also be 1, which it is also for 'Male',
## which has a mean of 0.524 showing that 52.4% of people surveyed were male.

## Rough frequency of values:
## For binary variables 'Male' and 'Urban', 52.4% and 61.2% are '1', with 45.8%
## and 38.8% correspondingly being '0', respectively, which refers to women and
## rural inhabitants. 'Ability' and 'Personal Income' have only unique min or
## max values and are not integers, so each value is unique- excepting a rare
## coincidence. This is actually not so uncommon with income, especially towards
## the middle (densely populated) part of the distribution. Since many middle-
## class people are paid roughly similar salaries, which tend to come in
## "round" amounts to the bulk of employees of the similar rank from employers
## (e.g. $50k a year, $75k a year etc.) 'Socioeconomic Index' is unique among
## this set of variables by being both non-binary and having large numbers of
## identical values. This implies it was calculated as a result of categorical
## responses to questions or fulfillment of particular criteria, each with a  
## finite number of possible outcomes, among people surveyed for the sample.

count(proj_df, socioeconomic_index)

## This shows frequency of each outcome for Socioeconomic Index, and proves
## they are almost never unique outcomes

## Min/Max:
## personal_income min: 0.0004, max: 130.2000
## ability min: -27979.5, max: 16061.5
## socioeconomic_index min: 0.00 max: 80.53
## i_urban min: 0.00, max: 1.00
## i_male min: 0.00, max: 1.00

## 3 informative plots:

ggplot(proj_df, aes(x = personal_income)) +
  geom_density()

## This shows how personal income is skewed right by the very wealthy

ggplot(proj_df, aes(x = ability)) +
  geom_density()
  
## This shows ability distribution is less biased than 
## personal income but has more variance

ggplot(proj_df, aes(x = socioeconomic_index)) +
  geom_density()

## This shows socioeconomic index is the least uniform distribution, 
## with erratic shape and density peaks all over. Implying that the index
## of the people sampled is a subpar representation of the general population,
## assuming the general population follows a distribution with most people 
## clustered in the center and progressively less density as one deviates
## from the median

ggplot(proj_df, aes(x = ability, y = personal_income)) +
  geom_point() +
  labs(x = "Ability Metric", y = "Annual Income",
       title = "'Ability' Relationship to Income") +  
  geom_smooth()

## Rough indicator of how well the 'Ability' metric gauges for Income 
## Must look into it more closely to see if it's effective, unclear now

## Q02 Regression:

reg1 = lm(personal_income ~ socioeconomic_index+ ability+ i_urban+
            i_male, data = proj_df)

reg1 %>% tidy()
summary(reg1)$coefficient
summary(reg1)
reg1

## Q03 Scatter Plot w. Residuals:

e1 <- resid(reg1)

ggplot(reg1, aes(x = socioeconomic_index, y = e1))+
  geom_point(size = 0.5)+
  labs(x = "Socioeconomic Index", y = "Residuals", 
       title = "Residuals Plot (Soc. Econ. Index)")+
  geom_smooth(aes(x = socioeconomic_index, y = e1))

## Q04:

## It is likely there is Heteroskedasticity. There seems to be a notable
## concentration of large residuals at the min and especially the max possible
## figures for Soc. Index, which cause noticeable warping in the line. 
## On the other hand, the largest residuals are mainly concentrated
## in the center of the plot, which is an encouraging sign, and the line
## does not seem to waver much outside of the fringes.

## Q05: The specification chosen for this particular variable seems to be
## appropriate. The p-value is highly significant (2.2e-16)

## Q06: There could be omitted variable bias affecting the results, which 
## could be controlled for to make a more effective model. 
## One notable thing is 'peak distortion' making it seem like males are likely
## to earn more due to that trait than is the case: A glance at individuals
## with the highest income shows the survey's top earner, an 'outlier' who
## makes $382,000 a year more than the second highest earner, is male. As
## are the next 8 highest earning people. These 9 high earning males become
## progressively less and less separated from each other going down the list
## of highest earners, meaning that the highest earners all being male could
## have a considerable effect on the conclusion due to their significant 
## deviation in income from the vast majority of people surveyed. The top 
## earner makes 4.687 std. deviations more money than the second highest. And
## 15.975 standard deviations more than the median earner- who represents,
## roughly speaking, the most likely person to be recorded in this survey
## and is a fair approximation of a very large portion of the respondents.
## The difference between mean and median of personal income being
## 6.8754 - 4.800 = 2.0754, a very considerable amount of skew
## towards the right, supports this theory. (std. dev. of personal_income
## is 8.15) To control for this one could remove the top 10 highest earners,
## or maybe the top 0.5% overall, then re-run the regression as before and 
## note, presumably, the decline in size of i_male. One way to do a similar 
## thing is to control for *age* in the regression, since 9 of the top 10 are
## over 60 years old, including the outlier.

## Q07:
## Controlling for Physical Disability could increase the effect Socio-
## Economic index has on Income in the model. Since the physically disabled
## are less likely to leverage the benefit of a fortunate background into more 
## earnings due to medical issues keeping them partially or totally out of the
## job market, their inclusion makes Socio-Economic Index appear to have a 
## smaller impact than it generally does on most of the labor force's income.
## It also has a chance of affecting ability due to decreased stamina from 
## medical difficulties, making ability seem less consequential. Possibly an 
## urban environment is also perceived to be more advantageous to income than 
## proper due to better health services and more work opportunities 
## affecting this comparatively small group of people disproportionately.
##
## To fulfill conditions for OVB, i_physical_disability must correlate with
## both the outcome variable Personal Income (which is highly likely and 
## quite easily provable), and an independent variable (which is not as 
## certain, and more difficult to prove). The only independent variable it 
## could reasonably interact with is ability. Ability is still statistically
## significant and impacts personal income: +1 std. dev. Ability nets a 
## projected extra ~$9,310 a year. Compared to +1 std. dev Soc. Econ. which
## gives a projected ~$28,440 additional income per year. 

## Q08: Regression w. new variable i_physical_disability
## First, proof there appears to be a correlation between  
## Disability and Personal income:

ggplot(proj_df, aes(x = i_physical_disability, y = personal_income))+
  geom_point(size = 0.5)+
  labs(x = "Disabled", y = "Income", 
       title = "Disabled Earnings")

skim(proj_df$i_physical_disability)

reg2 = lm(personal_income ~ socioeconomic_index+ ability+ i_urban+
            i_male+ i_physical_disability, data = proj_df)

reg2 %>% tidy()
summary(reg2)$coefficient
summary(reg2)
reg2

## Q09
## There was very little change in any of the coefficients or any aspects of
## the regression otherwise. This makes sense due to the small sample size of
## people with disabilities in the data. Out of 8,000 people, only 187 of
## them were recorded as having a physical disability- a mere 2.34%. Also,
## the std. error of i_physical_disability (0.547) was larger than its 
## estimated negative effect on personal income, which was -0.516 (or -$5,160).
## This effect is indeed sizable but the sample size wasn't large enough 
## to prove anything. 

## Q10: Pick intriguing subset, estimate 3 new models, keep outcome
## variable if prudent, use a log variable and an interaction
## between variables

## First a log-log regression of Personal Income on Education, because I was 
## curious how strong the correlation is between the two by itself, and there
## were problems with residuals when using education and personal_income on
## their own. To do this I needed to add 1 to all the values in the education
## column or else it's not possible to use log (not allowed perform log on  
## values of 0 in the program)

edu1 <- (proj_df$education+1)

logedu <- log(edu1)

reg4 = lm(log(personal_income) ~ logedu, data = proj_df)

e4 <- resid(reg4)

ggplot(reg4, aes(x = logedu, y = e4))+
  geom_point(size = 0.5)+
  labs(x = "Log(Education)", y = "Residuals", 
       title = "Residuals Plot (logpov)")+
  geom_smooth(aes(x = logedu, y = e4))+
  theme_economist()

reg4 %>% tidy()
summary(reg4)$coefficient
summary(reg4)
reg4

## The correlation is significant, meaning that a 1% increase in education
## leads to a 0.816% increase in personal income elastically
## Next regressions are focused on people who graduated college.
## 
## This regression shows Socio Economic Index, a measure of a person's 
## background which has a significant effect on future earnings. The 
## graduated college statistic is not significant in this regression on 
## its own (though it predicts a significant increase in earnings). The 
## interaction variable determines the difference between people with 
## the same socio economic index in terms of annual income. The two 
## statistics that aren't the standalone graduated_college dummy variable
## are extremely statistically significant. 

skim(proj_df, i_grad_college)
count(proj_df, i_grad_college)
count(proj_df, socioeconomic_index)
skim(proj_df, socioeconomic_index)
median(proj_df$socioeconomic_index)

reg3 = lm(personal_income ~ socioeconomic_index+ i_grad_college+ 
            socioeconomic_index:i_grad_college, data = proj_df)

e3 <- resid(reg3)

reg3 %>% tidy()
summary(reg3)$coefficient
summary(reg3)
reg3

ggplot(reg3, aes(x = socioeconomic_index, y = e3))+
  geom_point(size = 0.5)+
  labs(x = "socioeconomic_index", y = "Residuals", 
       title = "Residuals Plot (socioeconomic_index)")+
  geom_smooth(aes(x = socioeconomic_index, y = e3))

## Final regression regresses personal income on socioeconomic index, 
## education, age, and i_male. It is the cleanest of the three and likely
## the best. All of the terms are highly statistically significant and all
##  of them show a substantial positive impact on income. The smallest of 
## these is age, while the largest are socioeconomic index and education. 

reg5 = lm(personal_income ~ + socioeconomic_index+ education+ 
            age+ i_male, data = proj_df)

e5 <- resid(reg5)

reg5 %>% tidy()
summary(reg5)$coefficient
summary(reg5)
reg5

## Q11: 
## I wanted to gauge the level of social mobility currently present and
## also learn about the effects of education on income. The first one 
## was done to figure out how income and education alone were linked. The 
## second was to find out some about 'Ability', the only variable here I did 
## not feel I had a good understanding of. The third was done to use education
## and age  in a regression since I believed they would have the most 
## impact outside of the variables used in earlier problems on personal
## income of the ones to choose from in the data frame

## Q12:
## The last model is easily the best, most statistically significant, and 
## most consistent of them. It also was the only one which most strongly
## confirmed my expectations. The first one needed to be switched over to 
## logarithms and the second one had an insignificant variable in it.

## Q13:
## Socioeconomic coefficient of 0.130 means each additional point in that 
## column correlates with an extra $1,300 a year in personal income. For
## education, 0.659, it is $6,590 for each additional year of education. But 
## education top out at 11 on this data frame, suggesting they might use
## 5th grade as a starting point and not distinguish between undergrad, 
## graduate, and postgraduate education. Age has a coeff. of 0.099, meaning
## That every year from age 24 (youngest) is correlated with $990 more.
## The Male dummy variable coeff. is 3.32, meaning the average male makes
## $33,200 more than the average female. Intercept for this is -9.85, meaning
## a value of 0 for all of these (this is not possible for age, and unfeasible
## for 2 of the others) would extrapolate to -$98,500 salary.

## Q14:
## I generally trust the best, final model. All of the values make sense and
## are statistically highly significant. The coefficients are consistent, as 
## are the t-statistics, p-values, and standard errors. P-values, t-statistics,  
## and standard errors are all very small. T-values are large. It has a 
## reasonable layout and met most of my expectations.

## Q15:
## I would try to find a correlation to life expectancy. Perhaps use variables
## for health insurance, socioeconomic index, poverty pct, income, quality of
## life for graduates and non-graduates and try to account for as many
## omitted environmental differences as possible.


