#' ---
#' title: Computer Poject 2
#' author: Carmen Abans
#' ---

#' (a) Use the read.csv command to read the CPS12.csv data set into R. Use the attach command to attach the data set into R.
cps <- read.csv("CPS12.csv")
attach(cps)

#' (b) Print out an summary of the data set. In particular, find and report the sample average of the variables ahe, age, female and bachelor, respectively.
summary(cps)      # The summary shows: Min.    1st Qu.   Median    Mean    3rd Qu.    Max
summary(ahe)
summary(age)
summary(female)
summary(bachelor)

#' (c) Run a regression (model1) of average hourly earnings, ahe on age, female and bachelor. Find and use a sentence to interpret the meaning of the regression coefficients.
model1 <- lm(ahe ~ age + female + bachelor)
summary(model1)
# We have got the following regressors:
# Estimated Intercept (β0): 0.08186   Intercept (when every variable is 0)
# Estimated age       (β1): 0.45299   This means that when the worker's age increases by 1 year the ahe will increase by $0.45299.
# Estimated female    (β2): -2.80451  This means that women earn $2.80 less than men, on average.
# Estimated bachelor  (β3): 6.93600   This means that workers with bachelor degrees earn $6.93600 more, on average than workers without them.

# We have also gotten the standard error of the regressors (the average distance that the observed values deviate from the regression line):
# Std. Error age        0.02331 
# Std. Error female     0.13395 
# Std. Error bachelor   0.13375 
# Std. Error intercept  0.08186

# We have also obtained the t-values (t-value=coefficient divided by its standard error) of each variable:
# t value age       19.435
# t value female    -20.937
# t value bachelor  51.858

# We also have the p-values of each variable:
# Pr(>|t|) age      <2e-16
# Pr(>|t|) female   <2e-16
# Pr(>|t|) bachelor <2e-16

#' (d) Abby is a 28-years-old, female worker with a bachelor degree. Based on the regression from (model1), predict her average hourly earnings.
0.08186 + 0.45299*28 - 2.80451*1 + 6.93600*1

#' (e) Brian is a 32-years-old, male worker without a bachelor degree. Based on the regression from (model1), predict his average hourly earnings. 
0.08186 + 0.45299*32 - 2.80451*0 + 6.93600*0

#' (f) Cindy is a 25-years-old, female worker without a bachelor degree. Based on the regression from (model1), predict her average hourly earnings.
0.08186 + 0.45299*25 - 2.80451*1 + 6.93600*0

#' (g) Find the SER, R2 and the adjusted R2 from the regression from (model1). Why are R2 and the adjusted R2 similar to each other in this regression?
summary(model1)
# When we did the summary of model1 we got:
#   a residual standard error (SER) of $8.164
#   a R2 of 0.1764 
#   an adjusted R2 of 0.1763
# R2 and the adjusted R2 are similar to each other because the sample size is large (n=15312).

#' (h) Run a regression (model2) of average hourly earnings, ahe on age only.
model2 <- lm(ahe ~ age)
summary(model2)

#' (i) Perform an F-test to compare (model1) and (model2). You may use the (anova) command.
anova(model1, model2)
# Since p-value = (< 2.2e-16) which is smaller than 0.05, we reject H0 (H0: β(female)=β(bachelor)=0). This means that at least one of the β is not zero (they are important)
# Comparing to model1, model2 suffers from the omitted variable bias because when there are more variables the coefficients change (there are significant variables omitted).

#' (j) Based on the F-test result, do we prefer (model1) or (model2)?
# We prefer model1 because it has significant variables that have been omitted in model2. 
# Also, we can see that model1 is better than model2 because it's SER is smaller and it's adjusted R2 is larger.

#' (k) Run a regression (model3) of the logarithm of average hourly earnings, log(ahe) on age, age^2, female and bachelor.
lahe <- log(ahe)
model3 <- lm(lahe ~ age + I(age^2) + female + bachelor)
summary(model3)

#' (l) Run a regression (model4) of the logarithm of average hourly earnings, log(ahe) on age, age^2, female, female∗age, female∗age^2, and bachelor.
model4 <- lm(lahe ~ age + I(age^2) + female + I(female*age) + I(female*age^2) + bachelor)
summary(model4)

#' (m) Perform an F-test to compare (model3) and (model4). You may use the (anova) command.
anova(model3, model4)
# Since p-value = 2.321e-08 which is smaller than 0.05, we reject H0 (H0: β(female*age)=β3(female*age^2)=0) that means that at least one of them is not 0.
# Comparing to model4, model3 suffers from the omitted variable bias because when there are more variables the coefficients change (there are significant variables omitted).

#' (n) Based on the F-test result, do we prefer (model3) or (model4)?
# We prefer model4 because it has significant variables that have been omitted in model3. 
# Also, we can see that model4 is better than model3 because it's SER is smaller (M4=0.5052<M3=0.5057) and it's adjusted R2 is larger (M4=0.1847>M3=0.1829).


