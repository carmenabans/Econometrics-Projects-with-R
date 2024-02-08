#' ---
#' title: Computer Project 1
#' author: Carmen Abans
#' ---


#' a) Use the read.cvs command to read the Earnings_and_Height.cvs data set into R. Use the attach command to attach the data set into R.
eah <- read.csv("Earnings_and_Height.csv")
attach(eah)


#' b) Print out an summary of the data set. In particular, find and report the sample average of the variables earnings, height and sex, respectively.
summary(eah)      # The summary shows the following information about the variables in eah data set: Min., 1st Qu., Median, Mean, 3rd Qu., Max
summary(earnings) 
summary(height)       
summary(sex)      # The sex is a dummy variable where: 1=Male, 0=Female

#' c) Run a regression of earnings on height. In particular, find and use a sentence to interpret the meaning of the regression coefficient of the variables height.
# Regression
ols <- lm(earnings ~ height)
summary(ols)
# We have got that the height has this coefficients:

#   Estimate    707.67      (β1) This means that when the height increases by 1 (one inch taller) the earnings increase by $707.67.
#                           the earnings increase by $707.67.

#   Std. Error  50.49       (standard error of β1) This means that the average distance that the observed values
#                           deviate from the regression line is 50.49 
#                           (The smaller the value, the closer our values are to the regression line)

#   t value     14.016      This is the coefficient divided by its standard error

#   Pr(>|t|)    <2e-16 ***  p-value

#' d) Plot a graph of earnings over height. e) On the graph, add a fitted line of the regression.
plot(earnings ~ height)     # Graph
abline(ols)                 # Fitted line

#' f) Suppose Alex is 65 inches; Bob is 67 inches; Chris is 70 inches tall. Based on the regression, predict their corresponding earnings.
# Alex (Method 1)
-512.73 + 707.67*65
# Bob (Method 2)
ols$coefficient[1] + ols$coefficient[2] * 67
# Chris(Method 3)
predict(ols, data.frame(height=70))

#' g) Find the R2 and SER from the regression in part (c). Use a sentence to interpret each of them.
summary(ols)
#     When we did the summary of the ols we got a residual standard error (SER) of $26780 and it is the measure
#     of the spread of the error term u.

#     We also got the R2 which it appears to be 0.01088. This means that approximately 1.08% of earnings are 
#     explained by the height.

#' h) Based on the regression in part (c), find the p-value of the variables height and perform a t-test.
summary(ols)
#     The summary of the ols shows that our p-value is smaller than 2.2e-16. 
#     If we do the t-test based on the p-value we get that the absolute value of the p-value is smaller than 1.96 
#     For that reason we reject the null (H0: β1=0). 
#     That means that β1!=0 so there is a relationship between height and earnings.

#' i) Based on the regression in part (c), use the confint command to calculate the Confidence Interval (CI) of the variables height. Does your CI give you the same t-test conclusion?
confint(ols)
# We've got that the CI = [608.7078, 806.6353]. Since 0 is out of the CI we also end up rejecting the null.
# This is to be expected as all three t-test methods are equivalent.

#' j) Run a regression of earnings on sex. For both the regression intercept and coefficient of the variables sex, use a sentence to interpret its meaning.
ols2<- lm(earnings ~ sex)
summary(ols2)
# We've got that Earnings = 45621 + 2838.8 x Male (as the sex variables are 1=Male, 0=Female).
# This means that men earn on average $2838.8 more than women.
# We can also see that the mean earnings of women is $45621.
# And if we want to know the mean earnings of men we just need to set the Male=1 which results in:
45621.0+2838.8
