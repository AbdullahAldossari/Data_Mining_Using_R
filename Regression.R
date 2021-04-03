#------------------------------------------------------------------------#
# Regresssion                                                #
# requires that the car, gvlma, MASS, leaps packages have been installed #
# install.packages(c('car', 'gvlma', 'MASS', 'leaps'))                   #
#------------------------------------------------------------------------#

install.packages(c('car', 'gvlma', 'MASS', 'leaps'))    



# simple linear regression

# The dataset women in the base installation provides the height and weight for a set of
# 15 women ages 30 to 39. We want to be able to predict weight from height. Having an equation
# for predicting weight from height can help us to identify overweight or underweight
# individuals.

str(women)

fit <- lm(weight ~ height, data = women)
summary(fit)

# From the output, we can see that the prediction equation is
# 
# Weight = -87.52 + 3.45 * Height
# 
# The regression coefficient (3.45) is significantly different from zero
# (p < 0.001) and indicates that there's an expected increase of 3.45 pounds of weight
# for every 1 inch increase in height. 
# The multiple R-squared (0.991) indicates that the
# model accounts for 99.1 percent of the variance in weights. 

                                                    .
# The residual standard error (1.53 lbs.) can be thought of as the average error in
# predicting weight from height using this model. 
# 
# The F statistic tests whether the predictor
# variables taken together, predict the response variable above chance levels. 
# Because there's only one predictor variable in simple regression, in this example the F
# test is equivalent to the t-test for the regression coefficient for height.


women$weight
fitted(fit)
residuals(fit)

# scatter plot of height by weight

plot(women$height, women$weight, main = "Women Age 30-39", 
    xlab = "Height (in inches)", ylab = "Weight (in pounds)")
# add the line of best fit
abline(fit)






# The plot suggests we might be able to improve our prediction using
# a regression with a quadratic term,  X2.

# Polynomial regression

# I(height^2) adds a height squared term to the equation.
# The I function treats the contents of the parenthesis as a 
# R expression.

fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)

plot(women$height,women$weight,
     main="Women Age 30-39",
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")
lines(women$height,fitted(fit2))


# From this new analysis, the prediction equation is

#   Weigth = 261.88 - 7.35 X Heigth + 0.083 X Height^2

# and both regression coefficients are significant at the p < 0.0001 level. 
# The amount of variance accounted for has increased to 99.9 percent. 
# The significance of the squared term (t = 13.89, p < .001), and the increase 
# in F suggests that inclusion of the quadratic term improves the
# model fit.


################ Multiple Regression ######################


# In multiple regression we have more than one indpendant variable. 

# If we  want to explore the relationship between a state's murder rate and other
# characteristics of the state, including population, illiteracy rate, 
# average income, and frost levels (mean # number of days below freezing).

# We are using the state.x77 dataset is built into R. 


# Because the lm() function requires a data frame (and the state.x77 dataset is
# contained in a matrix), we can simplify things with this code:


states <- as.data.frame(state.x77[, c("Murder", "Population", 
    "Illiteracy", "Income", "Frost")])
    
cor(states)

library(car)
scatterplotMatrix(states, spread = FALSE, lty.smooth = 2, 
main = "Scatterplot Matrix")


# - Multiple linear regression

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
     data = states)
summary(fit)


# When there is more than one predictor variable, the regression coefficients indicate
# the increase in the dependent variable for a unit change in a predictor variable, holding
# all other predictor variables constant. 

# For example, the regression coefficient for Illiteracy is 4.14, suggesting that an increase 
# of 1 percent in illiteracy is associated with a 4.14 percent increase in the murder rate, 
# controlling for population, income, and temperature. 

# The coefficient is significantly different from zero at the p < .0001
# level. On the other hand, the coefficient for Frost isn't 
# significantly different from zero (p = 0.954) suggesting that Frost and 
# Murder are not linearly related when controlling for the other predictor variables. 
# Taken together, the predictor variables account for 57 percent of the variance in 
# murder rates across states.

# F tells us the strength of the overall model.  If we are improving the model,
# F should go up
# R^2 tells us the percent variation in the dependant variable (murder) that can
# be explained by the predictor variables (or the regression equation).
# The t-stats tell us how significant the individual variables are 
# to the model.  We are looking for both negative and positive
# values above 2 (-2) and 3 (-3).


    
# Multiple linear regression with a significant interaction term

# Suppose we are interested in the impact of automobile weight and horse power on mileage.
# We can fit a regression model that includes both predictors, along with their interaction
# we use a ; between them. 

fit <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(fit)

# Looking at the Pr(>|t|) column we see that the interaction between horse power
# and car weight is significant.

# A significant interaction between two predictor variables tells us
# that the relationship between one predictor and the response variable depends 
# on the level of the other predictor. 

# In this case it means that the relationship between miles per gallon and horse 
# power varies by car weight.



fit <- lm(Murder ~ Population + Illiteracy + Income +
    Frost, data=states)

confint(fit)

# The results suggest that you can be 95 percent confident that the interval [2.38, 5.90]
# contains the true change in murder rate for a 1 percent change in illiteracy rate. Additionally,
# because the confidence interval for Frost contains 0, you can conclude that
# a change in temperature is unrelated to murder rate, holding the other variables constant.
# But your faith in these results is only as strong as the evidence that you have that
# your data satisfies the statistical assumptions underlying the model.


############## Simple Regression Diagnostics ####################

# one way to evaluate the statistical assumptions in regression analysis
# is to plot the results of lm

fit <- lm(weight ~ height, data = women)
par(mfrow = c(2, 2))
plot(fit)
# 
# We can then evaluate the the results based on the assumptions of
# ordinary least squares:
#     
# Normality - The Normal Q-Q plot (upper right) is a probability plot of the standardized
# residuals against the values that would be expected under normality. If
# we have met the normality assumption, the points on this graph should fall on the
# straight 45-degree line.  We want to see residuals follow a straight line.  We want to 
# see the residuals lined fairly well on the straight dotted line. 
# 
# Independence - we can not tell if the dependent variable values are independent
# from these plots. But, there is no reason to believe that one woman's weight influences
# another woman's weight.


# 
# Linearity - In the Residuals versus Fitted graph (upper left).  This plot shows if 
# residuals have non-linear patterns.  Here we see clear evidence of a curved
# relationship, which suggests that we may want to add a quadratic term to the
# regression.
# 
# Scale-Location: Homoscedasticity - This plot will show if residuals  are spread
# evenly along the range of predictors. We are checking the assumption of equal variance. 
# If we have met the constant variance assumption, the points in 
# the Scale-Location graph (bottom left) should be a random band around a horizontal
# line. We seem to meet this assumption.

# Residuals vs. Leverage -  This plot helps us to find influential cases if any exist.  We
# look for outlying values at the upper right or lower right corner.  These spots are 
# where cases can be influence the regression line.  Cases outside  
# the dashed line, Cooke's distance. Cases outside of Cook's dstance (high
# Cook's Distance score) are influencial to the regression results.  Results
# will be altered if we exclude these cases. 



# regression diagnostics for quadratic fit

newfit <- lm(weight ~ height + I(height^2), data = women)
par(mfrow = c(2, 2))
plot(newfit)


# This second set of plots suggests that the polynomial regression provides a better fit
# with regard to the linearity assumption, normality of residuals (except for observations
# 13 and 15), and homoscedasticity (constant residual variance). 
# Dropping observations 13 and 15 produces a better model fit.

# regression diagnostics for quadratic fit 
# with deleted observations

newfit <- lm(weight ~ height + I(height^2), data = women[-c(13, 15),])

plot(newfit)
summary(newfit)

#WARNING:  You need to be careful when deleting data. Your models should fit
# your data, not the other way around!

# We can try diagnostics on our state data

# basic regression diagnostics for states data


fit <- lm(Murder ~ Population + Illiteracy + Income + 
    Frost, data = states)
par(mfrow = c(2, 2))
plot(fit)

#the model assumptions appear to be well satisfied, with the exception that
# Nevada is an outlier.


# Assessing normality
library(car)
states <- as.data.frame(state.x77[, c("Murder", "Population", 
                                      "Illiteracy", "Income", "Frost")])

fit <- lm(Murder ~ Population + Illiteracy + Income + 
    Frost, data = states)
qqPlot(fit, labels = FALSE, simulate = TRUE, main = "Q-Q Plot")

states["Nevada", ]

fitted(fit)["Nevada"]
residuals(fit)["Nevada"]
rstudent(fit)["Nevada"]


# We can see that for Nevada the murder rate is 11.5 percent, 
# but the model predicts a 3.9 percent murder rate.
# Why does Nevada have a higher murder rate than predicted from population, 
# income, illiteracy, and temperature?  Any ideas?

rstudent(fit)

# Studentized residuals quantify how large the residuals are in 
# standard deviation units, and therefore can be easily used to 
# identify outliers: 
# An observation with an internally studentized residual that is larger than 3 
# (in absolute value) is generally deemed an outlier.

# Function for plotting studentized residuals

residplot <- function(fit, nbreaks=10) {
    z <- rstudent(fit)
    hist(z, breaks=nbreaks, freq=FALSE,
    xlab="Studentized Residual",
    main="Distribution of Errors")
    rug(jitter(z), col="brown")
    curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
    lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
    legend("topright",
        legend = c( "Normal Curve", "Kernel Density Curve"),
        lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(fit)


# The errors follow a normal distribution quite well, with the exception of
# a large outlier.


#### Dependence of Errors


# The best way to assess whether the dependent variable values (and
# thus the residuals) are independent is from your knowledge of how 
# the data were collected.
# 
# For example, time series data will often display autocorrelational observations
# collected closer in time will be more correlated with each other than with observations
# distant in time. 
# 
# The car package provides a function for the Durbin-Watson test to
# detect such serially correlated errors.


#  Durbin Watson test for Autocorrelated Errors

durbinWatsonTest(fit)

# The nonsignificant p-value (p=0.282) suggests a lack of autocorrelation, and conversely
# an independence of errors


### Linearity 

# You can look for evidence of nonlinearity in the relationship between the dependent
# variable and the independent variables by using component plus residual plots (also
# known as partial residual plots). 
# 
# The plot is produced by crPlots() function in the car package. 
# We are looking for any systematic departure from the linear model that
# we specified.

library(car)
crPlots(fit)
# Component + Residual Plots

# The component plus residual plots confirm that we have met the linearity assumption.
# The form of the linear model seems to be appropriate for this dataset.

### HOMOSCEDASTICITY
# Is there constant variance in the error over the dataset (over the plot range)?

# The car package also provides two useful functions for identifying non-constant error
# variance.

# The ncvTest() function produces a score test of the hypothesis of constant
# error variance against the alternative that the error variance changes with the level of
# the fitted values. A significant result suggests heteroscedasticity (nonconstant error
# variance).
# The spreadLevelPlot() function creates a scatter plot of the absolute standardized
# residuals versus the fitted values, and superimposes a line of best fit.

# Assessing homoscedasticity

library(car)
ncvTest(fit)

# The score test is not significant (p = 0.19), suggesting we met the constant
# variance assumption. 

spreadLevelPlot(fit)

# We can also see this in the spread-level plot. The
# points form a random horizontal band around a horizontal line of best fit.
# If we violated the assumption, we would see a more sloped line.


# The Global test of linear model assumptions

# The gvla() function performs a global validation of linear 
# model assumptions along with additional infomation. 
# Using the states data:

library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)

# Based on the result of (p = 0.597) the data meet all the statisical 
# assumptions that go with the ordinary least squares regression model 


################## Multicolinearity ####################


# Multicolinearity - When one independent variable is highly correlated with 
# another independent variable (or with a combination of two or more other 
# independent variables), the marginal contribution of that independent variable 
# is influenced by other independent variables. 
# As a result, estimates for regression coefficients can be unreliable.


#  Evaluating multi-collinearity

# Multicollinearity can be detected using the variance inflation factor
# (VIF). For any predictor variable, the square root of the VIF indicates the 
# degree to which the confidence interval for that variable’s regression parameter is expanded
# relative to a model with uncorrelated predictors.
# VIF values are provided by the vif() function in the car package.
# 
# As a general rule, a sqrt >2
# indicates a multicollinearity problem.


vif(fit)
sqrt(vif(fit)) > 2

# No multicolinearity here

# OUTLIERS

# Outliers are observations that are not predicted well by the model, 
# unusually large positive or negative residuals.
# 
# Positive residuals indicate that the model is underestimating the 
# response value, while negative residuals indicate an overestimation.
# 
# Points in the Q-Q plot that lie outside the confidence band are 
# considered outliers.
# 
# A general rule of thumb is that standardized residuals that are larger
# than 2 or less than -2 are should be considered as possible outliers.

############ Assessing outliers  ###############

# The car package also provides a statistical test for outliers: The outlierTest()

library(car)
outlierTest(fit)


# Unusual or High Leverage Observations 


# Index plot of hat values


#  Observations with high leverage are identified through the hat statistic.
# In a dataset, the average hat value is p/n, where p is the number of parameters estimated
# in the model (including the intercept) and n is the sample size. 
# 
# Generallay, an observation with a hat value greater than 2 or 3 
# times the average hat value should be suspect.

hat.plot <- function(fit){
    p <- length(coefficients(fit))
    n <- length(fitted(fit))
    plot(hatvalues(fit), main = "Index Plot of Hat Values")
    abline(h = c(2, 3) * p/n, col = "red", lty = 2)
    identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}

hat.plot(fit)

# Alaska and California are particularly unusual when itcomes to their predictor values. 
# 
# Alaska has a much higher income than other states,while having a lower 
# population and temperature. 
# 
# California has a much higher population than other states, while having 
# a higher income and higher temperature. 
# 
# These states are atypical compared with the other 48 observations.
# High leverage observations may or may not be influential observations. 
# That will depend on whether they are also outliers

################ INFLUENTIAL OBSERVATIONS  ###################

# Influential observations are observations that have a disproportionate impact on the
# values of the model parameters.
# 
# There are two methods for identifying influential observations: Cook's distance,
# or D statistic and added variable plots. 
# 
# Generally, Cook's D values greater than 4/(n-k-1),
# where n is the sample size and k is the number of predictor variables,
# indicate an influential observation.

# Cook's D Plot
# identify D values > 4/(n-k-1)


cutoff <- 4/(nrow(states) - length(fit$coefficients) - 1)
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

# We see Alaska, Hawaii, and Nevada as influencial observations

# Cook's D plots can help identify influential observations, but they do not provide
# information on how these observations affect the model. 

# Added-variable plots are more helpful.  

# Added variable plots
# use the mouse to identify points interactively

avPlots(fit, ask = FALSE, onepage = TRUE, id.method = "identify")

# We can combine the information from outlier, leverage, and influence plots into
# one highly informative plot using the influencePlot() function from the car
# package:

########## Influence Plot ###########


influencePlot(fit, id.method = "identify", main = "Influence Plot", 
    sub = "Circle size is proportial to Cook's Distance")

# Influence plot 
# States above +2 or below -2 on the vertical axis
# are considered outliers. States above 0.2 or 0.3 on the horizontal axis have high
# leverage (unusual combinations of predictor values). Circle size is proportional
# to influence. Observations depicted by large circles may have disproportionate
# influence on the parameters estimates of the model.

# The resulting plot shows that Nevada and Rhode Island are outliers; New
# York, California, Hawaii, and Washington have high leverage; and Nevada, Alaska, and
# Hawaii are influential observations.
# 
# Options for correcting violations:
#     
# There are four approaches to dealing with violations of regression assumptions:
#     Deleting observations -caution
#     Transforming variables - log?
#     Adding or deleting variables
#    Using another regression approach - nonlinear


############## AIC #################


# The Akaike Information Criterion (AIC) is a method for comparing
# models. The index takes into account a model's statistical fit and the number of
# parameters needed to achieve this fit. 

# Models with smaller AIC values indicating adequate fit with fewer parameters are preferred.

# Comparing models with the Akaike Information Criterion

fit1 <- lm(Murder ~ Population + Illiteracy + Income + 
    Frost, data = states)
fit2 <- lm(Murder ~ Population + Illiteracy, data = states)
AIC(fit1, fit2)

# The AIC values suggest that the model without Income and Frost is a better model.
# 
# Comparing two models is relatively straightforward, but what do we do when there
# are a large number of possible models to consider? 
#     
# Two popular approaches for determining a final set of predictor variables 
# from a larger number of possible are stepwise methods and all-subsets regression.


    
################ Stepwise Regression #####################

# In stepwise selection, variables are added to or deleted from a model one at a time,
# until some stopping criterion is reached. 
# 
# For example, in forward stepwise regression
# you add predictor variables to the model one at a time, stopping when the addition 
# of variables would no longer improve the model. 
# 
# In backward stepwise regression, you start with a model that includes all
# predictor variables, and then delete them one at a time until removing variables 
# would degrade the quality of the model. 
# 
# In stepwise , you combine the forward and backward stepwise approaches. 
# 
# Variables are entered one at a time, but at each step, the variables 
# in the model are reevaluated, and those that don’t contribute to the
# model are deleted. 
# 
# A predictor variable may be added to, and deleted from, a model
# several times before a final solution is reached.

############  Backward stepwise selection  ##############

# The stepAIC() function in the MASS package performs
# stepwise model selection (forward, backward, stepwise) using 
# the AIC critera

library(MASS)
fit1 <- lm(Murder ~ Population + Illiteracy + Income + 
    Frost, data = states)
stepAIC(fit, direction = "backward")

# We start with all four predictors in the model. 
# For each step, the AIC column provides the model AIC resulting from the deletion 
# of the variable listed in that row. 
# 
# In the first step, Frost is removed, decreasing the AIC from 97.75 to 95.75. 
# In the second step, Income is removed,decreasing the AIC to 93.76. 
# Deleting any more variables would increase the AIC, so the process stops.
# 
# Stepwise regression is controversial. Although it may find a good model, 
# there’s no guarantee that it will find the best model. 
# This is because not every possible model is evaluated. 
# 
# An approach that attempts to overcome this limitation is all subsets regression.


#####  All subsets regression  #######


# use the mouse to place the legend interactively 
# in the second plot

library(leaps)
leaps <- regsubsets(Murder ~ Population + Illiteracy + 
    Income + Frost, data = states, nbest = 4)
plot(leaps, scale = "adjr2")

# Population, and Illiteracy alone has an adjusted R-square of 0.55. 
# Here you see that a model with fewer predictors has a larger adjusted R-square 
# The graph suggests that the two-predictor model of Population and 
# Illiteracy is the best.
                                                                                                                       

library(car)
subsets(leaps, statistic = "cp", 
    main = "Cp Plot for All Subsets Regression")
abline(1, 1, lty = 2, col = "red")


# We see the best four models for each subset size based on the
# Mallows Cp statistic. 
# 
# Better models will fall close to a line with intercept 1 and slope 1.
# 
# The plot suggests that you consider a two-predictor model with Population and
# Illiteracy; a three-predictor model with Population, Illiteracy, and Frost, or Population,
# Illiteracy and Income (they overlap on the graph and are hard to read); or a fourpredictor
# model with Population, Illiteracy, Income, and Frost. 
# We can reject the other possible models.



