## In this Multiple regression model I will be 
# exploring the life satisfactory data from 20 people 


#First loading the data to R 
life <- read.csv("LifeSatisfaction.csv")
str(life)

#Loading the package

install.packages(c('car', 'gvlma', 'MASS', 'leaps'))    


# we  want to explore the relationship between life satisfaction and the other
# variables in this data set  including age, gender ,relationship status etc


# - Multiple linear regression

Life_fit <- lm(LifeSatC ~ ., data = life)
summary(Life_fit)






#The model contains all of the variable from the data set 

#The multiple r squared is .78 and the adjusted R-Squared is .63 
#With F-Statistics 5.286 
#The regression equation
#T-value for subject is .460 
#For Age is 
#Plotting the model
Life_fit1 <- lm(LifeSatC ~ ., data = life)
par(mfrow = c(2, 2))
plot(Life_fit1)


## In the Normality Q-Q graph the model does not follow a 45 degree-Line
#We want to see residuals follow a straight line.  We want to 
# see the residuals lined fairly well on the straight dotted line. 

#In the residuals vs fitted graph we don't see any non linear pattern because there is
#no clear curve

#In the scale location we see a random band around a horizontal
# line. We seem to meet this assumption.

## in Resdiduals vs Leverage we can see that there are some
#outliers in the top right corner we will see the results if we exclude them. 



#The graph suggested that there are some models needs to be excluded
#Now I want to exclude 6,12,16,17,18 to improve the model 

NewLifeFit <- lm(LifeSatC ~ ., data = life[-c(6,12,16,17,18),])

plot(NewLifeFit)
summary(NewLifeFit)




##When I excluded the model has been improved we can see the R squared is .91 compared to 
# .78 from the previous model and the F-Statistics is 7.77 compared to 5.286 in the last model. 


## In the Normality Q-Q graph the model still does not follow a 45 degree-Line
#We want to see residuals follow a straight line.  We want to 
# see the residuals lined fairly well on the straight dotted line. 

#In  the Residuals vs Fitted graph We can see the curve now 



## in Resdiduals vs Leverage we can still see some outliers 

# The Global test of linear model assumptions

library(gvlma)
gvmodel <- gvlma(NewLifeFit)
summary(gvmodel)


##All of the assumptipons have been acceptable in 

#Creating the stepwise selection

library(MASS)
NewLifeFit1 <- lm(LifeSatC ~ ., data = life)
stepAIC(NewLifeFit, direction = "backward")


#In this stepwise backward selection we can see that
#The multiple regression equation started with 
#LifeSatC ~ Subject + Age + Gender + Married + IncomeC + HealthC + smoke
#The model has been improved and we can see that variables AIC are going up in 
#the stepwise backward selection 
#Then it worked the way up till that is #least significant is removed. 
#This process continues until no nonsignificant variables remain. and the last
# equation is LifeSatC ~ Gender + Married + IncomeC
# So in conclusion Life Satisfaction can be affected by 
# Your relationship status if you are married or not and your income. 

#Coefficients:
 # (Intercept)          Age       Gender      Married      IncomeC  
#       17.3774      -0.2983       7.9135      11.9511       0.4196
