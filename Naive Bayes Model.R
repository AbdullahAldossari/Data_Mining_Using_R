## Nayve Bayes Assignmnet

USmortality <- read.csv("USMortality.csv")
## I choose the USMortality data set
USmortality

str(USmortality)
##Will creat a column called Mrate to set the mortality that is
#higher than 150 to"High" and lower than 150 to "low" 

data1 <- USmortality
data1$MRate <- ifelse(USmortality$Rate >= 121, "High", "Low")
str(data1)

##converting it to factors
data1$MRate <- as.factor(data1$MRate)
##viewing the new category and checking the structure of the data
str(data1)
##Deleting the following columns SE and Cause 

data1 <- data1[, c(-1,-4,-6)]
str(data1)
## installing packages that I will be using 
install.packages('e1071')
install.packages('caret')


library(e1071)
library(caret)
##divide the data set into training and testing
set.seed(2)

random <- sample(2, nrow(data1), prob = c(0.7, 0.3), replace = T)

data_train <- data1[random == 1, ]
data_test <- data1[random == 2, ]


## running the naivbayes function and all of the variables as independent
## while keebing Mrate to be as the dependent
data_nb <- naiveBayes(MRate ~ . , data = data_train)

data_nb
## The summary After running data_nb we see that :
## There is %14 of mortality rate of the testing the data set is high while %85 of 
# mortality rate of testing the data set is low 
#Under the heading conditional probabilites given the status is Rural we see that 
# the probability of the mortality rate being high is lower than probability of the 
#mortality rate being low.However, given the status is Urban we see that the the probablity
#of mortality rate being high is higher than the mortality being low in Urban. 
##The probability when it comes to Gender in Feamle the probability of being High is higher than
#being low while in Male the probability of being high is lower than high. 


##The variable pred_nb stores the high and low levels of mortality
##creating a confusion matrix
pred_nb <- predict(data_nb, data_test)

confusionMatrix(table(pred_nb, data_test$MRate))

##Accuracy per cent is %73 
## P-value is uch lower than .05 which is a good indication 
##Kappa is %100
# Sensitivity is high 
#  we can say it is a pretty good model

