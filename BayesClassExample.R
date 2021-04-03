


data <- read.csv("NB_data.csv")


# Data Cleaning

# Our Objective is to predict the income levels of customers based on the other 
# variables. 


str(data)

# We create a column stating the income levels,
# i.e., high and low, according to the income mentioned.
# 
# If a customer has income more than 35000,
# then we keep him in the "high" slot, otherwise we set him "low".


data1 <- data

# We do this with the ifelse function.  If income is > 3500
# the customer is classified high, otherwise he is in the low category
# all this is put in a new category "inc" in data1.

data1$inc <- ifelse(data$Income >= 35000, "High", "Low")

# View the new category:

str(data1)



# Now, we remove the 9th variable (Income) 


data1 <- data1[, -9]


# A few variables that we believe might be irrelevant with regards to this model should be removed. 
# These may include, "Customer", "Gender" and "Marital Status". 
# These variables have no direct connections on determining the income of the customers.


data1 <- data1[, c(-1,-5,-7)]


# Checking the structure of the variables:


str(data1)

# We see there are 9134 records and 6 variables structured in a data frame. 
# The variable "inc" is the char data type, That is a problem. 
# As there are only two levels in this variable, high and low, 
# we have to convert it into factor data type.


data1$inc <- as.factor(data1$inc)



# Naive Bayes Classifier Model
# Installing the libraries,


install.packages('e1071')
install.packages('caret')


library(e1071)
library(caret)



# Divide the dataset into training and testing sets, keeping the ratio as 7:3,


set.seed(2)
# for consistency

random <- sample(2, nrow(data1), prob = c(0.7, 0.3), replace = T)

data_train <- data1[random == 1, ]
data_test <- data1[random == 2, ]



# Running the naiveBayes() function. Keeping "inc" as the dependent 
# variable and considering all the other 5 variables as independent variables 
# (indicated with "." sign). Running the model on the training set first.


data_nb <- naiveBayes(inc ~ . , data = data_train)

data_nb




# Running "data_nb" we get to see the summary of the model run. We read it as:
# 
# Under the heading "A-priori probabilities", we see that there is 49% chance of 
# income of the testing dataset customers being low. Similarly 51% chance of income 
# of the testing dataset customers being high.
# 
# Under the heading "Conditional probabilities", we get the conditional probabilities 
# of all the variables individually.
# If the State is "Arizona", the probability of the income being high is more than the 
# probability of the income being low. Similarly, if the state is "California", the 
# probability of the income being low is more than the probability of the income is high. 
# We read the rest in this manner.
# 
# Next, if the Education is "Bachelor", the probability of the income being low is 
# more than the probability of the income is high. Compared to "Master", the probability 
# of the income being high is much more than the probability of the income is low, which is logical.
# 
# We can read the other observations in the same way.
# 
# Now we run the model on the test data and get the predictions,

pred_nb <- predict(data_nb, data_test)

# The variable "pred_nb" stores the high and low levels corresponding to all the records. 
# To read it properly let's create a confusion matrix out of it,


confusionMatrix(table(pred_nb, data_test$inc))


# The accuracy is 87%.
# 
# Validation Observations
# The diagonal values are the number of correct predictions and the off-diagonals 
# are considered a number of wrong predictions. So we see that there are much 
# lower wrong predictions (352 + 0) compared to the correct predictions (1382 + 1047).
# 
# Accuracy per cent is high (87%) which is a good indication.
# P-value is much lower than 0.05 (<2.2e-16), which is desired.
# 
# Kappa statistic is also high (around 75%). This indicates that there is a huge 
# difference between the actual accuracy and the random accuracy.
# 
# Sensitivity is high, but Specificity is indicative of the false negatives
# (352 high incomes classified as low).  
# Hence with all these observations, we can say it is a pretty good model.

# Insights
# 1. For State, customers living in "Arizona", "Nevada", "Oregon", "Washington" has probabilities 
# of high income. Those living in "California" has the opposite result. 
# But we see that the variance among these two levels for all the States is lower.
# 
# 2. For Education, customers who hold the degrees of "Bachelor", "Doctor" and
# "High School or Below" has probabilities of income being low than being high. 
# But those holding "Master" shows the opposite insight. Also, "College" people
# have an income level of a standard. 
# 
# 3. For Employment Status, customers who are "Disabled", "Retired", "Unemployed" and 
# also who is on "Medical Leave", have low income, and their probability of getting a 
# high income is exactly 0. Extremely opposite is the case of "Employed" customers. 
# Their probability to get high income is perfectly 1. 
# 
# 4. For Location, customers living in "Rural" and "Urban" has the much much higher 
# probability of income being high than being low. On the other hand, the ones living 
# in "Suburban" has the opposite result. There the probability of income being low is 
# more than being high. 
# 
# 5. For Vehicle, customers having "Four-Door Car", "Luxury Car" and "Two-Door Car" has 
# a higher probability of income being high than being low. Customers having "Luxury SUV", 
# "Sports Car" and "SUV" has just the opposite result. But we see that the variance among 
# these two levels for all the Vehicles is less.