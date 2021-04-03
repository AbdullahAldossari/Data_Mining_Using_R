

#####  Neural Networks Example  -------------------

##### Neural Networks #########-------------------
## Example: Modeling the Strength of Concrete  ----
# What mixture of the concrete ingredients will result 
# in the strongest concrete?

## Exploring and preparing the data ----
# read in data and examine structure
concrete <- read.csv("concrete.csv")
str(concrete)

# Note the output is a numerical variable

# custom normalization function
normalize <- function(x) { 
    return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete, normalize))

# confirm that the range is now between zero and one
summary(concrete_norm$strength)

# compared to the original minimum and maximum
summary(concrete$strength)

# create training and test data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

## Step 3: Training a model on the data ----
# train the neuralnet model
install.packages("neuralnet")
library(neuralnet)

# simple ANN with only a single hidden neuron
set.seed(12345) # to guarantee repeatable results
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                                ash + water + superplastic + 
                                coarseagg + fineagg + age,
                            data = concrete_train)

# visualize the network topology
plot(concrete_model)

## Evaluating model performance ----
# Compute()function returns $neurons which stores the neurons for each layer in
# the network, and $net.result which stores the predicted values
# obtain model results

model_results <- compute(concrete_model, concrete_test[1:8])

# obtain predicted strength values

predicted_strength <- model_results$net.result

# This is a numeric prediction, so we cannot use a confusion matrix to
# look at accuracy.  We must measure the corellation between the 
# predicted concrete strength and the true value
# examine the correlation between predicted and actual values

cor(predicted_strength, concrete_test$strength)

## Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
set.seed(12345) # to guarantee repeatable results
concrete_model2 <- neuralnet(strength ~ cement + slag +
                                 ash + water + superplastic + 
                                 coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)

# plot the network
plot(concrete_model2)


# evaluate the results as we did before
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

# Obviously an improved accuracy

