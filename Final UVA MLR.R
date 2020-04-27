# Multiple Linear Regression

#Use UVA data from Final Cleaning File
dataset = UVA_data

#Splitting the dataset into the Training set and Test set for training/testing
library(caTools)
set.seed(123)
split = sample.split(dataset$Stress, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)



#Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Stress ~ .,
               data = training_set)

#Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)


#Improving models
library(MASS)

#Stepwise selection model
step.model <- stepAIC(regressor, direction = "both", 
                      trace = FALSE)
summary(step.model)

#Forward selection model
forward.model <- stepAIC(regressor, direction = "forward", 
                         trace = FALSE)
summary(forward.model)

#Backward selection model
backward.model <- stepAIC(regressor, direction = "backward", 
                          trace = FALSE)
summary(backward.model)

summary(step.model)$adj.r.squared
summary(forward.model)$adj.r.squared
summary(backward.model)$adj.r.squared






