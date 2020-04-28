#Uses Dartmouth data to predict Selfworth scores of users

dataset = masterkey_selfworth
dataset$user <- NULL
dataset$openToNewExperiences_complex_15min <- NULL
dataset$reserved_quiet_15min <- NULL
dataset$convential_uncreative_15min <- NULL
dataset$awakeness_in_class <- NULL

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$selfWorth, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)



# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = selfWorth ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
summary(regressor)

library(MASS)
step.model <- stepAIC(regressor, direction = "both", 
                      trace = FALSE)
summary(step.model)

forward.model <- stepAIC(regressor, direction = "forward", 
                         trace = FALSE)
summary(forward.model)

backward.model <- stepAIC(regressor, direction = "backward", 
                          trace = FALSE)
summary(backward.model)

summary(step.model)$r.squared
summary(forward.model)$r.squared
summary(backward.model)$r.squared
