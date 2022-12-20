##read the csv
insurance = read.csv(file = 'insurance.csv')

insurance$sex = ifelse(insurance$sex == 'female', 0, 1)
insurance$smoker = ifelse(insurance$smoker == 'no', 0, 1)

##Extracting the number of parameter combinations
n = dim(insurance)[1]

##making train test split
train_index = sample(1:n, round(0.8*n))
train = insurance[train_index, ]
test = insurance[-train_index, ]

##defining #of trees
n_tree = c(100,300,500,800,1000,1500,2000)

## combining trees
parameters = expand.grid('number_of_trees' = n_tree)
parameters$RMSE = NA
parameters$MAE = NA

##Extracting the number of parameter combinations
n = dim(parameters)[1]

library(randomForest)

for(i in 1:n){
  ## build random forest
  RF = randomForest(charges ~ age + sex + bmi + children + smoker + region, data = train, n.trees = parameters$n_tree[i])
  
  ## predict on test data
  RF_pred = predict(RF, test, n.trees = parameters$n_tree[i], type = 'response')
  
  ## store results
  parameters$RMSE[i] = sqrt(mean((RF_pred - test$charges)^2))
  parameters$MAE[i] = mean(abs(RF_pred - test$charges))
}
parameters

##Defining the depth of the trees
n_depth = c(2, 3, 4, 5, 6)

##Combining the trees and depths
parameters = expand.grid('Number_of_Trees' = n_tree, 'Depth' = n_depth)
parameters$RMSE = NA
parameters$MAE = NA

##Extracting the number of parameter combinations
n = dim(parameters)[1]

library(gbm)
library(caret)

##changing region to a factor for the GBM model
train$region <- factor(train$region)

for(i in 1:n){
  
  ##Building the gradient boosting model
  GBM = gbm(charges ~ age + sex + bmi + children + smoker + region, data = train, n.trees = parameters$Number_of_Trees[i], interaction.depth = parameters$Depth[i], distribution = 'gaussian')
  
  ##Predicting on the test data set
  GBM_pred = predict(GBM, test, n_trees = parameters$Number_of_Trees[i], type = 'response')
  
  ##Storing results
  parameters$RMSE[i] = sqrt(mean((GBM_pred - test$charges)^2))
  parameters$MAE[i] = mean(abs(GBM_pred - test$charges))                          
  
}

##sorting RMSE MAE
parameters = parameters[order(parameters$RMSE, parameters$MAE, decreasing = FALSE), ]

parameters

##based on the results i would use the gradient boosting model with 100 trees and a depth of 2 because it has the lowest RMSE and MAE of all other
## boosting models. when compared to the random forest model its RMSE is about 419 lower and its MAE is 433 lower than the best Random Forest model
## making the gradient boosting model with 100 trees and depth 2 the best choice for predicting insurance charges















