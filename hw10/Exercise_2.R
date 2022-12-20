##Reading csv file
ames_houses = read.csv(file = 'Ames_Housing_Data.csv')
head(ames_houses)

##spliting the data
n = dim(ames_houses)[1]

train_index = sample(1:n, round(0.8*n))
train = ames_houses[train_index, ]
test = ames_houses[-train_index, ]


##Defining the number of trees
n_tree = c(100, 300, 500, 800, 1000, 1500, 2000)

parameters = expand.grid('Number_of_Trees' = n_tree)
parameters$RMSE = NA
parameters$MAE = NA

##Extracting the number of parameter combinations
n = dim(parameters)[1]

library(randomForest)

for(i in 1:n){
  
  ##Building the random forest model
  RF = randomForest(Price ~ Size + Land + Bedrooms + Multiple_Car, data = train, n.trees = parameters$n_tree[i])
  
  ##Predicting on the test dataset
  RF_pred = predict(RF, test, n.trees = parameters$n_tree[i], type = 'response')
  
  ##Storing results
  parameters$RMSE[i] = sqrt(mean((RF_pred - test$Price)^2))
  parameters$MAE[i] = mean(abs(RF_pred - test$Price))   
}
parameters

## For the Random forest model, we would choose the model with 800 trees because of having a RMSE of 18530.19 and a MAE of 14129.18
  
##With depth
##Defining the depth of the trees
n_depth = c(2, 3, 4, 5, 6)

##Combining the trees and depths
parameters = expand.grid('Number_of_Trees' = n_tree, 'Depth' = n_depth)
parameters$RMSE = NA
parameters$MAE = NA

##Extracting the number of parameter combinations
n = dim(parameters)[1]

library(gbm)

  
for(i in 1:n){
  
  ##Building the gradient boosting model
  GBM = gbm(Price ~ Size + Land + Bedrooms + Multiple_Car, data = train, n.trees = parameters$Number_of_Trees[i], interaction.depth = parameters$Depth[i], distribution = 'gaussian')
  
  ##Predicting on the test dataset
  GBM_pred = predict(GBM, test, n_trees = parameters$Number_of_Trees[i], type = 'response')
  
  ##Storing results
  parameters$RMSE[i] = sqrt(mean((GBM_pred - test$Price)^2))
  parameters$MAE[i] = mean(abs(GBM_pred - test$Price))                          
  
}

##sorting RMSE MAE
parameters = parameters[order(parameters$RMSE, parameters$MAE, decreasing = FALSE), ]

parameters

## We believe that the best preferred model to choose would be the model with 100 trees with a depth of 5 because of having the lowest RMSE at 20955.56 and MAE at 17005.08

## We would choose the Random forest model because of having lower scores of RMSE and MAE, when compared to the lowest preferred GBM model results