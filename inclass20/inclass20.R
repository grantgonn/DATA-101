##read the csv
autos = read.csv(file = 'autos.csv')

##splitting 
n = dim(autos)[1]

train_index = sample(1:n, round(0.8*n))
train = autos[train_index,]
test = autos[-train_index,]

##definging #of trees
n_tree = c(100,300,500,800,1000,1500,2000)

##defning depth
n_depth = c(2,3,4,5,6)

##combining tree depth
parameters = expand.grid('number_of_trees' = n_tree, 'depth' = n_depth)
parameters$RMSE = NA
parameters$MAE = NA

##extracting the number of parameter combinations 
n = dim(parameters)[1]

library(gbm)

for(i in 1:n){
  ##building gbm model
  GBM = gbm(mpg ~ cylinders +displacement+ horsepower +weight +acceleration+ year, data = train, 
            n.trees = parameters$number_of_trees[i], interaction.depth = parameters$Depth[i], distribution = 'gaussian' )
  ##predicting on test data
  GBM_pred = predict(GBM, test, n.trees = parameters$number_of_trees[i], type = 'response')
  ##storeing results
  parameters$RMSE[i] = sqrt(mean(GBM_pred - test$mpg)^2)
  parameters$MAE[i]= mean(abs(GBM_pred - test$mpg))
  
}
##soring on rmse and mae
parameters = parameters[order(parameters$RMSE, parameters$MAE, decreasing = FALSE),]
parameters
