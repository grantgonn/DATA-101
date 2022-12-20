##Question 1) A, True
##Question 2) C, Model 3 

##reading csv
autos = read.csv(file = 'autos.csv')
head(autos)

##standardizing the input data
autos$z_cylinders = (autos$cylinders - mean(autos$cylinders)) / sd(autos$cylinders)
autos$z_displacement = (autos$displacement - mean(autos$displacement)) / sd(autos$displacement)
autos$z_horsepower = (autos$horsepower - mean(autos$horsepower)) / sd(autos$horsepower)
autos$z_weight = (autos$weight - mean(autos$weight)) / sd(autos$weight)
autos$z_acceleration = (autos$acceleration - mean(autos$acceleration)) / sd(autos$acceleration)
head(autos)

##splitting the data into train and test
n = dim(autos[1])
train_index = sample(1:n, round(.8*n))
train = autos[train_index, ]
test = autos[-train_index, ]

##k-NN
library(caret)
knn_md = knnreg(mpg ~ z_cylinders + z_displacement + z_horsepower + z_weight + z_acceleration, data = train, k=5)

##predicting on test data
knn_pred = predict(knn_md, test)
knn_rmse = sqrt(mean((knn_pred - test$mpg)^2))
knn_rmse
knn_mae = mean(abs(knn_pred - test$mpg))
knn_mae

##random froest
library(randomForest)
RF_md = randomForest(mpg ~ z_cylinders + z_displacement + z_horsepower + z_weight + z_acceleration, data = train, ntree = 500)

RF_pred = predict(RF_md, test)
RF_rmse = sqrt(mean((RF_pred - test$mpg)^2))
RF_rmse
RF_mae = mean(abs(RF_pred - test$mpg))
RF_mae

##gradient boosting 
library(gbm)
gbm_md = gbm(mpg ~ z_cylinders + z_displacement + z_horsepower + z_weight + z_acceleration, data = train, distribution = "gaussian", n.trees = 500, interaction.depth = 4)

gbm_pred = predict(gbm_md, test, n.trees = 500 )
gbm_rmse = sqrt(mean((gbm_pred - test$mpg)^2))
gbm_rmse
gbm_mae = mean(abs(gbm_pred - test$mpg))
gbm_mae

##either the random forest or the gradient boosing since they have similar performance in terms of RMSE and MAE


