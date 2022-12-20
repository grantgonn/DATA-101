## Reading the csv file
diamonds = read.csv(file = 'diamonds.csv')
head(diamonds)
table(diamonds)

##Removing x variable
diamonds = diamonds[, -1]
head(diamonds)

##Creating scatter plot of radius_mean and area_mean (not finished, need to check)
plot(carat ~ price, data = diamonds)
grid()

## Based on the observations of the scatter-plot, there is a positive correlation
## as carat increases, the price slowly increases as well


## Applying z-score to carat
diamonds$z_carat = (diamonds$carat - mean(diamonds$carat)) / sd(diamonds$carat)
head(diamonds)

## z-score of depth
diamonds$z_depth = (diamonds$depth - mean(diamonds$depth)) / sd(diamonds$depth)

## z-score of table
diamonds$z_table = (diamonds$table - mean(diamonds$table)) / sd(diamonds$table)


## z-score of x
diamonds$z_x = (diamonds$x - mean(diamonds$x)) / sd(diamonds$x)


## z-score of y
diamonds$z_y = (diamonds$y - mean(diamonds$y)) / sd(diamonds$y)


## z-score of z
diamonds$z_z = (diamonds$z - mean(diamonds$z)) / sd(diamonds$z)
head(diamonds)


## Here we split the data into training and testing

diamonds_train_X = diamonds[0:43000,c('z_carat', 'z_depth', 'z_table', 'z_x', 'z_y', 'z_z') ]
diamonds_train_Y = diamonds[0:43000, c('price')]

diamonds_test_X = diamonds[43000:53940,c('z_carat', 'z_depth', 'z_table', 'z_x', 'z_y', 'z_z') ]
diamonds_test_Y = diamonds[43000:53940, c('price')]


## Here we load the class library

library(caret)
## Here we create a two nearest neighbors classifier
## and then we predict on testing data

ten_nearest_neighbors = knnreg(diamonds_train_X, diamonds_train_Y, k = 10)


## Here we compare the actual against predictions

## Here we predict on the testing data
predictions = predict(ten_nearest_neighbors, diamonds_test_X)

## Here we visualize the actuals vs predictions
plot(diamonds_test_Y, predictions, xlab = 'actual price',
     ylab = 'Predicted price' , pch = 16, col = 'blue')
grid()

##Based on the plot created, the model is is not accurate in its predictions on price compared to the actual prices

