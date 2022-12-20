##Reading csv file
churn = read.csv(file = 'Churn_Data.csv')
head(churn)

library(randomForest)

##Building the random forest model
RF_md = randomForest(as.factor(Churn) ~ Age + EstimatedSalary + CreditScore + Balance + NumOfProducts, data = churn) 


## Defining the new observations
newdata = data.frame('Age' = 50, 'EstimatedSalary' = 100000, 'CreditScore' = 600, 'Balance' = 100000, 'NumOfProducts' = 2)
newdata

##Estimating the likelihood of churn
predict(RF_md, newdata, type = 'prob')

##The observation has a 83.2% liklihood of not churning.

##Loading gbm library 
library(gbm)

## Here we fit the boosted classification tree
gbm.md = gbm(Churn ~ Age + EstimatedSalary + CreditScore + Balance + NumOfProducts, data = churn,
             distribution = 'bernoulli', n.tree = 500,
             interaction.depth = 4)

## Here we predict the probability of new observations

predict(gbm.md, newdata_gbm = data.frame(Age = 50, EstimatedSalary = 100000, CreditScore = 600, Balance = 100000, NumOfProducts = 2), n.tree = 500, type = 'response')

predict(gbm.md, newdata_gbm, type = 'response')
## Based on the gbm model, the customers likelihood of churning is only at 29% and would not be flagged based on this model since the threshold is 40%
