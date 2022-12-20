##reading csv
churn = read.csv(file = 'Customer_Churn.csv')
head(churn)

##changing the label to 0 1 
churn$churn_numb = ifelse(churn$Churn == 'No',0,1)
head(churn)

library(randomForest)
##building the random forest
rf_md = randomForest(as.factor(churn_numb) ~ tenure + MonthlyCharges, data = churn)

##defining the new observation
newdata = data.frame('tenure' = 12, "MonthlyCharges" = 250)

##predicting churn
predict(rf_md, newdata, type = 'prob')
