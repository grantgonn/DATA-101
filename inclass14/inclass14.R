##reading csv
churn = read.csv(file = 'Customer_Churn.csv')
head(churn)

##changing the lable to 0 1 
churn$churn_numb = ifelse(churn$Churn == 'No',0,1)
head(churn)

library(rpart)
##building the decision tree model
tree_md = rpart(churn_numb ~ tenure + MonthlyCharges, data = churn)

##defining the new observation
newdata = data.frame('tenure' = 12, "MonthlyCharges" = 250)

##predicting churn
predict(tree_md, newdata)
