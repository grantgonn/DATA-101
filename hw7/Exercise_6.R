##reading csv file
churn = read.csv(file = 'Churn_Data.csv')
head(churn)

##Building logistic regression model
glm_md = glm(Churn ~ Age + EstimatedSalary + CreditScore + Balance + NumOfProducts, data = churn)

##Creating new observations
newdata = data.frame('Age' = 50, 'EstimatedSalary' = 100000, 'CreditScore' = 600, 'Balance' = 100000, 'NumOfProducts' = 2)
newdata

##Predicting with new observations
predict(glm_md, newdata)

## With this prediction, the customer would not be flagged because they are only 34.4% chance of likelihood to churn,
## While the threshold is at 40% to flag out customers