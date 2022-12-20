##Reading csv file
churn = read.csv(file = "Customer_Churn.csv")
head(churn)

##changing labels to 0 and 1
churn$Churn_numb = ifelse(churn$Churn == 'No',0,1)
head(churn)

##building the logistic reg model
logit_md = glm(Churn_numb ~ tenure + MonthlyCharges, data = churn, family = 'binomial')

##Extracting
summary(logit_md)

##definging newobservation
newdata = data.frame('tenure'= 12, 'MonthlyCharges' = 250)
newdata

##predicting likelyhood of churning 
predict(logit_md, newdata, type = 'response')
