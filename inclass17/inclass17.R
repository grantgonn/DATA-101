##Question 1) B, False
##Question 2) B, False
##Question 3) A, model 1
##Question 4) B, false

##reading csv
churn = read.csv(file = 'Customer_Churn.csv')
head(churn)

##changing the label to 0 1 
churn$churn_numb = ifelse(churn$Churn == 'No',0,1)
head(churn)

##defining full model
md_full = glm(churn_numb ~ gender + SeniorCitizen + tenure + Contract + PaperlessBilling + MonthlyCharges 
              + DeviceProtection + TechSupport, data = churn, family = 'binomial')
summary(md_full)

##running hybrid selection
hybrid = step(md_full, direction = 'both')


















