##Question 1) B, False
##Question 2) B, False
##Question 3) B, model 2

##reading csv
churn = read.csv(file = 'Customer_Churn.csv')
head(churn)

##changing the label to 0 1 
churn$churn_numb = ifelse(churn$Churn == 'No',0,1)
head(churn)

##Building null logistic model 
md_null = glm(churn_numb ~ 1, data = churn, family = 'binomial')
summary(md_null)

##building th full logistic model
md_full = glm(churn_numb ~ gender + SeniorCitizen + tenure + Contract + PaperlessBilling + MonthlyCharges + DeviceProtection + TechSupport, data = churn, family = 'binomial')
summary(md_full)

##performing forward selection
forward_selection = step(md_null, scope = list(lower = md_null, upper = md_full), direction = 'forward')

##peforming backwards selection
backwards_selestion = step(md_full, direction = 'backward')



