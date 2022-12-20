## Reading data
mercury = read.csv(file = 'Mercury.csv')

head(mercury)

##Fitting to linear model
lm_md = lm(formula = log(Hg) ~ log(Alk) + log(pH) + log(Ca) + log(Chlo), data = mercury)
summary(lm_md)

##B0 = .35461 B1 = -0.49013 B2 = 0.26202 B3 = 0.09376 B4 = -0.21706

## R^2 is .6159, our model explains 61.59% of the variation in the log(Hg) can be explained by log(Alk) + log(pH) + log(Ca) + log(Chlo)

lm_md = lm(log(Hg) ~ log(Alk) + log(Chlo), data = mercury)
summary(lm_md)

##B0 = .69449, B1 = -0.37877 B2 = -0.20049

## R^2 is .608. our model explains 60.8% of the variation in the log(Hg) can be explained by log(Alk) + log(Chlo)

##We would the model in B, because the model would explain more of the variance than model D.