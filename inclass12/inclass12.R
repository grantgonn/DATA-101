##reading csv file
autos = read.csv(file = 'Automobile_data.csv')
head(autos)

##building linear model
lm_md = lm(price ~ horsepower + wheel.base, data = autos)
##extracting results
summary(lm_md)

##creating new observation
newdata = data.frame('horsepower' = 150, 'wheel.base' = 100)
##predicting observation
predict(lm_md, newdata)
