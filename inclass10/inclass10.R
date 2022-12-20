autos = read.csv('Automobile_data.csv')
##creating scatterplot
plot(autos$wheel.base, autos$length, xlab = 'wheelbase', ylab = 'length', psh = 16)
grid()
##there is a positive linear relationship between wheelbase and length in the scatterplot
##buld lm
lm_md = lm(length ~ wheel.base, data = autos)
##extract results
summary(lm_md)
##for one unit increase of wheel base length increases by 1.844 on average