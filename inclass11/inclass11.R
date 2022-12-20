##reading csv
autos = read.csv('Automobile_data.csv')
##making scatterplot bwtween wheel base and length
plot(autos$wheel.base, autos$length, xlab = 'wheel base', ylab = 'length', psh = 16)
grid()
##there is a positive linear relationship in the scatterplot between wheel base and length

##building lm model
lm_md = lm(length ~ wheel.base, data = autos)

##checking for constand variance
plot(fitted(lm_md), residuals(lm_md), xlab = 'fitted values', ylab = 'residuals')
abline(h = 0, lwd = 2)
grid()
##residuals seem to follow an upside down parabola pattern which means constant variance is not met