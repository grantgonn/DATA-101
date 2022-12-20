##reading the csv file
autos = read.csv(file='Automobile_data.csv')

##reporting number of observations and variables
dim(autos)


##printing the last three observations
tail(autos,3)

#frequency table of companies
table(autos$company)

##seecting audi with 4 cylinders
audi_4 = subset(autos, company == 'audi' & num.of.cylinders == "four")
audi_4
