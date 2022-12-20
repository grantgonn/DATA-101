##reading the csv file
Index = read.csv(file = 'Index.csv')
head(Index)

##creating a bar chart 
Index$Index
barplot(table(Index$Index))
##barplot is left skewed, most data falls under index 4 or 5

##creating side by side boxplots 
boxplot(Height ~ Gender, data = Index)
##we see the height distribution is very similar between the two plots 

boxplot(Weight ~ Gender, data = Index)
##we see the height distributions are very similar