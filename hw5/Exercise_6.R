##Reading csv file
customer_offers = read.csv(file = 'customer_offers.csv')
head(customer_offers)
##Quick summary of variables
summary(customer_offers)
##Dropping customer_name variable
customer_offers = customer_offers[,-1]
head(customer_offers)
## running k-means
customer_offers_cluster = kmeans(customer_offers, centers = 3, nstart = 20)
customer_offers_cluster$cluster
##The center of the clusters are 1, 42, and 83.
head(customer_offers)
## To describe the clusters, in the first cluster around 1 it appears to have a similar grouping as the cluster around 42
## The cluster around 42,appears to primarily 3 as the main number in its grouping
## For the cluster around 83, it has the least amount of observations in its clusters.