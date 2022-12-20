##reading the csv
interactions = read.csv(file = 'customer_interactions.csv')
##computing z-score
interactions$z_spend = (interactions$spend - mean(interactions$spend))/ sd(interactions$spend)
interactions$z_interactions = (interactions$interactions - mean(interactions$interactions))/ sd(interactions$interactions)

##kmeans
interactions_clusters = kmeans(interactions[, c(3,4)], centers = 4, nstart = 20)

##sppending cluster labels
interactions$cluster = interactions_clusters$cluster
head(interactions)

##hierahclal clustering 
hc = hclust(dist(interactions[, c(3,4)], method = "euclidian"), method = 'complete')

##visualizing
plot(hc)
