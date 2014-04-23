##
## Week6 HW: Airline Loyalty Program
##

library(caret)

airline <- read.csv("AirlinesCluster.csv")
str(airline)
summary(airline)

# normalize data using preProcess() in caret
preproc1 <- preProcess(airline, method=c("center", "scale"))
airline.nor <- predict(preproc1, airline)

# hierachical clustering
dm <- dist(airline.nor, method="euclidean")
hier.clust <- hclust(dm, method="ward")
plot(hier.clust)

# split into 5 clusters
hc.groups <- cutree(hier.clust, k=5)
rect.hclust(hier.clust, k=5)

table(hc.groups)

# compute the centroids
tapply(airline$Balance, hc.groups, mean)
apply(airline, 2, function(x) tapply(x, hc.groups, mean))
# or
sapply(split(airline, hc.groups), colMeans)

# kmeans clustering
set.seed(88)
km.clust <- kmeans(airline.nor, centers=5, iter.max=1000)
table(km.clust$cluster)

# centroids
km.clust$centers
apply(airline, 2, function(x) tapply(x, km.clust$cluster, mean))

# compare the partitions from the two clustering solutions
table(hc.groups, km.clust$cluster)