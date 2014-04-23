##
## Week6 HW: Document Clustering
##

dk <- read.csv("dailykos.csv")
dim(dk)

dist.matrix <- dist(dk[-1], method="euclidean")
hier.clust <- hclust(dist.matrix, method="ward")

# hierachical clustering
plot(hier.clust)
rect.hclust(hier.clust,k=7)

# split into 7 groups
groups <- cutree(hier.clust, k=7)
table(groups)

clust1 <- subset(dk, groups == 1)
clust2 <- subset(dk, groups == 2)
clust3 <- subset(dk, groups == 3)
clust4 <- subset(dk, groups == 4)
clust5 <- subset(dk, groups == 5)
clust6 <- subset(dk, groups == 6)
clust7 <- subset(dk, groups == 7)

head(sort(colMeans(clust1[-1]), decreasing=T))
#      bush      kerry   democrat       poll republican      state 
head(sort(colMeans(clust2[-1]), decreasing=T))
#  november      poll      vote challenge  democrat      bush
head(sort(colMeans(clust3[-1]), decreasing=T))
#      bush   democrat republican      state    parties      elect
head(sort(colMeans(clust4[-1]), decreasing=T))
#     kerry     bush     poll presided    voter campaign 
head(sort(colMeans(clust5[-1]), decreasing=T))
#      bush    iraq      war    administration  presided    american
head(sort(colMeans(clust6[-1]), decreasing=T))
#     poll  democrat     elect     kerry      bush      race
head(sort(colMeans(clust7[-1]), decreasing=T))
#     dean    kerry     poll   edward    clark democrat

# kmeans
set.seed(1000)
km.clust <- kmeans(dk[-1], centers=7)
table(km.clust$cluster)

km.groups <- list()
for(i in 1:7){
  km.groups <- c(km.groups, list(subset(dk, km.clust$cluster == i)))
}

lapply(km.groups, function(x) head(sort(colMeans(x[-1]), decreasing=T)))

# check the docID of km.cluster 7
ids7 <- km.groups[[7]]$Document
len <- length(ids7) # 308
# compare the composition to hier.clusters
hids4 <- clust4$Document
hids1 <- clust1$Document
hids3 <- clust3$Document
hids6 <- clust6$Document

length(intersect(ids7, hids1))
length(intersect(ids7, hids3))
length(intersect(ids7, hids4))
length(intersect(ids7, hids6))

# should just use table
prop.table(table(groups, km.clust$cluster), margin=2)