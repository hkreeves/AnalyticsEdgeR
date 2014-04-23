##
## Week6 HW: Medical Cost
##

library(caret)
library(flexclust)

claims <- read.csv("reimbursement.csv")
str(claims)

# how many records have at least one chronic condition
prop.table(table(rowSums(claims[2:12]) >= 1))

# correlation between conditions
cor(claims[2:12])

# histogram of dependent variable
hist(claims$reimbursement2009, breaks=50) # highly right-skewed

# transform to log
claims$reimbursement2009 <- log(claims$reimbursement2009 + 1)
claims$reimbursement2008 <- log(claims$reimbursement2008 + 1)

hist(claims$reimbursement2009, breaks=50)

# how many zeros
mean(claims$reimbursement2009 == 0)

# split data
set.seed(144)
spl = sample(1:nrow(claims), size=0.7*nrow(claims))
train = claims[spl,]
test = claims[-spl,]

# train data with linear regression
lm1 <- lm(reimbursement2009 ~ ., data=train)
summary(lm1)

# predict on test set and calculate RMSE
pred.lm <- predict(lm1, newdata=test)
rmse.lm <- sqrt(mean((test$reimbursement2009 - pred.lm)^2))

# naive baseline
base <- mean(train$reimbursement2009)
sqrt(mean((test$reimbursement2009 - base)^2)) #3.335486

# time series smart baseline: just follow 2008
sqrt(mean((test$reimbursement2009 - test$reimbursement2008)^2)) # 2.094668

# cluster-then-predict methodology
train.limited <- train[-14]
test.limited <- test[-14]

preproc = preProcess(train.limited)
train.norm = predict(preproc, train.limited)
test.norm = predict(preproc, test.limited)

summary(train.norm)
summary(test.norm)

# kmeans clustering
set.seed(144)
km <- kmeans(train.norm, centers=3)
groups.train <- km$cluster
centers.train <- km$centers

km.kcca <- as.kcca(km, train.norm)
cluster.train <- predict(km.kcca)
cluster.test <- predict(km.kcca, newdata=test.norm)

table(cluster.test)

# split data according to cluster assignment
train.split <- split(train, cluster.train)
test.split <- split(test, cluster.test)

# which training set data frame has the highest average value of 
# the dependent variable?
tapply(train$reimbursement2009, cluster.train, mean)

lm.split <- sapply(train.split, function(x) lm(reimbursement2009 ~ ., data=x)$coefficient)

# build cluster-dependent models
lm.c1 <- lm(reimbursement2009 ~ ., data=train.split[[1]])
lm.c2 <- lm(reimbursement2009 ~ ., data=train.split[[2]])
lm.c3 <- lm(reimbursement2009 ~ ., data=train.split[[3]])

pred.c1 <- predict(lm.c1, newdata=test.split[[1]])
pred.c2 <- predict(lm.c2, newdata=test.split[[2]])
pred.c3 <- predict(lm.c3, newdata=test.split[[3]])

mean(pred.c1)
mean(pred.c2)
mean(pred.c3)
# compare to real dependent variable
tapply(test$reimbursement2009, cluster.test, mean)

# rmse
library(medley)
rmse(pred.c1, test.split[[1]]$reimbursement2009)
rmse(pred.c2, test.split[[2]]$reimbursement2009)
rmse(pred.c3, test.split[[3]]$reimbursement2009)

library(plyr)
pred.test <- c(pred.c1, pred.c2, pred.c3)
real.test <- ldply(test.split)$reimbursement2009 
rmse(pred.test, real.test) # 1.811335, quite an improvement over both baseline models