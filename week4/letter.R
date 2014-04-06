##
## Week4 HW: abpr
##

library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

abpr <- read.csv("letters_ABPR.csv")
str(abpr)

abpr$isB <- as.factor(abpr$letter == "B")

# split data with balanced isB
set.seed(1000)
splits <- sample.split(abpr$isB, SplitRatio=0.5)
train <- subset(abpr, splits)
test <- subset(abpr,!splits)

# baseline: most frequent
prop.table(table(test$isB))

# tree
tree.b <- rpart(isB ~ . - letter, data=train, method="class")
pred <- predict(tree.b,newdata=test, type="class")
mean(test$isB == pred) # 0.9358151
table(test$isB, pred)

# forest
set.seed(1000)
rf.b <- randomForest(isB ~ . - letter, data=train)
pred.rf <- predict(rf.b, newdata=test, type="class")
mean(test$isB == pred.rf) # 0.9878049
table(test$isB, pred.rf)

# multiclass prediction
abpr$letter <- as.factor(abpr$letter)
set.seed(2000)
splits <- sample.split(abpr$letter, 0.5)
train.abpr <- subset(abpr, splits)
test.abpr <- subset(abpr, !splits)

# baseline: most frequent
prop.table(table(test.abpr$letter)) # 0.2573813 (P)

# CART
tree.abpr <- rpart(letter ~ . - isB, data=train.abpr, method="class")
pred.tree <- predict(tree.abpr, newdata=test.abpr, type="class")
mean(test.abpr$letter == pred.tree) # 0.8786906
table(test.abpr$letter, pred.tree)

# forest
rf.abpr <- randomForest(letter ~ . - isB, data=train.abpr)
pred.rf <- predict(rf.abpr, newdata=test.abpr, type="class")
mean(test.abpr$letter == pred.rf) # 0.9807445
table(test.abpr$letter, pred.rf)