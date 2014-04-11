##
## Week5 HW: spam filter
##

library(caTools)
library(tm)
library(ROCR)
library(rpart)
library(rpart.plot)
library(RTextTools)

email <- read.csv("emails.csv", stringsAsFactors=F)
str(email)
table(email$spam)
summary(nchar(email$text))

# shortest email
which.min(nchar(email$text))

# convert text to documentTermMatrix
corpus <- Corpus(VectorSource(email$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)

# remove sparse terms
spdtm <- removeSparseTerms(dtm, 0.95)
spdtm

email.sparse <- as.data.frame(as.matrix(spdtm))
colnames(email.sparse) <- make.names(colnames(email.sparse))

# find the most frequent term
colnames(email.sparse)[which.max(colSums(email.sparse))]
sort(colSums(email.sparse), decreasing=T)[1:10]

# How many word stems appear at least 5000 times in the ham emails in the dataset? 
email.sparse$spam <- email$spam
colnames(email.sparse)[which(colSums(subset(email.sparse, spam == 0))[-331] >= 5000)]

colnames(email.sparse)[which(colSums(subset(email.sparse, spam == 1))[-331] >= 1000)]

# build model
email.sparse$spam <- as.factor(email.sparse$spam)
set.seed(123)

split <- sample.split(email.sparse$spam, 0.7)
train <- subset(email.sparse, split)
test <- subset(email.sparse, !split)

# logistic regression
log1 <- glm(spam ~ ., data=train, family="binomial")
pred.train <- predict(log1, type="response")
sum(pred.train < 1e-5)
sum(pred.train > (1-1e-5))
summary(log1) # the logistic model is strongly overfitted

table(train$spam, pred.train > 0.5)
mean(train$spam == as.numeric(pred.train > 0.5)) # 0.9990025

roc1 <- prediction(pred.train, train$spam)
performance(roc1, "auc")@y.values

# CART
tree1 <- rpart(spam ~ ., data=train)
prp(tree1)

pred.tree <- predict(tree1)[,2]
mean(train$spam == as.numeric(pred.tree > 0.5)) # 0.942394

roc2 <- prediction(pred.tree, train$spam)
performance(roc2, "auc")@y.values

# random forests
library(randomForest)

set.seed(123)
rf1 <- randomForest(spam ~ ., data=train)

pred.rf <- predict(rf1, type="prob")[,2]
mean(train$spam == as.numeric(pred.rf > 0.5)) # 0.9793017

roc3 <- prediction(pred.rf, train$spam)
performance(roc3, "auc")@y.values

### on test set
pred1 <- predict(log1, test, type="response")
pred2 <- predict(tree1, test)[,2]
pred3 <- predict(rf1, test, type="prob")[,2]

mean(as.numeric(pred1 > 0.5) == test$spam) # 0.9505239
mean(as.numeric(pred2 > 0.5) == test$spam) # 0.9394645
mean(as.numeric(pred3 > 0.5) == test$spam) # 0.9749709

performance(prediction(pred1, test$spam), "auc")@y.values # 0.9627517
performance(prediction(pred2, test$spam), "auc")@y.values # 0.963176
performance(prediction(pred3, test$spam), "auc")@y.values # 0.9975656

############ part 2 ###############

## extra word count as a new variable
word.count <- rowSums(as.matrix(dtm))
# or
library(slam)
word.count = rollup(dtm, 2, FUN=sum)$v

hist(word.count)
hist(log(word.count))

email.sparse$logwc <- log(word.count)
boxplot(logwc ~ spam, data=email.sparse) # spam wordcount < ham wordcount

# split
train2 <- subset(email.sparse, split)
test2 <- subset(email.sparse, !split)

# new CART
tree2 <- rpart(spam ~ ., data=train2)
# new rf
set.seed(123)
rf2 <- randomForest(spam ~., data=train2)

prp(tree2)

# on test set
pred4 <- predict(tree2, test2)[,2]
pred5 <- predict(rf2, test2, type="prob")[,2]

mean(as.numeric(pred4 > 0.5) == test2$spam) #  0.9301513
mean(as.numeric(pred5 > 0.5) == test2$spam) # 0.9778813

performance(prediction(pred4, test2$spam), "auc")@y.values # 0.9582438
performance(prediction(pred5, test2$spam), "auc")@y.values # 0.9980905

## 2-gram new variable (phases of consecutive 2 words)
# Another source of information that might be extracted from text is 
# the frequency of various n-grams. An n-gram is a sequence of 
# n consecutive words in the document. For instance, 
# for the document "Text analytics rocks!", which we would 
# preprocess to "text analyt rock", the 1-grams are 
# "text", "analyt", and "rock", the 2-grams are "text analyt" 
# and "analyt rock", and the only 3-gram is "text analyt rock". 
# n-grams are order-specific, meaning the 2-grams "text analyt" 
# and "analyt text" are considered two separate n-grams. 
# We can see that so far our analysis has been extracting only 1-grams.
#
# In this last subproblem, we will add 2-grams to our predictive model. 
# Begin by installing and loading the RTextTools package. 

dtm2gram <- create_matrix(as.character(corpus), ngramLength=2)
spdtm2gram <- removeSparseTerms(dtm2gram, 0.95)
spdtm2gram

email.sparse2g <- as.data.frame(as.matrix(spdtm2gram))
colnames(email.sparse2g) <- make.names(colnames(email.sparse2g))
rownames(email.sparse2g) <- NULL

# combine 1gram and 2gram
emails.combine <- cbind(email.sparse, email.sparse2g)

train3 <- subset(emails.combine, split)
test3 <- subset(emails.combine, !split)

# build models with new varaibles included
tree3 <- rpart(spam ~ ., data=train3)
set.seed(123)
rf3 <- randomForest(spam ~ ., data=train3)

prp(tree3, varlen=0)

# on test set
pred6 <- predict(tree3, test3)[,2]
pred7 <- predict(rf3, test3, type="prob")[,2]

mean(as.numeric(pred6 > 0.5) == test3$spam) #  0.93539
mean(as.numeric(pred7 > 0.5) == test3$spam) # 0.9778813

performance(prediction(pred6, test3$spam), "auc")@y.values # 0.9648206
performance(prediction(pred7, test3$spam), "auc")@y.values # 0.9977307





