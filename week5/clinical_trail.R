##
## Week5 HW: Clinical trial
##

library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)

trial <- read.csv("clinical_trial.csv", stringsAsFactors=F)
str(trial)
summary(trial)

# summarized the number of characters of abstracts
summary(nchar(trial$abstract))
# how many absense of abstract
summary(nchar(trial$abstract) == 0) # 112

# title
trial$title[which.min(nchar(trial$title))]

# convert text to documentTermMatrix
corpus.title <- Corpus(VectorSource(trial$title))
corpus.title <- tm_map(corpus.title, tolower)
corpus.title <- tm_map(corpus.title, removePunctuation)
corpus.title <- tm_map(corpus.title, removeWords, stopwords("english"))
corpus.title <- tm_map(corpus.title, stemDocument)
dtm.title <- DocumentTermMatrix(corpus.title)
sdtm.title <- removeSparseTerms(dtm.title, 0.95)
df.title <- as.data.frame(as.matrix(sdtm.title))

corpus.abs <- Corpus(VectorSource(trial$abstract))
corpus.abs <- tm_map(corpus.abs, tolower)
corpus.abs <- tm_map(corpus.abs, removePunctuation)
corpus.abs <- tm_map(corpus.abs, removeWords, stopwords("english"))
corpus.abs <- tm_map(corpus.abs, stemDocument)
dtm.abs <- DocumentTermMatrix(corpus.abs)
sdtm.abs <- removeSparseTerms(dtm.abs, 0.95)
df.abs <- as.data.frame(as.matrix(sdtm.abs))
dtm.abs <- DocumentTermMatrix(corpus.abs)

# most frequent word
names(df.abs)[which.max(colSums(df.abs))]

# bind the two dtm after prefixing "T" and "A" to title terms and abstract terms, respectively
colnames(df.title) <- paste0("T", colnames(df.title))
colnames(df.abs) <- paste0("A", colnames(df.abs))
dtm <- cbind(df.title, df.abs)
dtm$trial <- trial$trial

# split data
set.seed(144)
split <- sample.split(dtm$trial, SplitRatio=0.7)
train <- subset(dtm, split)
test <- subset(dtm, !split)

# baseline
prop.table(table(test$trial)) # 0.5609319

# CART
tree1 <- rpart(trial ~., data=train, method="class")
prp(tree1)
pred.train <- predict(tree1)[,2]
summary(pred.train)
table(train$trial, predict(tree1, type="class"))
mean(train$trial == predict(tree1, type="class")) # 0.8233487
# sensitivity
441/(131+441)
# specificity
631/(631+99)

# test set
pred <- predict(tree1, test)[,2]
table(test$trial, (pred > 0.21))
mean(test$trial == (pred > 0.21))

# AUC
roc1 <- prediction(pred, test$trial)
roc.perf <- performance(roc1, "sens", "acc")
plot(roc.perf, print.cutoffs.at=seq(0.2,0.3, by=0.02)) # t=0.22 
performance(roc1, "auc")@y.values # 0.8371063