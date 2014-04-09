##
## Week5 HW: Wiki Vandalism Detector
##

library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)

wiki <- read.csv("wiki.csv", row.names=1, stringsAsFactors=F)
str(wiki)
summary(wiki)

wiki$Vandal <- as.factor(wiki$Vandal)
table(wiki$Vandal)

# create Bags of Words for added content
corpus.add <- Corpus(VectorSource(wiki$Added))
corpus.add <- tm_map(corpus.add, removeWords, stopwords("english"))
corpus.add <- tm_map(corpus.add, stemDocument)
# create document term matrix
dtm.add <- DocumentTermMatrix(corpus.add)
dtm.add # (3876 documents, 6675 terms)
# filter out sparse terms 0.3%
sparse.add <- removeSparseTerms(dtm.add, 0.997)
sparse.add (3876 documents, 166 terms)
words.add <- data.frame(as.matrix(sparse.add))
colnames(words.add) <- paste("A", colnames(words.add))

# repeate the same process for removed content
corpus.rm <- Corpus(VectorSource(wiki$Removed))
corpus.rm <- tm_map(corpus.rm, removeWords, stopwords("english"))
corpus.rm <- tm_map(corpus.rm, stemDocument)
dtm.rm <- DocumentTermMatrix(corpus.rm)
sparse.rm <- removeSparseTerms(dtm.rm, 0.997)
words.rm <- data.frame(as.matrix(sparse.rm))
colnames(words.rm) <- paste("R", colnames(words.rm))

wikiwords <- cbind(words.add, words.rm)
wikiwords$Vandal <- wiki$Vandal

set.seed(123)
split <- sample.split(wikiwords$Vandal, 0.7)
train <- subset(wikiwords, split)
test <- subset(wikiwords, !split)

# baseline: most frequent result (non-vandalism)
prop.table(table(test$Vandal)) # 0.5313844

# CART model
tree1 <- rpart(Vandal ~ ., data=train)
prp(tree1)

pred1 <- predict(tree1, newdata=test, type="class")
table(test$Vandal, pred1)
mean(test$Vandal == pred1) # 0.5417025

# Identify "http" appearance
wikiwords2 <- wikiwords
wikiwords2$http <- as.numeric(grepl("http", wiki$Added, fixed=T))
table(wikiwords2$http)

train2 <- subset(wikiwords2, split)
test2 <- subset(wikiwords2, !split)

tree2 <- rpart(Vandal ~ ., data=train2)
prp(tree2)

pred2 <- predict(tree2, newdata=test2, type="class")
table(test2$Vandal, pred2)
mean(test2$Vandal == pred2) # 0.5726569

# Add new features: #words added, #words removed
wikiwords2$nw.add <- rowSums(as.matrix(dtm.add))
wikiwords2$nw.rm <- rowSums(as.matrix(dtm.rm))
mean(wikiwords2$nw.add)

train3 <- subset(wikiwords2, split)
test3 <- subset(wikiwords2, !split)

tree3 <- rpart(Vandal ~ ., data=train3)
prp(tree3)

pred3 <- predict(tree3, newdata=test3, type="class")
table(test3$Vandal, pred3)
mean(test3$Vandal == pred3) # 0.6552021

# use Minor and Loggedin
wikiwords3 <- wikiwords2
wikiwords3$Minor <-  wiki$Minor
wikiwords3$Loggedin <- wiki$Loggedin

train4 <- subset(wikiwords3, split)
test4 <- subset(wikiwords3, !split)

tree4 <- rpart(Vandal ~ ., data=train4)
prp(tree4)

pred4 <- predict(tree4, newdata=test4, type="class")
table(test4$Vandal, pred4)
mean(test4$Vandal == pred4) # 0.7188306