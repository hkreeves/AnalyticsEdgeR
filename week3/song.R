##
## Week3 HW: Music Popularity
##

songs <- read.csv("songs.csv", stringsAsFactors=F)
str(songs)
summary(songs)

# number of songs per year
table(songs$year)

# michael jackson
sum(songs$artistname == "Michael Jackson")
songs[songs$artistname == "Michael Jackson", c("songtitle", "Top10")]

# timesignature
table(songs$timesignature)

# highest pitch
head(songs$songtitle[order(songs$tempo, decreasing=T)])

# dataset splitting
train <- subset(songs, year <= 2009)
test <- subset(songs, year == 2010)
table(train$Top10);table(test$Top10)
dim(train)

# keep only numerical variables
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
train <- train[, !names(train) %in% nonvars]
test <- test[, !names(train) %in% nonvars]
str(train)

# build logistic regression
log1 <- glm(Top10 ~ ., data=train, family="binomial")
summary(log1) #aic=4827.154

# multicollinearity
cor(train[, -1]) # loudness and energy are highly correlted

# model 2 and 3 of keeping only one variable between the two
log2 <- glm(Top10 ~ . - loudness, data=train, family="binomial")
summary(log2) # aic=4937.8
log3 <- glm(Top10 ~ . - energy, data=train, family="binomial")
summary(log3) # aic=4848

# optional: step-wise selection
log.opt <- step(log3)

# predicting on test set
pred.test <- predict(log3, newdata=test, type="response") > 0.45
table(test$Top10, pred.test)
mean(as.numeric(pred.test) == test$Top10) # 0.879

# baseline model: pick most frequent outcome
mean(test$Top == names(sort(table(test$Top), dec=T))[1]) # 0.842

# optional: step-wise model
pred.test.opt <- predict(log.opt, newdata=test, type="response") > 0.45
table(test$Top10, pred.test.opt)
mean(as.numeric(pred.test.opt) == test$Top10) # 0.874 no where better

# Notice that a label record is more concerned about correctly predicting
# Top10 songs. 

# sensitivity of model3
19/(40+19) 
# specificity of model3
309/(309+5)