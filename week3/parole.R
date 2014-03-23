##
## Week3 HW: Parole
##

parole <- read.csv("parole.csv")
str(parole)
summary(parole)

# how many violators
table(parole$violator)

# convert factor vaiables
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
summary(parole)

# split dataset
library(caTools)
set.seed(144)
split <- sample.split(parole$violator, SplitRatio=0.7)
train <- subset(parole, split)
test <- subset(parole, !split)

# build logistic regression model using all variables as dep. var
log1 <- glm(violator ~., data=train, family="binomial")
summary(log1)

# test
guy <- list(male=1, race=1, age=50, state=1, time.served=3, max.sentence=12,
		multiple.offenses=0, crime=2)
guy$state <- factor(guy$state, levels=1:4)
guy$crime <- factor(guy$crime, levels=1:4)
pred.guy <- predict(log1, newdata=data.frame(guy), type="response")

# predict on test set
pred.test <- predict(log1, newdata=test, type="response")
summary(pred.test)
table(test$violator, pred.test > 0.5)
# sensitivity
12/(11+12) # 0.5217391
# specificity
167/(167+12) # 0.9329609
# accuracy
(167+12)/nrow(test)

# baseline model of nonvialoter prediction
1-mean(test$violator)

# ROCR AUC score
library(ROCR)
roc1 <- prediction(pred.test, test$violator)
performance(roc1, "auc")@y.values # 0.8945834
plot(performance(roc1, "tpr", "fpr"), colorize=T,
	print.cutoffs.at=seq(0,1,0.1))

# investigate sources of bias

