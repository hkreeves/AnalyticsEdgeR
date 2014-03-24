##
## Week3 HW: Loan
##

loan <- read.csv("loans.csv")
str(loan)
summary(loan)

prop.table(table(loan$not.fully.paid))

# how many complete cases
complete <- complete.cases(loan)
table(complete)

table(loan$not.fully.paid[complete])
table(loan$not.fully.paid[!complete])

# multiple imputation
library(mice)
set.seed(144)

loan[,1:(ncol(loan)-1)] <- complete(mice(loan[,1:(ncol(loan)-1)]))
#vars.for.imputation = setdiff(names(loan), "not.fully.paid")
#imp = complete(mice(loan[vars.for.imputation]))
#loan[vars.for.imputation] = imp
loan <- read.csv("loans_imputed.csv")

# split data
library(caTools)
set.seed(144)
split <- sample.split(loan$not.fully.paid, SplitRatio=0.7)
train <- subset(loan, split)
test <- subset(loan, !split)

# logistic regression model using all independent variables
log1 <- glm(not.fully.paid ~., data=train, family="binomial")
summary(log1) # int.rate is not significant, counter-intuitively
cor(train[-2]) # indeed it is high correlated with fico (0.7)

# predict on test set
test$predict.risk <- predict(log1, newdata=test, type="response")
table(test$not.fully.paid, test$predict.risk > 0.5)
# accuracy
mean(test$not.fully == is.numeric(test$predict.risk > 0.5))
# baseline
mean(test$not.fully == 0)

# AUC using ROCR
library(ROCR)
roc1 <- prediction(test$predict.risk, test$not.fully.paid)
as.numeric(performance(roc1, "auc")@y.values) # 0.672

# a smarter baseline model: bivariate model with int.rate
baseline <- glm(not.fully.paid ~ int.rate, data=train, family="binomial")
summary(baseline)
pred.base <- predict(baseline, newdata=test, type="response")
sort(pred.base, dec=T)[1:5] # highest prediction is 0.4266240
# a 0.5 threshold would result in 0 not.fully.paid prediction
# AUC of baseline
roc.base <- prediction(pred.base, test$not)
as.numeric(performance(roc.base, "auc")@y.values) # 0.624

# explore the profit expectation
test$profit <- exp(test$int.rate*3)-1
test$profit[test$not.fully.paid==1] <- -1
maximun profit on $10 investment
10*max(test$profit)
summary(test$profit) # mean is 0.2094

# investment strategy of picking 100 high interest loans with lowest default prob.
summary(test$int.rate)
highinterest <- subset(test, int.rate > 0.15)
str(highinterest)
mean(highinterest$profit) # 0.2251015
t.high <- table(highinterest$not)
prop.table(t.high)

# select top 100 with lowest default prob. on prediction
#cutoff <- sort(highinterest$predict.risk, dec=F)[100]
#picks2 <- subset(highinterest, predict.risk <= cutoff)

picks <- highinterest[order(highinterest$predict.risk)[1:100],]
mean(picks$profit)
prop.table(table(picks$not))
summary(picks$profit)