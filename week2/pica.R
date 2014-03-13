##
## Week2 HW PISA
##

# load data
pica <- read.csv("pisa2009Train.csv")
pica.test <- read.csv("pisa2009Test.csv")
str(pica)

# average on male
tapply(pica$reading, pica$male, mean)

# missing data
summary(pica)
pica <- na.omit(pica)
pica.test <- na.omit(pica.test)
dim(pica)
dim(pica.test)

# ordered - unordered factors
levels(pica$raceeth)
pica$raceeth <- relevel(pica$raceeth, ref="White")
#pica$grade <- factor(pica$grade, ordered=T)

pica.test$raceeth <- relevel(pica.test$raceeth, ref="White")

# linear regression
lm1 <- lm(readingScore ~ ., data=pica)
summary(lm1)

# rmse (loading "medley")
pred <- predict(lm1)
RMSE <- rmse(pica$readingScore, pred) # 73.37
#RMSE <- sqrt(mean(lm1$residuals^2))

# predict
pred.test <- predict(lm1, newdata=pica.test)
summary(pred.test)
max(pred.test)-min(pred.test) # 284.4683

SSE.test <- sum((pred.test - pica.test$readingScore)^2) # 5762082
RMSE.test <- rmse(pred.test, pica.test$readingScore) #  76.29079

# baseline model
base <- mean(pica$readingScore) # 517.9629
SST.test <- sum((base - pica.test$readingScore)^2)
SST.test

R2.test <- 1 - SSE.test/SST.test; R2.test
