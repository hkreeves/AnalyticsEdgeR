##
## Week2 HW: Flu Trend
##

# load data
flu <- read.csv("FluTrain.csv")
flu.test <- read.csv("FluTest.csv", stringsAsFactors=F)
str(flu)
flu$Week <- as.character(flu$Week)

plot(flu$Queries, flu$ILI, pch=1, col="steelblue2", xlab="Queries",
ylab="ILI-related physician visits")

# highest week
flu$Week[which.max(flu$ILI)]
flu$Week[which.max(flu$Queries)]

# histograms
hist(flu$ILI) # skew right
hist(flu$Queries)

# log(ILI) ~ Queries
plot(flu$Queries, log(flu$ILI), pch=1, col="steelblue2", xlab="Queries",
ylab="natural log of ILI-related physician visits")

# linear regression
lm1 <- lm(log(ILI) ~ Queries, data=flu)
summary(lm1)
abline(lm1)

# R^2 and correlation
R2 <- 0.709
corr <- cor(log(flu$ILI), flu$Queries)

# prediction
pred <- exp(predict(lm1, newdata=flu.test))

# What is our estimate for the percentage 
# of ILI-related physician visits for the week of March 11, 2012
res <- pred[grep("^2012-03-11", flu.test$Week)]
act <- flu.test$ILI[grep("^2012-03-11", flu.test$Week)]
rel.err <- (act-res)/act

# rmse
rmse <- sqrt(mean((pred-flu.test$ILI)^2)) # 0.7490645

# time series model
install.packages("zoo")
library(zoo)
# extract date from data
flu$WeekStart <- as.Date(sapply(flu$Week, 
function(x) strsplit(x, split=" - ")[[1]][1]))

# lag data by 2 weeks (-2)
ili.lag2 = lag(zoo(flu$ILI), -2, na.pad=T)
flu$ILILag2 <- coredata(ili.lag2)
sum(is.na(flu$ILILag2))

# train a lm model using Queries and ILILag2
# Question: ILILag is shifted and should still be highly correlated with ILI
# and Queries. In fact, cor(flu$Queries, flu$ILILag2, use="complete.obs") givs
# 0.707
lm.lag <- lm(log(ILI) ~ Queries + log(ILILag2), data=flu)
summary(lm.lag)

# process test data in the same way
ili.lag2 = lag(zoo(flu.test$ILI), -2, na.pad=T)
flu.test$ILILag2 <- coredata(ili.lag2)
sum(is.na(flu.test$ILILag2))

# fill in the missing values in test set
flu.test$ILILag2[1:2] <- flu$ILI[(nrow(flu)-1):nrow(flu)]

# prediction using lm.lag
pred <- exp(predict(lm.lag, newdata=flu.test))
rmse <- sqrt(mean((pred-flu.test$ILI)^2)) # 0.2942029 much better than lm1

## EXTRA: compare lm1 with a poisson regression?
poisson1 <- glm(ILI ~ Queries, data=flu, family="poisson")
