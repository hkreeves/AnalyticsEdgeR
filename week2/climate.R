##
## Week2 Climate Change
##

# load data and split
climate <- read.csv("climate_change.csv")
climate.test <- subset(climate, Year > 2006)
climate <- subset(climate, Year <= 2006)
str(climate)

# build lm on training data
lm1 <- lm(Temp ~ . - Year - Month, data=climate)
summary(lm1)

SSE <- sum(lm1$residuals^2)
SST <- sum((climate$Temp-mean(climate$Temp))^2)
R2 <- 1 - SSE/SST
RMSE <- sqrt(SSE/nrow(climate))

# multicolinearity
cor(climate$N2O, climate)
cor(climate$CFC.11, climate)

# remove variables
lm2 <- lm(Temp ~ N2O + MEI + TSI + Aerosols, data=climate)
summary(lm2) 

# use step() to automate the vairable selection
lmStep <- step(lm1)

# predict with lmStep
pred <- predict(lmStep, newdata=climate.test)
SSE.test <- sum((pred-climate.test$Temp)^2)
RMSE.test <- sqrt(SSE.test/nrow(climate.test))
# SST is based on the "baseline" model trained from the training set
SST.test <- sum((mean(climate$Temp)-climate.test$Temp)^2)
R2.test <- 1 - SSE.test/SST.test