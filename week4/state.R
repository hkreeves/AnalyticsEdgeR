##
## Week4 HW: state
##


data(state)
statedata <- data.frame(state.x77)
str(statedata)

# linear regression
lm1 <- lm(Life.Exp ~ ., data=statedata)
summary(lm1) # adj R^2 0.6922

# SSE
sum((statedata$Life - predict(lm1))^2) 

lm2 <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=statedata)
summary(lm2) # adj R^2 0.7126
# SSE
sum((statedata$Life - predict(lm2))^2) 

# CART
set.seed(334)
tree1 <- rpart(Life.Exp ~ ., data=statedata, method="anova")
printcp(tree1)
prp(tree1)
# SSE
sum((statedata$Life - predict(tree1))^2) 

# CART with minibucket = 5
tree2 <- rpart(Life.Exp ~ ., data=statedata, method="anova", minbucket=5)
prp(tree2)
# SSE
sum((statedata$Life - predict(tree2))^2)

# overfitted tree
tree3 <- rpart(Life.Exp ~ Area, data=statedata, minbucket=1)
sum((statedata$Life - predict(tree3))^2)

# cross validation
library(caret)

set.seed(111)
fitControl <- trainControl(method="cv", 10)
cvGrid <- expand.grid(cp=seq(0.01, 0.50, by=0.01))
tree4 <- train(Life.Exp ~ ., data=statedata, method="rpart", trControl=fitControl, tuneGrid=cvGrid) 
print(tree4)
prp(tree4$finalModel)
# SSE
sum((statedata$Life - predict(tree4$finalModel))^2)

# cv for overfitted tree
set.seed(111)
tree5 <- train(Life.Exp ~ Area, data=statedata, method="rpart", trControl=fitControl, tuneGrid=cvGrid)
tree5
prp(tree5$finalModel)
sum((statedata$Life - predict(tree5$finalModel))^2)
