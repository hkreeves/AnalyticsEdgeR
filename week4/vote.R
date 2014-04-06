##
## Week4 HW: vote study
##

library(rpart)
library(rpart.plot)

gerber <- read.csv("gerber.csv")
str(gerber)
summary(gerber)

# which treatment group has largest fraction of voters
# create a new feature: type, indicates 1-control, 2-haw, 3-civic, 4-neigh, 5-self
type <- (gerber$control + gerber$haw*2 + gerber$civic*3 + gerber$neigh*4
		+gerber$self*5)  
gerber$type <- factor(type)

tapply(gerber$voting, gerber$type, mean)
#        1         2         3         4         5 
#0.2966383 0.3223746 0.3145377 0.3779482 0.3451515 

# logistic regression
lg1 <- glm(voting ~ civicduty + hawthorne + neighbors + self, data=gerber,
		family="binomial")
summary(lg1)

# use a threshold 0.3, what is the accuracy
pred <- predict(lg1, type="response")
table(gerber$voting, pred>0.3)
mean(gerber$voting==(pred>0.3))
mean(gerber$voting==(pred>0.5))
mean(gerber$voting)

# CART regression tree
tree1 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(tree1)
tree2 = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(tree2)

# interaction term
tree3 <- rpart(voting ~ control, data=gerber, cp=0.0)
tree4 <- rpart(voting ~ sex + control, data=gerber, cp=0.0)
prp(tree3, digit=6)
prp(tree4, digit=7)

lg2 <- glm(voting ~ sex + control, data=gerber, family="binomial")
lg3 <- glm(voting ~ sex * control, data=gerber, family="binomial")
summary(lg2)

prob = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(lg2, newdata=prob, type="response")
predict(lg3, newdata=prob, type="response")