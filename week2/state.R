##
## Week2 HW: State
##

# load data
data(state)
statedata = cbind(data.frame(state.x77), 
state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)

# plot location
plot(statedata$x, statedata$y)
# symbol plot that has varying symbol size
symbols(statedata$x, statedata$y, circles=sqrt(statedata$Area/1000),
inches=1/3, ann=F, bg="steelblue2", fg=NULL)

# region average high school rate
tapply(statedata$HS.Grad, statedata$state.region, mean)

# boxplot of muder ~ region
boxplot(Murder ~ state.region, data=statedata)
# find outlier in Northeast
ne <- subset(statedata, state.region=="Northeast")
ne[which.max(ne$Murder),] ## New York is the answer

# predict life.Exp
lm1 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad
+ Frost + Area, data=statedata)
summary(lm1)

# plot Life ~ Income
plot(statedata$Income, statedata$Life, pch=19, col="steelblue2")

# refine model by removing Area (largest p-value), then Illiteracy, then Income
lm2 <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=statedata)
summary(lm2)

# predict with lm2
pred.life <- predict(lm2)
# lowest life.exp
sort(pred.life)
statedata$state.name[which.min(statedata$Life)]
statedata$state.name[which.max(statedata$Life)]

# examin residuals
resid <- pred.life - statedata$Life
sort(abs(resid))
plot(resid)