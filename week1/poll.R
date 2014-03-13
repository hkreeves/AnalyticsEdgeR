##
## Week1 HW: Anonymity Poll
##

poll <- read.csv("AnonymityPoll.csv")
str(poll)
summary(poll)
poll$State <- as.character(poll$State)

# how many smartphone
table(poll$Smartphone, useNA="ifany")

# state & region
unique(poll$State[poll$Region=="Midwest"])
sort(table(poll$State[poll$Region=="South"]))

# internet & smartphone use
table(poll$Internet.Use, poll$Smartphone, useNA="ifany")

# subset of internet-and/or-smartphone users
limited <- subset(poll, Internet.Use==1 | Smartphone==1)
table(limited$Internet.Use, limited$Smartphone, useNA="ifany")
nrow(limited)

# NA in limited
summary(limited)
apply(limited, 2, function(x) any(is.na(x)))
table(limited$Info)
# proportion
prop.table(table(limited$Worry, limited$Info), 2)
prop.table(table(limited$Worry))
prop.table(table(limited$Anon))
prop.table(table(limited$Tried))
prop.table(table(limited$Privacy.Laws))

# demographic
hist(limited$Age, breaks=seq(0,100, 10))
agegroup <- cut(limited$Age, seq(0,100,10))
boxplot(limited$Info ~ agegroup)

length(unique(limited$Age)) # 72
t <- table(limited$Age, limited$Info) # a table 72 by 11
which(t==max(t), arr.ind=T)

plot(jitter(limited$Age), jitter(limited$Info), pch=19, col="orange")

# smartphone ~ info online
tapply(limited$Info, limited$Smart, summary)
tapply(limited$Tried, limited$Smart, summary)
prop.table(table(limited$Tried, limited$Smart),2)