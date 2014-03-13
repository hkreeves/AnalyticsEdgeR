##
## edX The Analytics Edge
## Week 1 HW
##

## Problem 1: Motor Vehicle Theft

mvt <- read.csv("mvtWeek1.csv", stringsAsFactors=F)
str(mvt)

## dim of dataset
dim(mvt)

## max id
max(mvt$ID)

## min beat
min(mvt$Beat)

## how many arrest
table(mvt$Arrest, useNA="ifany")

## how many locations are "Alley"
sort(table(as.factor(mvt$LocationDescription)), dec=T)[1:10]
length(grep("^ALLEY$", mvt$LocationDescription))
## there is another value "BOWLING ALLEY"
unique(mvt$LocationDescription[grep("ALLEY", mvt$LocationDescription)])

#### Problem 2
head(mvt$Date)

## convert date strings to timestamps
mvt$Date <- as.Date(strptime(mvt$Date, format="%m/%d/%y %H:%M"))

## median date
median(mvt$Date)

## add Month and weekday
mvt$Month <- months(mvt$Date)
mvt$Weekday <- weekdays(mvt$Date)
barplot(sort(table(mvt$Month)), cex.names=0.6)
barplot(sort(table(mvt$Weekday)), cex.names=0.6)

## which month has highest number of arrest-made thefts
barplot(sort(table(mvt$Month[mvt$Arrest])), cex.names=0.6)
tab <- table(mvt$Arrest, mvt$Month)
tab <- tab[,order(table(mvt$Month))]
barplot(tab, cex.names=0.6, col=c("red", "orange"),
legend=c("Unar", "Arr"))
sort(table(mvt$Month[mvt$Arrest]))

#### Problem 3
## time trend
hist(mvt$Date, breaks=100)

## time trend of arrest
boxplot(Date ~ Arrest, data=mvt)

## arrests each year
tabyear <- table(mvt$Arrest, mvt$Year)
tabPortion <- sweep(tabyear, 2, colSums(tabyear), FUN="/")
## or simply
prop.table(tabyear, 2)

#### Problem 4
## sort location popularity
top5Loc <- sort(table(mvt$LocationDescription, exclude="OTHER"), dec=T)[1:5]

## create a subset for top5 loc. only
top5 <- mvt[mvt$LocationDescription %in% names(top5Loc),]
nrow(top5)

## arrest rate in top5
top5$LocationDescription <- factor(top5$LocationDescription)
table(top5$LocationDescription[top5$Arrest])/table(top5$LocationDescription)

## weekday thefts at gas station
table(top5$Weekday[top5$LocationDescription=="GAS STATION"])

table(top5$Weekday, top5$LocationDescription)