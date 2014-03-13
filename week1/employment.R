##
## Week1 HW: Employment
##

cps <- read.csv("CPSData.csv")
str(cps)
dim(cps)

# popular industry
sort(table(cps$Industry))

# states
sort(table(cps$State), dec=T)[1:5]
sort(table(cps$State))[1:5]

prop.table(table(cps$Citi))

table(cps$His, cps$Race)

# missing values
summary(cps)
apply(cps, 2, function(x) any(is.na(x))) 

# missing values in marriage status
prop.table(table(cps$Region, is.na(cps$Married)), 1)
prop.table(table(cps$Sex, is.na(cps$Married)), 1)
prop.table(table(cut(cps$Age,c(0,20,40,60,100)), is.na(cps$Married)), 1) # young people relate
prop.table(table(cps$Citi, is.na(cps$Married)), 1)

# missing values in metroCode
t <- prop.table(table(cps$State, is.na(cps$Metro)), 1)
t[t[,2]==1, ]
t[t[,2]==0, ]
rownames(t)[which.min(abs(t[,2]-0.3))]

# region based
prop.table(table(cps$Region, is.na(cps$Metro)), 1)

sort(tapply(is.na(cps$Metro), cps$State, mean))

# metro and country
metro <- read.csv("MetroAreaCodes.csv", stringsAsFactors=F)
country <- read.csv("CountryCodes.csv", stringsAsFactors=F)

str(metro); str(country)

# join tables
cps <- merge(cps, metro, by.x="MetroAreaCode", by.y="Code", all.x=T)
str(cps)
sum(is.na(cps$MetroArea))

# frequency in MetroArea
sort(table(cps$MetroArea), dec=T)[1:20]

# proportion of Hispanic in MetroArea
sort(tapply(cps$Hispanic, cps$MetroArea, mean), dec=T)[1:10]

# Asian prop > 0.20
asian.prop <- tapply(cps$Race=="Asian", cps$MetroArea, mean)
asian.prop[asian.prop > 0.2]

# high-school diploma prop
hs.prop <- tapply(cps$Education == "No high school diploma", cps$MetroArea,
mean, na.rm=T)
sort(hs.prop)[1]

## join the country code table
cps2 <- merge(cps, country, by.x="CountryCode", by.y="Code", all.x=T)
str(cps)

sum(is.na(cps$Country))

# most common country
sort(table(cps$Country), dec=T)[1:5]
prop.table(table(cps$Country != "United States", cps$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA"), 2)

# largest prop of indian(Brazil, ...)
sort(tapply(cps$Country=="India", cps$MetroArea, sum, na.rm=T), dec=T)[1]
sort(tapply(cps$Country=="Brazil", cps$MetroArea, sum, na.rm=T), dec=T)[1]
sort(tapply(cps$Country=="Somalia", cps$MetroArea, sum, na.rm=T), dec=T)[1]
