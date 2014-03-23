##
## Week3 HW: World Series
##

ws <- read.csv("baseball.csv")
str(ws)

length(table(ws$Year))

# subset the playoff samples
playoff <- subset(ws, Playoffs == 1)
dim(playoff)

# number of teams in playoff
playoff.table <- table(playoff$Year)

# adding new variable NumCompetitors
playoff$NumCompetitors <- playoff.table[as.character(playoff$Year)]
table(playoff$NumCom)

# extract from RankPlayoffs the binary variable WinWorldSeries
playoff$WinWorldSeries <- as.numeric(playoff$RankPlayoffs == 1)
table(playoff$Win)

# build bivariate models to check significance of each of the 12 dep. vars
vars <- names(playoff)[-c(1,10,12,13,17)]
#[1] "League"         "Year"           "RS"             "RA"             "W"              "OBP"           
#[7] "BA"             "RankPlayoffs"   "G"              "OOBP"           "OSLG"           "NumCompetitors"

results <- list()
for(var in vars){
	res <- glm(paste("WinWorldSeries ~", var), data=playoff, family=binomial)
	results <- c(results, list(res)) ## append a list to a listOflist
}
sapply(results, summary)

# correlation
cor(playoff[,vars[2:12]]) # Year and NumComp is highly correlated (0.91)

# multivariate model
# the following are significant based on the bivariate model
formulas <- c("Year", "RA", "RankSeason", "NumCompetitors")
formula <- "WinWorldSeries ~ Year + RA + RankSeason + NumCompetitors"
res2 <- glm(formula, data=playoff, family=binomial)
summary(res2) # none of the dep. vars is significant...

# a new model forumla list
# use a loop to make the construction
for(i in 1:3){
  for(j in (i+1):4){
    formulas <- c(formulas, paste(formulas[i], "+", formulas[j]))
  }
}

new.results <- list()
for(formula in formulas){
  res <- glm(paste("WinWorldSeries ~", formula), data=playoff, family=binomial)
  new.results <- c(new.results, list(res))
}
sapply(new.results, summary)
aics <- sapply(new.results, function(res) res$aic)
names(aics) <- formulas