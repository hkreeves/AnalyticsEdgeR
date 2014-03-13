##
## Week1 HW: Stock Dynamics
##

ibm <- read.csv("IBMStock.csv")
ge <- read.csv("GEStock.csv")
pg <- read.csv("ProcterGambleStock.csv")
coca <- read.csv("CocaColaStock.csv")
boeing <- read.csv("BoeingStock.csv")

# dim
sapply(list(ibm,ge, pg, coca, boeing), nrow) # 480 *4

ConvDate <- function(d){
as.Date(d$Date, '%m/%d/%y')
}
ibm$Date <- ConvDate(ibm)
ge$Date <- ConvDate(ge)
pg$Date <- ConvDate(pg)
coca$Date <- ConvDate(coca)
boeing$Date <- ConvDate(boeing)

# merge all
stock <- cbind(ibm, ge[,2], pg[,2], coca[,2], boeing[,2])
names(stock) <- c("date", "ibm", "ge", "pg", "coca", "boeing")

# earlier year and latest
min(ibm$Date)
max(ibm$Date)

# stats of stock prices
summary(stock)
sd(stock$pg)

## Plot Stock price
plot(stock$date, stock$coca, type="l", xlab="Time", ylab="Stock Price ($)",
main="Coca Cola Stock")
lines(stock$date, stock$pg, col="red")
abline(v=as.Date(c("2000-03-01")), lwd=2)

plot(stock$date, stock$ibm, type="l", xlim=c(as.Date("1997-01-01"), as.Date("1998-01-01")),
ylim=c(0,210), xlab="Time", ylab="Stock Price ($)")
lines(stock$date, stock$ge, col="red")
lines(stock$date, stock$pg, col="blue")
lines(stock$date, stock$coca, col="green")
lines(stock$date, stock$boeing, col="pink")
legend("topright", col=c("black", "red", "blue", "green", "pink"), lty=1, pch=NA,
legend=names(stock)[2:6])
abline(v=as.Date(c("1997-01-01", "2006-01-01")))

## unadjusted stock price
plot(ibm2$Date, ibm2$StockPrice, type="l", 
xlim=as.Date(c("1978-01-01", "2011-01-01")),
xlab="Time", ylab="Stock Price ($)",
main="IBM Stock")

## adjusted stock price
ibm2 <- ibm
ibm2$StockPrice[ibm2$Date < as.Date("1979-06-01")] <- ibm2$StockPrice[ibm2$Date < as.Date("1979-06-01")]/4
ibm2$StockPrice[ibm2$Date < as.Date("1997-06-01")] <- ibm2$StockPrice[ibm2$Date < as.Date("1997-06-01")]/2
ibm2$StockPrice[ibm2$Date < as.Date("1999-06-01")] <- ibm2$StockPrice[ibm2$Date < as.Date("1999-06-01")]/2

## month-wise study
ibmByMonth <- tapply(stock$ibm, months(stock$date), mean) 
sort(ibmByMonth)
geByMonth <- tapply(stock$ge, months(stock$date), mean) 
sort(geByMonth)
cocaByMonth <- tapply(stock$coca, months(stock$date), mean) 
sort(cocaByMonth)

which(stock$date==as.Date("1997-09-01")) # 333
which(stock$date==as.Date("1997-12-01")) # 336
stock[333:336,]