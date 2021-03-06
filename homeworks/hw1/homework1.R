# An analytical detective
setwd("C:/Users/erlizhou/Desktop/MOOC/The_Analytics_Edge/homeworks/hw1")
mvt = read.csv("mvtWeek1.csv")
nrow(mvt)
ncol(mvt)
max(mvt$ID)
min(mvt$Beat)
summary(mvt)
mvt$Date[1]
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Month)
table(mvt$Weekday)
table(mvt$Arrest,mvt$Month)
hist(mvt$Date, breaks=100)
boxplot(mvt$Date~mvt$Arrest)
table(mvt$Arrest,mvt$Year)
sort(table(mvt$LocationDescription))
Top5 = subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")
nrow(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Arrest)
table(Top5$LocationDescription, Top5$Weekday)

# Stock dynamics
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CoCacolaStock.csv")
Boeing = read.csv("BoeingStock.csv")
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
nrow(IBM)
summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)
sd(ProcterGamble$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice)
lines(ProcterGamble$Date, ProcterGamble$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice, col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-06-01")), lwd=3)
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)
abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-12-31")), lwd=2)
tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(ProcterGamble$StockPrice, months(CocaCola$Date), mean)
tapply(Boeing$StockPrice, months(CocaCola$Date), mean)

# DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES
CPS = read.csv("CPSData.csv")
str(CPS)
summary(CPS)
sort(table(CPS$State))
table(CPS$Race, CPS$Hispanic)
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))
table(CPS$State, is.na(CPS$MetroAreaCode))
table(CPS$Region, is.na(CPS$MetroAreaCode))
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")
nrow(MetroAreaMap)
nrow(CountryMap)
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)
sort(table(CPS$MetroArea))
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
sort(tapply(CPS$Race=="Asian", CPS$MetroArea, mean))
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
summary(CPS)
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")
sort(tapply(CPS$Country == "India", CPS$MetroArea, mean, na.rm=TRUE))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, mean, na.rm=TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, mean, na.rm=TRUE))
