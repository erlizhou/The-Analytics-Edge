# Climate Change
climate = read.csv("climate_change.csv")
summary(climate)
training = subset(climate, Year <= 2006)
testing = subset(climate, Year > 2006)
linear <- lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols, data=training)
summary(linear)
cor(training)
linear2 <- lm(Temp~MEI+N2O+TSI+Aerosols, data=training)
summary(linear2)
newmodel <- step(linear)
summary(newmodel)
testpredict <- predict(newmodel, newdata=testing)
SSE <- sum((testing$Temp-testpredict)^2)
SST <- sum((testing$Temp-mean(training$Temp))^2)
1-SSE/SST

# READING TEST SCORES
pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")
nrow(pisaTrain)
summary(pisaTrain)
tapply(pisaTrain$readingScore, pisaTrain$male, mean)
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
nrow(pisaTrain)
nrow(pisaTest)
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)
(sum(lmScore$residuals^2)/nrow(pisaTrain))^0.5
predTest <- predict(lmScore, newdata = pisaTest)
max(predTest) - min(predTest)
SSE = sum((pisaTest$readingScore-predTest)^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE
mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore-mean(pisaTrain$readingScore))^2)
SST
1-SSE/SST

# DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA 
FluTrain <- read.csv("FluTrain.csv")
summary(FluTrain)
which.max(FluTrain$ILI)
FluTrain$Week[303]
which.max(FluTrain$Queries)
hist(FluTrain$ILI)
plot(log(FluTrain$ILI), FluTrain$Queries)
FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)
cor(log(FluTrain$ILI), FluTrain$Queries)^2
FluTest <- read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
?which
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]
(FluTest$ILI[11] - PredTest1[11]) / FluTest$ILI[11]
SSE1 = sum((PredTest1 - FluTest$ILI)^2)
RMSE1 = sqrt(SSE1/nrow(FluTest))
RMSE1
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
FluTrend2 <- lm(log(ILI) ~ log(ILILag2) + Queries, data = FluTrain)
summary(FluTrend2)
ILILag2Test = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2Test)
summary(FluTest$ILILag2)
nrow(FluTrain)
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
FluTest$ILILag2[1]
FluTest$ILILag2[2]
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE2 = sum((PredTest2 - FluTest$ILI)^2)
RMSE2 = sqrt(SSE2/nrow(FluTest))
RMSE2
