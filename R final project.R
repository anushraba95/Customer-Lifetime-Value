library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)


data<-read.csv(file.choose())
str(data)
summary(data)
dim(data)
View(data)
colnames(data)[3]<-"clv"

quantile(data$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
boxplot(data$clv)
data1<-data[data$clv <36000,]
nrow(data)
nrow(data1)
nrow(data)-nrow(data1)
quantile(data1$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
data2<-data1[data1$clv <14722, ]

boxplot(data2$clv)
nrow(data)-nrow(data2)
str(data2)

as.data.frame(colSums(is.na(data2)))

data3<-data2[,-c(1,2,7)]
set.seed(123)
spl<-sample.split(data3$clv, 0.7)
trainingdata<-subset(data3, spl == TRUE)

str(trainingdata)
dim(trainingdata)

testdata<-subset(data3, spl == FALSE)
str(testdata)
dim(testdata)

lnmod=lm(clv~.,data=trainingdata)
summary(lnmod)

lnmod1=lm(clv~Response+	Coverage+	Education+	EmploymentStatus+	Gender+	Income+	Location.Code+	Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+	Months.Since.Policy.Inception+	Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	Sales.Channel+	Total.Claim.Amount+	Vehicle.Class+	Vehicle.Size, data=trainingdata)
summary(lnmod1)

lnmod2=lm(clv~	Coverage+	Education+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	Vehicle.Class, data=trainingdata)
summary(lnmod2)

lnmod3=lm(clv~	Coverage+	Education+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV"), data=trainingdata)
summary(lnmod3)

lnmod4=lm(clv~	Coverage+	I(Education=="College")+I(Education =="High School or Below")+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV"), data=trainingdata)
summary(lnmod4)

fnlmod=lm(clv~	Coverage+	I(EmploymentStatus=="Unemployed")+	 Monthly.Premium.Auto + Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV"), data=trainingdata)
summary(fnlmod)

vif(fnlmod)
fitted(fnlmod)

par(mfrow=c(2,2))
plot(fnlmod)

trainingdata$pred <- fitted(fnlmod)
MAPE<-print((sum((abs(trainingdata$clv-trainingdata$pred))/trainingdata$clv))/nrow(trainingdata))

durbinWatsonTest(fnlmod)
dwt(fnlmod)

bptest(fnlmod)             

resid <- fnlmod$residuals
par(mfrow=c(1,1))
plot(density(resid))
ad.test(resid) 
pearson.test(resid)
par(mfrow=c(1,1))
qqnorm(resid)

clvPred <- predict(fnlmod,testdata) 

actuals_preds <- data.frame(cbind(actuals=testdata$clv, predicteds=clvPred))
correlation_accuracy <- cor(actuals_preds)
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)

f1<-lm(clv~Coverage+I(EmploymentStatus=="Unemployed")+ Monthly.Premium.Auto+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+I(Vehicle.Class=="SUV"),data=data3)
summary(f1)


vif(f1)
durbinWatsonTest(f1)
bptest(f1)
resid2 <- f1$residuals
par(mfrow=c(1,1))
plot(density(resid2))
ad.test(resid2) 
pearson.test(resid2)
par(mfrow=c(1,1))
qqnorm(resid2)

par(mfrow=c(2,2))
plot(f1)


