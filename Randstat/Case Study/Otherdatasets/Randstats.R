summary(CEO)
getwd()

pnorm(1.5,0,1)-pnorm(-1.5,0,1)

rm(list =  ls())
setwd('/Users/rsklanu/Randstat/Case Study/Otherdatasets')
browtime<-read.csv("InternetMobileTime.csv",header=TRUE)

## summarize
summary(browtime)
variance<-var(browtime$Minutes)
variance
stder<-sqrt(variance/length(browtime$Minutes))
stder
##Data visualization
boxplot(browtime$Minutes)
hist(browtime$Minutes)

##Hypothesis testing
t.test(browtime$Minutes,mu=125)

## day2 

rm(list =  ls())
setwd('/Users/rsklanu/Randstat/Case Study/Otherdatasets/Travel Management')
travel<-read.csv("CaseStudyXYZTraveldomestic.csv",header=TRUE)

summary(travel)
str(travel)
##factor(travel$CarrierAA,levels = c(1,2),)
tapply(travel$amount,travel$CarrierAA,mean)

##visualize
boxplot(travel$amount~travel$CarrierAA)
par(mfrow=c(1,1))
hist(travel$amount[travel$CarrierAA=="UNITED AIRLINES"])
hist(travel$amount[travel$CarrierAA=="AMERICAN AIRLINES"])

travel$amount[travel$amount>700]<-700

##two sameple t-test
t.test(travel$amount~travel$CarrierAA,mu=0)


x<-0:200
d<-dchisq(x,df=50,ncp=0)
plot(x,d,type="l")

318/44


##F - test for variance
tapply(travel$amount,travel$CarrierAA,var)
var.test(travel$amount~travel$CarrierAA,ratio=1)
var.test(travel$amount~travel$CarrierAA,ratio=1,alternative="greater")


## hotel

rm(list =  ls())
setwd('/Users/rsklanu/Randstat/Case Study/Otherdatasets/Travel Management')
travel<-read.csv("Case Study Hotel Tariff Comparison.csv",header=TRUE)
summary(travel)
par(mfrow=c(2,2))
boxplot(travel$mpdrev)
boxplot(travel$bpdrev)
hist(travel$mpdrev)
hist(travel$bpdrev)

##hypothesis test
t.test(travel$mpdrev,mu=3000,alternative = "greater")
t.test(travel$bpdrev,mu=3000,alternative = "greater")


t.test(travel$mpdrev,mu=4000,alternative = "less")
t.test(travel$bpdrev,mu=4000,alternative = "less")

## Variance test
var.test(travel$bpdrev,travel$mpdrev,ratio=1,alternative = "greater")

## two sample / welch test
t.test(travel$bpdrev,travel$mpdrev,mu=0)

##test for normality
shapiro.test(travel$mpdrev)
shapiro.test(travel$bpdrev)

## Proption test

prop.test()
?prop.test

rm(list =  ls())
install.packages('MASS')
setwd('')
titanic<-read.csv("titanic.csv",header = TRUE)

