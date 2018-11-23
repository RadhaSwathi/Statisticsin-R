rm(list=ls())



setwd("F:/Sridhar/PGPBDA/Hypothesis Testing/Data Files")

x <- rnorm(10)
y <- rnorm(10)
ttest <- t.test(x,y)
names(ttest)
ttest$statistic
ttest$parameter
ttest$estimate
ttest$null.value
ttest$alternative
ttest$method
#### Internet Data
mobiletime<-read.csv("InternetMobileTime.csv",header=TRUE)
######## One-Sample (Large) two-tail Test


t.test(mobiletime$Minutes, mu=144)

t.test(mobiletime$Minutes, mu=144, alternative="greater")

t.test(mobiletime$Minutes, mu=250, alternative="less")

####Two Sample Test Using Luggage data
luggage<-read.csv("Luggage.csv", header=TRUE)

###Step 1. Test of Variances. Uses ratio of two variances, so it is an F Test.
var.test(luggage$WingA,luggage$WingB)

####Step 2. Test of Means with Equal variances
t.test(luggage$WingA,luggage$WingB, var.equal=TRUE, paired=FALSE)

####Step 2. Test of Means with unequal variances
t.test(luggage$WingA,luggage$WingB)


### Paired Sample Test:
concrete<-read.csv("Concrete1.csv",header=TRUE)
t.test(concrete$Two.days,concrete$Seven.days, paired=TRUE)


###Using Cash Transfer Data

####Confidence interval for $t-distribution with n=5 degrees of freedom
dt(0.5, df=50)
pt(1.96, df=50)
1 - pt(1:5, df = 1)
qt(c(.025, .975), df=5)
dt(c(.025, .975), df=5)

qt(.975, df = c(10,20,50,100,1000))
dt(.975, df = c(10,20,50,100,1000))

#### One-Sample (Large) two-tail Test
t.test(stockret$rtsensex,mu=0.002) # Ho: mu=0.0075

#### One-Sample (Large) one-tail Test
t.test(stockret$rtwipro, mu=0.0075, alternative="greater")

t.test(stockret$rtwipro, mu=0.0075, alternative="less")

####One sample test at a different confidence level
t.test(stockret$rtwipro, mu=0.0075, conf.level=0.99)



### Two-sample t-test with equal variances
t.test(stockret$rttata,stockret$rtwipro, var.equal=TRUE, paired=FALSE)


#### Variance test of two samples
###Do two samples come from populations with equal variancess?

var.test(stockret$rttata,stockret$rtwipro)


#### Two-sample t-test with unequal varainces
t.test(stockret$rttata,stockret$rtwipro)

t.test(stockret$rttata,stockret$rtwipro,mu=0.0075, alternative="greater")


#### Proportion test

#####One Sample T-test:

result1 <- prop.test(x = 95, n = 160, p = 0.5)
result1 

##### One Sample T-Test, one sided
result1a <- prop.test(x = 95, n = 160, p = 0.5, alternative="greater")
result1a 

##### Two Samples of Smokers across two countries India, and China


result2 <- prop.test(x = c(490, 400), n = c(500, 500))
result2

####Car Crash Example

survivors <- matrix(c(1781,1443,135,47), ncol=2)
colnames(survivors) <- c('survived','died')
rownames(survivors) <- c('no seat belt','seat belt')
survivors

result.prop <- prop.test(survivors)
result.prop 



library(MASS)         # load the MASS package 
head(quine) 
table(quine$Eth, quine$Sex)

####Problem: Assuming that the data in quine follows the normal distribution, 
####find the 95% confidence interval estimate of the difference between the 
####female proportion of Aboriginal students and the female proportion of Non-Aboriginal 
#####students, each within their own ethnic group.

prop.test(table(quine$Eth, quine$Sex), correct=FALSE) 

smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )
prop.test(smokers, patients)


ts = replicate(1000,t.test(rnorm(10),rnorm(10))$statistic)
range(ts)

t.power = function(nsamp=c(10,10),nsim=1000,means=c(0,0),sds=c(1,1)){
  lower = qt(.025,df=sum(nsamp) - 2)
  upper = qt(.975,df=sum(nsamp) - 2)
  ts = replicate(nsim,
                 t.test(rnorm(nsamp[1],mean=means[1],sd=sds[1]),
                        rnorm(nsamp[2],mean=means[2],sd=sds[2]))$statistic)
  
  sum(ts < lower | ts > upper) / nsim
}

t.power(means=c(0,1))



stockret<-read.csv('SENSEX.csv', header=TRUE)


delta2<-mean(mobiletime$Minutes)-144
stdev<-sd(mobiletime$Minutes)
power.t.test(n=30,delta=delta2,sd=stdev,sig.level=0.05,type="one.sample",alternative="one.sided")


