rm(list=ls())
library(psych)

####Sampling distribution from a discrete distribution

##### We consider a discrete distribution
x = c(1, 3, 5)
px = c(0.6, 0.3, 0.1)

#### What is the mean and varaince of this distribution? Refer pages

expectedval<-c(t(x)%*%(px))
varianceval<-c(t((x-expectedval)^2)%*%(px))
stddeviation<-sqrt(varianceval)

#####################Simulation from this distribution
iterations = 1000
samplesize = 500 ### This is more important
draws <- matrix(ncol=iterations, nrow=samplesize)
means <- matrix(ncol=1, nrow=iterations)
stddev<- matrix(ncol=1, nrow=iterations)
varian<- matrix(ncol=1, nrow=iterations)
for(i in 1:iterations){
draws[,i] <-sample(x, size = samplesize, replace = TRUE, prob = px)
means[i,1]<-mean(draws[,i])
stddev[i,1]<-sd(draws[,i])
varian[i,1]<-var(draws[,i])
}

###### This step illustrates that the mean of all sample means is almost the same as population mean
###### This step also illustrates that the sample means are normally distributed with mean 
###### equal to that of population mean and variance equal to population variance divided by size of the sample
######
describe(means)
hist(means, main = "500 discrete draws")
summary(means)
qqnorm(means)



boxplot(means)
boxplot(stddev)

################## Standard deviation of sample means
stdev1<-sd(means)
stdev2<-stddeviation/sqrt(samplesize)

sdmean<-mean(stddev)

varmean<-mean(varian)
summary(varian)
hist(varian)
describe(varian)

sddev<-sd(stddev)
hist(stddev)
describe(stddev)

##########################################
###Generate random variables from uniform Distribution with min=10 and max=20

#####################Simulation from this distribution
unifmean<-(10+20)/2
unifvariance<-(20-10)^2/12
unifstddev<-sqrt(unifvariance)

iterations = 1000
samplesize = 500 ### This is more important
udraws <- matrix(ncol=iterations, nrow=samplesize)
umeans <- matrix(ncol=1, nrow=iterations)
ustddev<- matrix(ncol=1, nrow=iterations)
for(i in 1:iterations){
  udraws[,i] <-runif(samplesize,min=10,max=20)
  umeans[i,1]<-mean(udraws[,i])
  ustddev[i,1]<-sd(udraws[,i])
}

###### This step illustrates that the mean of all sample means is almost the same as population mean
###### This step also illustrates that the sample means are normally distributed with mean 
###### equal to that of population mean and variance equal to population variance divided by size of the sample
######
describe(umeans)
hist(umeans, main = "500 discrete draws")
summary(umeans)
boxplot(umeans)

###############Verify mean of sample means is the same as population mena
difference<-unifmean-mean(umeans)
difference



################## Standard deviation of sample means
ustdev1<-sd(umeans)
ustdev2<-unifstddev/sqrt(samplesize)
stddifference<-ustdev1-ustdev2
stddifference
##########################################


