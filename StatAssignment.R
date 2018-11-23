
#Read data
rm(list =  ls())
setwd('/Users/rsklanu/Randstat/SRM')
Cash<-read.csv("cashtransfer.csv",header=TRUE)

#Summarize
summary(Cash)

#Data visualization
par(mfrow=c(2,2))
#Histogram
hist(Cash$bplcaloriebeforect)
hist(Cash$aplcaloriebeforect)
hist(Cash$bplcalorieafterct)
hist(Cash$aplcalorieafterct)
#boxplot
boxplot(Cash$bplcaloriebeforect)
boxplot(Cash$aplcaloriebeforect)
boxplot(Cash$bplcalorieafterct)
boxplot(Cash$aplcalorieafterct)

#Test
#question 1
t.test(Cash$bplcaloriebeforect,mu=2300,alternative = "greater")
t.test(Cash$bplcalorieafterct,mu=2300,alternative = "greater")
t.test(Cash$aplcaloriebeforect,mu=2300,alternative = "greater")
t.test(Cash$aplcalorieafterct,mu=2300,alternative = "greater")
#question 2
t.test(Cash$bplcaloriebeforect,Cash$bplcalorieafterct)
t.test(Cash$aplcaloriebeforect,Cash$aplcalorieafterct)
#question 3
t.test(Cash$bplcaloriebeforect,Cash$aplcaloriebeforect)
t.test(Cash$bplcalorieafterct,Cash$aplcalorieafterct)
#Variance
var.test(Cash$bplcaloriebeforect,Cash$aplcaloriebeforect,ratio=1)
var.test(Cash$bplcalorieafterct,Cash$aplcalorieafterct,ratio=1)
