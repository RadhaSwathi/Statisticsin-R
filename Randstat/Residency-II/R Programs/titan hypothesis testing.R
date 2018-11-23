rm(list=ls())

setwd('F:/Sridhar/PGPBA/Courses/SMDM/SMDM Mumbai')

titan<-read.csv('titaninsurance.csv',header=TRUE)
View(titan)

#### Variance test of two samples
###Do two samples come from populations with equal variancess?

var.test(titan$new,titan$old)


### Two-sample t-test with equal variances and equal means
t.test(titan$new,titan$old, var.equal=TRUE, paired=TRUE)

### Two-sample t-test with equal variances and equal means
t.test(titan$new,titan$old, var.equal=TRUE, alternative="greater", paired=TRUE)

### Two-sample t-test with equal variances and equal means
t.test(titan$new,titan$old,mu=5, var.equal=TRUE,  paired=TRUE)

power.t.test(n = 30, delta = 5, sd = 1, sig.level = 0.05,
             power = NULL,
             type = c("paired"),
             alternative = c("one.sided"),
             strict = FALSE, tol = .Machine$double.eps^0.25)
Arguments


