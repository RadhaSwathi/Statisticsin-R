rm(list=ls())

totalN=200
arrivals<-c(0,1,2,3,4,5,6,7, 8)
obs.freq<-c(14,31,47,41,29,21,10,5,2)
obs.rel.freq<-c(14/200,31/200,47/200,41/200,29/200,21/200,10/200,5/200,2/200)

lambda<-c(t(arrivals)%*%obs.rel.freq)


expected<-totalN*((lambda^arrivals)*exp(-lambda))/factorial(arrivals)


chisq1 = (obs.freq - expected)^2/expected

goodness.of.fit = cbind(arrivals, obs.freq, expected)

colnames(goodness.of.fit) = c('Arrivals', 'Observed Counts', 'Expected Counts')
goodness.of.fit

chi.squared.statistic = sum(chisq1)
chi.squared.statistic

p.value = pchisq(chi.squared.statistic, length(obs.freq)-2, lower.tail = F)
p.value

plot(arrivals, obs.freq/totalN, xlim = c(0, 8), xlab = 'Number of Arrivals in a one-minute Interval', ylab = 'Proportion of Observed Counts', main = expression("Fitting the Poission Distribution to Arrivals"))

lines(arrivals, dpois(arrivals, lambda))




