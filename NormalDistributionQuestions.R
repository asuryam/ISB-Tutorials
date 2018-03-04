set.seed(789)
xseq<-seq(-4,4,.01)
densities<-dnorm(xseq, 0,1)
cumulative<-pnorm(xseq, 0, 1)
randomdeviates<-rnorm(1000,0,1) ## randomly generated numbers 

# par(mfrow=c(1,3), mar=c(3,4,4,2))
plot(xseq, densities, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Standard Normal", cex.axis=.8)
abline(v=0)
plot(xseq, cumulative, col="darkorange", xlab="", ylab="Cumulative Probability",type="l",lwd=2, cex=2, main="CDF of Standard Normal", cex.axis=.8)
hist(randomdeviates, main="Random draws from Std Normal", cex.axis=.8, xlim=c(-4,4))

## Normal Distribution 
## Q1 
mu1 <- 45
sigma1 <- 10 
xseq1 <- seq(mu1-8*sigma1, mu1+8*sigma1, 0.5)
densities1<-dnorm(xseq1, mu1,sigma1)
plot(xseq1, densities1, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Telephone call Placing ", cex.axis=.8)
abline(v=mu1)

## Q1 a part 
plot(xseq1, densities1, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Telephone call Placing a ", cex.axis=.8)
abline(v=mu1)
abline(v=60, col="red")
## Option1 rescale it to Z value and use pnorm 
z1_a <- (60 - mu1)/sigma1
pnorm(z1_a,lower.tail = T)
## Option 2 directlt use pnorm
pnorm(60, mean = mu1, sd = sigma1, lower.tail = T)


## Q1 b part 
plot(xseq1, densities1, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Telephone call Placing b ", cex.axis=.8)
abline(v=mu1)
abline(v=40, col="red")
## Option1 rescale it to Z value and use pnorm 
z1_b <- (40 - mu1)/sigma1
pnorm(z1_b,lower.tail = T)
## Option 2 directlt use pnorm
pnorm(40, mean = mu1, sd = sigma1, lower.tail = T)

##Q1 c part 
plot(xseq1, densities1, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Telephone call Placing c", cex.axis=.8)
abline(v=mu1)
abline(v=70, col="red")
## Option1 rescale it to Z value and use pnorm 
z1_c <- (70 - mu1)/sigma1
pnorm(z1_c,lower.tail = F)
## Option 2 directlt use pnorm with mean and 
pnorm(70, mean = mu1, sd = sigma1, lower.tail = F)

## Question 2
mu2_tr <- 5780 + 641 + 712
sigma_tr <- sqrt(142^2 + 78^2 + 72 ^2)
xseq2 <- seq(mu2_tr-8*sigma_tr, mu2_tr+8*sigma_tr, 40)
densities2<-dnorm(xseq2, mu2_tr,sigma_tr)
plot(xseq2, densities2, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Telephone call Placing ", cex.axis=.8)
abline(v=mu2_tr)
z2_c <- (7000 -mu2_tr)/sigma_tr
pnorm(z2_c, lower.tail = F)
pnorm(7000, mu2_tr, sigma_tr, lower.tail = F)

# ## Question3 Operations Research 
# 
# m1 <- 1.012
# sd1 <- 0.018
# xseq<-seq(0.8,1.2,.0001)
# densities<-dnorm(xseq, m1,sd1)
# cumulative<-pnorm(xseq, m1, sd1)
# randomdeviates<-rnorm(2000,m1,sd1)
# plot(xseq, densities, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Acceptable Pins", cex.axis=.8)
# abline(v=m1)
# plot(xseq, cumulative, col="darkorange", xlab="", ylab="Cumulative Probability",type="l",lwd=2, cex=2, main="CDF of Acceptable Pins", cex.axis=.8)
# 
# hist(randomdeviates, main="Acceptable Pins  Std Normal", cex.axis=.8, xlim=c(0.8,1.2))
# 
# ##Q1 What percentage of pins will be acceptable with in tolerance 
# ## 1.00 +/- 0.02
# plot(xseq, densities, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Acceptable Pins", cex.axis=.8)
# abline(v=1.02, col="black")
# abline(v=0.98, col = "black")
# abline(v=m1, col = "red")
# 
# pnorm(1.02,mean = m1, sd = sd1, lower.tail = T) -
#   pnorm(0.98,mean = m1, sd = sd1, lower.tail = T )
# 
# ##A1 63% of pins will be acceptable to the consumer
# 
# ##Q2 In order to improve the overall percentage accepted- What is that one need to do
# ## 1.00 +/- 0.02
# ## Lets assume percentage of acceptance is 95%, 99%
# ##1.96 (95%) = 1.02 - mu/sd
# new_mean_95 <-  1.02 -1.96*sd1 
# new_mean_95_1 <- 0.98 + 1.96*sd1
# new_mean_99 <- 1.02 - 2.58*sd1
# new_mean_99_1 <- 0.98+ 2.58*sd1
# 
# ## A2 the mean has to be 1.0 that is what customer desires 
# 
# ##Q3what is new standard deviation such that 95% of the pins are accepted 
# ## 95% of the area is covered +/-1.96 
# ##1.96 (95%) = 1.02 - mu/sd
# 
# sd1_95 = (1.02 - m1)/1.96 
# 
# sd1_99 = (1.02 - m1)/2.58
# 
# xseq<-seq(0.9,1.1,.0001)
# densities<-dnorm(xseq, m1,sd1_95)
# plot(xseq, densities, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Acceptable Pins", cex.axis=.8)
# abline(v=m1, col = "red")
# 
# xseq<-seq(0.9,1.1,.0001)
# densities<-dnorm(xseq, m1,sd1_99)
# plot(xseq, densities, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Acceptable Pins", cex.axis=.8)
# abline(v=m1, col = "red")
# 





# xseq<-seq(-4,4,.01)
# densities<-dnorm(xseq, 0,1)
# cumulative<-pnorm(xseq, 0, 1)
# randomdeviates<-rnorm(1000,0,1)
# 
# # par(mfrow=c(1,3), mar=c(3,4,4,2))
# 
# plot(xseq, densities, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Standard Normal", cex.axis=.8)
# abline(v=0)
# plot(xseq, cumulative, col="darkorange", xlab="", ylab="Cumulative Probability",type="l",lwd=2, cex=2, main="CDF of Standard Normal", cex.axis=.8)
# hist(randomdeviates, main="Random draws from Std Normal", cex.axis=.8, xlim=c(-4,4))
# 
# 
# x=seq(-3,3,length=200)
# y=dnorm(x,mean=0,sd=1)
# 
# plot(x,y)
# plot(x,y,type="l")
# 
# x=seq(20,80,length=200)
# y=dnorm(x,mean=50,sd=10)
# plot(x,y,type="l")
# 
# x=seq(-3,3,length=200)
# y=dnorm(x,mean=0,sd=1)
# plot(x,y,type="l")
# x=seq(-3,0,length=100)
# y=dnorm(x,mean=0,sd=1)
# polygon(c(-3,x,0),c(0,y,0),col="red")
# 
# pnorm(0,mean=0,sd=1)
# 
# 
# x=seq(-3,3,length=200)
# y=dnorm(x,mean=0,sd=1)
# plot(x,y,type="l")
# x=seq(-3,1,length=100)
# y=dnorm(x,mean=0,sd=1)
# polygon(c(-3,x,1),c(0,y,0),col="red")
# 
# 
# x=seq(20,80,length=200)
# y=dnorm(x,mean=50,sd=10)
# plot(x,y,type="l")
# x=seq(30,70,length=100)
# y=dnorm(x,mean=50,sd=10)
# polygon(c(30,x,70),c(0,y,0),col="red")
# 
# 
# x=seq(-3,3,length=200)
# y=dnorm(x,mean=0,sd=1)
# plot(x,y,type="l")
# x=seq(-3,-0.2533,length=100)
# y=dnorm(x,mean=0,sd=1)
# polygon(c(-3,x,-0.2533),c(0,y,0),col="red")
# text(-1,0.1,"0.40")
# arrows(0.5,0.1,-0.2,0,length=.15)
# text(0.5,0.12,"-0.2533")
# 
# 
# 
