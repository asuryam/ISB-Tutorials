set.seed(789)


## Sampling Distribution 
## Q1 
x_bar1 <- 119600
s1 <- 35000 ## assumed
n1<- 75
z_1a <- (125000-x_bar1)/(s1/sqrt(n1))
pnorm(z_1a, lower.tail = F)
pnorm(125000, x_bar1, s1/sqrt(n1), lower.tail = F)
## Part b of teh question 
s_dev <- seq(30000, 40000, 500)
p_norms<-pnorm(125000, x_bar1, s_dev/sqrt(n1), lower.tail = F)

Output_Sdev_Probabilty<-data.frame(Standard_Deviation = s_dev, Probability=p_norms)

## Q2
x_bar2 <- 2.9
s2 <- 0.5
n2<-25
z2<- (3.0-x_bar2)/(s2/sqrt(n2))
pnorm(z2, lower.tail = F)
pnorm(3, x_bar2, s2/sqrt(n2), lower.tail = F)

## Q3
p3 <- 0.38
n3 <- 100
s3<-sqrt((p3*(1-p3)))
z3 <- (0.3-p3)/(s3/sqrt(n3))
pnorm(z3, lower.tail = F)
pnorm(0.3,p3,s3/sqrt(n3),lower.tail = F)

##Q4 
x_bar4 <- 1.57
n4 <- 200
s4 <- 0.4
pnorm(1.62, x_bar4, s4/sqrt(n4), lower.tail = T) - pnorm(1.52, x_bar4, s4/sqrt(n4), lower.tail = T)
