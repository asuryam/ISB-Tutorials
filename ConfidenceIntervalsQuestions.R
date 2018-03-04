set.seed(789)

## Q1 
data1 <- c(32, 33, 28, 37,29,30,25,27,39,40,26,26,27,30,25,30,31,29,24,36,25,37,37,20,22,35,23,28,30,36,40,41)
x_bar1 <- mean(data1)
s1<- sd(data1)
n1<- length(data1)
df1<-n1-1
##90% CI
T_90_CI<-qt(c(0.05,0.95),df1)
x_bar1 + T_90_CI*s1/sqrt(n1)

T_95_CI<-qt(c(0.025,0.995),df1)
x_bar1 + T_95_CI*s1/sqrt(n1)

T_99_CI<-qt(c(0.005,0.995),df1)
x_bar1 + T_99_CI*s1/sqrt(n1)


##Q2
x_bar2 <- 43
s2<- 14
n2<-1200
df2<-n2-1
##90% CI
T_90_CI<-qt(c(0.05,0.95),df2)
x_bar2 + T_90_CI*s2/sqrt(n2)

T_95_CI<-qt(c(0.025,0.995),df2)
x_bar2 + T_95_CI*s2/sqrt(n2)

T_99_CI<-qt(c(0.005,0.995),df2)
x_bar2 + T_99_CI*s2/sqrt(n2)

N_90_CI<-qnorm(c(0.05,0.95))
x_bar2 + N_90_CI*s2/sqrt(n2)

N_95_CI<-qnorm(c(0.025,0.995))
x_bar2 + N_95_CI*s2/sqrt(n2)

N_99_CI<-qnorm(c(0.005,0.995))
x_bar2 + N_99_CI*s2/sqrt(n2)

##Q3
p <-42/68
q <- 1-p
s3<- sqrt(p*q)
n3<-68
N_90_CI<-qnorm(c(0.05,0.95))
p + N_90_CI*s3/sqrt(n3)

N_95_CI<-qnorm(c(0.025,0.995))
p + N_95_CI*s3/sqrt(n3)

N_99_CI<-qnorm(c(0.005,0.995))
p + N_99_CI*s3/sqrt(n3)

##Q4
B <- 2000
v4<- 40000000
zalpha <- qnorm(c(0.95,0.975, 0.995))
n4<- (zalpha^2*v4 )/B^2

##Q5
p <- 0.1
q <- 1-p
B5 <- 0.02
n5 <- (zalpha^2* p*q)/B5^2
n5
