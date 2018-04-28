##Q1
xbar1 <- 250
s1 <- 12
n1 <- 60

mu0 <- 247

## Option 1
t_test_statistic1 <- (xbar1 - mu0) / (s1/sqrt(n1))
pt(t_test_statistic1,df = n1-1, lower.tail = F)
2*pt(t_test_statistic1,df = n1-1, lower.tail = F)

## Option2
t_test_statistic1
qt(c(0.05, 0.95), df = n1-1)
qt(c(0.025, 0.975), df = n1-1)
qt(c(0.005, 0.995), df = n1-1)

##Q2  

n2 <- 70
p2 <- 0.52
q2 <- 1-p2
po <- 0.58

z_test_statistic_2 <- (p2-po) / sqrt(po * (1-po)/n2)
pnorm(z_test_statistic_2, lower.tail = F)
z_test_statistic_2
qnorm(c( 0.95))
qnorm(c(0.975))
qnorm(c( 0.995))


##Q3
sample_diff <- c(15, -8 , 32, 57, 20 , 10, -18, -12, 60, 72, 38, -5, 16, 22, 34, 41, 12, -38, 16, -40, 75,11, 2, 44, 10)
mu_diff <- 0 
n <- length(sample_diff)
x_bar_diff <- mean(sample_diff)
s <- sd(sample_diff)
T_value <- (x_bar_diff - mu_diff) / (s/sqrt(n))

qt(c(0.05,0.95),n-1)
qt(c(0.025,0.975),n-1)
qt(c(0.005, 0.995),n-1)

1-pt(T_value, n-1)



##Q4
x1_bar <- 26
x2_bar <-21
s1 <- 8 
s2 <- 6 
n1 <- 45
n2 <- 32

df <- n1+n2-2
mu_diff3 <- 0 

t_value3 <- ((x1_bar -x2_bar ) - mu_diff3)/sqrt(s1^2/n1 + s2^2/n2)
1-pt(t_value3, df)
pt(t_value3, n1+n2-2, lower.tail = F)
t_value3
qt(c(0.95), df )
qt(c(0.975), df )
qt(c(0.995), df )