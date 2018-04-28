## Anova Problem 1 
library(ggplot2)
library(dplyr)
Oil_prices <- read.csv("Oil_Prices.csv", header = T, stringsAsFactors = F)
Oil_prices$Country <- as.factor(Oil_prices$Country)
Oil_prices_mean <- Oil_prices %>%  group_by(Country) %>% summarise(Mean_Oil_Price = mean(Oil_Price), 
                                                                   count = n())
ggplot(Oil_prices_mean, aes(x = Country, y = Mean_Oil_Price, , fill= as.factor(count))) + geom_bar(stat = "identity")
ggplot(Oil_prices_mean, aes(x = Country, y = Mean_Oil_Price, , fill= as.factor(count))) + geom_point(stat = "identity")


Oil_prices <- Oil_prices %>%  group_by(Country) %>% mutate(group_mean = mean(Oil_Price))
Oil_prices$grand_mean <-  mean(Oil_prices$Oil_Price)

Oil_prices$STR <- (Oil_prices$group_mean - Oil_prices$grand_mean) ^ 2 
Oil_prices$SE <- (Oil_prices$Oil_Price - Oil_prices$group_mean) ^ 2 

SSTR <- sum(Oil_prices$STR)
SSE <- sum(Oil_prices$SE)

df1<- nrow(Oil_prices) - length(unique(Oil_prices$Country)) ##n-r
df2 <- length(unique(Oil_prices$Country)) -1  ## r-1

MSTR <- SSTR/df2
MSE <- SSE/df1
Fstatistic <- MSTR/MSE

qf(.995, df2, df1) 
pf(Fstatistic,df2, df1, lower.tail = F)

## Area under the curve
anova(lm(Oil_Price ~ Country, data = Oil_prices))

## T value for 95% Confidence intervals for the each of the population means
tvalue <- qt(0.975,df1)
Oil_prices_mean$LowerCI <- Oil_prices_mean$Mean_Oil_Price - tvalue * sqrt(MSE/Oil_prices_mean$count)
Oil_prices_mean$UpperCI <- Oil_prices_mean$Mean_Oil_Price + tvalue * sqrt(MSE/Oil_prices_mean$count)

## Comparison between different population 


Tukey_value <- qtukey(0.975, nmeans = 4, df = 28) * sqrt(MSE/8)

## Get paried differences among the group means and compare with Tukey_value


## Problem2

SSTR <- 45210
SST <- 92340
SSE <- SST - SSTR
r <- 8 
n <- r*100

MSE <- SSE/(n-r)
MSTR <- SSTR/(r-1)

Fstatistic_2 <-  MSTR/MSE
Fstatistic_2
qf(0.99, r-1, n-r) ## r-1 and n-1 degrees of freedom
pf(Fstatistic_2, r-1, n-r, lower.tail = F)

Tukey_value_2 <- qtukey(0.995, nmeans = 8, df = 792) * sqrt(MSE/100) ## r and n-r degrees of freedom

brands <- c("Magnavox", "General Electric", "Panasonic", "Zenith", "Sears",  "Philco", "Sylvania", "RCA")
mean <- c(77, 78, 82, 94, 88, 89,90,87)
brand_means <- data.frame(brands, mean, samp)

row.names(brand_means)<- brand_means$brands
brand_means$brands <- NULL

a <- as.matrix(dist(brand_means))
a <- abs(a -Tukey_value_2)
brands <- c("Magnavox", "General Electric", "Panasonic", "Zenith", "Sears",  "Philco", "Sylvania", "RCA")
mean <- c(77, 78, 82, 94, 88, 89,90,87)
sample_size <- c(100, 100, 100, 100, 100, 100, 100, 100)
brand_means <- data.frame(brands, mean, sample_size)
##CI
df2 <- sum(brand_means$sample_size) - nrow(brand_means) ##n-r degree of freedom

brand_means$L_CI<-brand_means$mean + qt(0.005, df2)*MSE/sqrt(brand_means$sample_size)
brand_means$U_CI<-brand_means$mean + qt(0.995, df2)*MSE/sqrt(brand_means$sample_size)

