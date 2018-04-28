

## Chi SQuare Distribution 

Milk_Sales <- data.frame(Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul" , "Aug", "Sep", "Oct", "Nov", "Dec"),
                         Sales = c(1610, 1585, 1649, 1590, 1540, 1397, 1410, 1350, 1495, 1564, 1602, 1655))
Milk_Sales$Expected_Sales <- mean(Milk_Sales$Sales)
chissq_stat <- sum((Milk_Sales$Sales - Milk_Sales$Expected_Sales)^2/Milk_Sales$Expected_Sales)
pchisq(chissq_stat, df = 11,lower.tail = F)
chisq.test(Milk_Sales[,2])

## Number of Arrivals 
Arrivals_Freq <- data.frame(Number_Arrivals = c(0,1,2,3,4,5),
                                 Observed_freq = c(7,18,25,17,12,5))


Lamda <- sum(Arrivals_Freq$Number_Arrivals * Arrivals_Freq$Observed_freq) / sum(Arrivals_Freq$Observed_freq)
ppois(0, lambda=Lamda)
ppois(1, lambda=Lamda)

Arrivals_Freq$Cum_Prob <- ppois(Arrivals_Freq$Number_Arrivals, lambda=Lamda)
library(data.table)
setDT(Arrivals_Freq)[,Exp_Prob:=Cum_Prob-shift(Cum_Prob,1,type="lag")]
Arrivals_Freq$Exp_Prob[is.na(Arrivals_Freq$Exp_Prob)] <- Arrivals_Freq$Cum_Prob[is.na(Arrivals_Freq$Exp_Prob)]
Arrivals_Freq$Exp_Freq <- sum(Arrivals_Freq$Observed_freq) * Arrivals_Freq$Exp_Prob
Chisquarestat <- sum((Arrivals_Freq$Observed_freq - Arrivals_Freq$Exp_Freq)^2/Arrivals_Freq$Exp_Freq)

pchisq(Chisquarestat, df = 5, lower.tail = FALSE)


