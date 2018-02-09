setwd("C:\\ISB-Tutorials\\SA2")
library(MASS)
library(tidyverse)
library(car)
library(perturb)
library(corpcor)

## Read the datasset
Data<-read.csv("Airfares.csv", header = TRUE, stringsAsFactors = F, na.strings = c("NA", "N/A", ""))
str(Data)
summary(Data)
sum(is.na(Data))

StudentResidualPlot <- function(model, comment){
  stu.resid <- studres(model)  
  hist(stu.resid, freq=FALSE,     
       main=comment) 
  xfit<-seq(min(stu.resid),max(stu.resid),length=80)  
  yfit<-dnorm(xfit)  
  lines(xfit, yfit)
  
}

CustomResidualPlot<- function(model){
  residualPlot(model1, id.n=5)
  residualPlots(model1,id.n=3)
}

DeletionDiagonsitics<- function(model, cutoff){
  influencePlot(model,id.n=3) # A user friendly representation of the above
  plot(model, which=4, cook.levels=cutoff)
}

Quant_colnames <- c ("COUPON", "NEW", "HI", "S_INCOME", "E_INCOME", 
                     "S_POP", "E_POP", "DISTANCE", "PAX", "FARE")
Qual_colnames <- c("VACATION", "SW", "SLOT", "GATE")

final_data <- select(Data,c(Quant_colnames, Qual_colnames))

final_data %>% gather(COUPON:FARE, key = "variable", value = "value") %>%
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~ variable, scales = 'free_x')

final_data %>% gather(COUPON:FARE, key = "variable", value = "value") %>%
  ggplot(aes(x=variable,y = value)) + 
  geom_boxplot() + facet_wrap(~ variable, scales = 'free_y')


boxplot(final_data$FARE, main="FARE", col ="blue")
boxplot(sqrt(final_data$FARE), main="sqrt(FARE)", col ="blue")
boxplot(log(final_data$FARE), main="log(FARE)", col ="blue")
boxplot(final_data$E_INCOME, main = "E-INCOME", col = "red")
boxplot(final_data$S_INCOME, main = "S-INCOME", col = "red")
boxplot(final_data$DISTANCE, main = "DISTANCE", col = "red")
boxplot(final_data$COUPON, main = "COUPON", col = "red")
boxplot(final_data$PAX, main = "PAX", col = "red")

## Qualitative variables 
final_data %>% gather(VACATION:GATE, key = "variable", value = "value") %>% ggplot(aes(x = value)) +
  geom_bar()+ facet_wrap(~variable, scales = 'free_x')

plot(final_data[,Quant_colnames])
cor(final_data[,Quant_colnames])

library(corrplot)
corrplot(round(cor(final_data[,Quant_colnames]),2), method = "number")

cor2pcor(cor(final_data[,Quant_colnames]))

model1 <-lm (FARE ~ ., data= final_data)
summary(model1)
anova(model1)
par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))
# residualPlot(model1, id.n=5)
# residualPlots(model1,id.n=3)
CustomResidualPlot(model1)
StudentResidualPlot(model1, "Initial Model")

#Box Cox tranformation 
gh<-boxcox(model1 )
gh$x[which.max(gh$y)]

model2 <-lm (log(FARE) ~ ., data= final_data)
summary(model2)
anova(model2)
par(mfrow=c(2,2))
plot(model2)

par(mfrow=c(1,1))
CustomResidualPlot(model2)
StudentResidualPlot(model2, "Model 2nd Iteration")

boxplot(final_data$COUPON)
boxplot(sqrt(final_data$COUPON))
final_data$SQRTCOUPON <- sqrt(final_data$COUPON)
boxplot(final_data$HI)
boxplot(log(final_data$HI))
final_data$HILOG <- log(final_data$HI)
boxplot(final_data$S_INCOME)
boxplot(log(final_data$S_INCOME))
final_data$S_INCOMELOG <- log(final_data$S_INCOME)
boxplot(final_data$S_POP)
boxplot(sqrt(final_data$S_POP))
final_data$SQRT_S_POP <- sqrt(final_data$S_POP)
boxplot(final_data$E_POP)
boxplot(sqrt(final_data$E_POP))
final_data$SQRT_E_POP<- sqrt(final_data$E_POP)
boxplot(final_data$DISTANCE)
boxplot(sqrt(final_data$DISTANCE))
final_data$SQRT_DISTANCE <- sqrt(final_data$DISTANCE)
boxplot(final_data$PAX)
boxplot((final_data$PAX)^2)
final_data$SQR_PAX <- final_data$PAX ^2

names(final_data)

model3 <- lm( log(FARE) ~., data = final_data[,-c(3,4)])
summary(model3)
anova(model3)
par(mfrow=c(2,2))
plot(model3)

par(mfrow=c(1,1))
CustomResidualPlot(model3)
StudentResidualPlot(model3, "Model 3rd Iteration")

vif(model3)
colldiag(final_data[,-c(3,4,10,11,12,13,14)], center = TRUE)



stepAIC(model3)

model4 <- lm(log(FARE) ~ COUPON + NEW + E_INCOME + E_POP + PAX + 
               VACATION + SW + SLOT + GATE + SQRTCOUPON + HILOG + S_INCOMELOG + 
               SQRT_S_POP + SQRT_E_POP + SQRT_DISTANCE + SQR_PAX, data = final_data[, -c(3, 4)])
                                                                                    
summary(model4)
anova(model4)
par(mfrow=c(2,2))
plot(model4)
par(mfrow=c(1,1))
residualPlot(model2, id.n=5)
residualPlots(model2,id.n=3)


vif(model4)
colldiag(final_data[,-c(3,4,6,10,11,12,13,14)], center = TRUE)

# Deletion Diagnostics
influence.measures(model4)
influenceIndexPlot(model4,id.n=3) # Index Plots of the influence measures
influencePlot(model4,id.n=3) # A user friendly representation of the above

cutoff <- 4/((nrow(final_data)-length(model4$coefficients)-2)) 
plot(model4, which=4, cook.levels=cutoff)
DeletionDiagonsitics(model4, cutoff)

model5 <- lm(log(FARE) ~ COUPON + NEW + E_INCOME + E_POP + PAX + 
                         VACATION + SW + SLOT + GATE + SQRTCOUPON + HILOG + S_INCOMELOG + 
                         SQRT_S_POP + SQRT_E_POP + SQRT_DISTANCE + SQR_PAX, data = final_data[-c(307,476,483), -c(3, 4)])

summary(model5)

influencePlot(model5,id.n=3) # A user friendly representation of the above

cutoff <- 4/((nrow(final_data)-length(model1$coefficients)-2)) 
DeletionDiagonsitics(model5, cutoff)
#plot(model5, which=4, cook.levels=cutoff)

model5

