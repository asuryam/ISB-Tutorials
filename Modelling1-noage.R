setwd("C:\\ISB-Tutorials\\SA2\\AUTOMPG\\")
library(MASS)
library(car)
##library(caTools)
library(perturb)
library(tidyverse)
library(corpcor)
library(dummies)
library(glmnet)
library(caret)

set.seed(789)
range02 <- function(x) {
  (x - min(x, na.rm=TRUE)) / diff(range(x, na.rm=TRUE))
}
StudentResidualPlot <- function(model, comment){
  stu.resid <- studres(model)  
  hist(stu.resid, freq=FALSE,     
       main=comment) 
  xfit<-seq(min(stu.resid),max(stu.resid),length=80)  
  yfit<-dnorm(xfit)  
  lines(xfit, yfit)
  
}

CustomResidualPlot<- function(model){
  residualPlot(model, id.n=5)
  residualPlots(model,id.n=3)
}

DeletionDiagonsitics<- function(model, cutoff){
  print(influencePlot(model,id.n=3)) # A user friendly representation of the above
  plot(model, which=4, cook.levels=cutoff)
}

Clustering_dummy <- function(data,k.max = 15){
  wss <- sapply(1:k.max, 
                function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
  wss
  plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
  
  d <- dist(as.matrix(data)) 
  hc <- hclust(d) 
  plot(hc)
}


Data <- read.csv("auto-mpg.data.csv", header = T, stringsAsFactors = F, na.strings = c("?", "", "N/A", NA))
str(Data)
summary(Data)

final_Data <- Data[complete.cases(Data),]


Quant_colnames <- c ("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration")
Qual_colnames <- c('origin')

Data1 <- select(final_Data,c(Quant_colnames, Qual_colnames))
Data1$origin<- as.factor(Data1$origin)
##Clustering 
Data_new <- dummy.data.frame(Data1, sep = ".")
Data_Clust <- Data_new
Data_Clust[,Quant_colnames] <- lapply(Data_Clust[,Quant_colnames], range02)
Data_Clust<-Data_Clust[,-1]## Knocking off the Response variable
Clustering_dummy(Data_Clust, 10)

Data1 %>% gather(mpg:acceleration, key = "variable", value = "value") %>%
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~ variable, scales = 'free_x')

Data1 %>% gather(mpg:acceleration, key = "variable", value = "value") %>%
  ggplot(aes(x=variable,y = value)) + 
  geom_boxplot() + facet_wrap(~ variable, scales = 'free_y')

Data1 %>% gather(origin, key = "variable", value = "value") %>% ggplot(aes(x = value)) +
  geom_bar()+ facet_wrap(~variable, scales = 'free_x')


boxplot(Data1$mpg, horizontal = T, col = "blue")
boxplot(Data1$mpg ~ Data1$origin, horizontal = T, col = "blue")
boxplot(Data1$cylinders, horizontal = T, col = "red")
boxplot(Data1$displacement, horizontal = T, col = "red")
boxplot(Data1$displacement ~ Data1$origin, horizontal = T, col = "red")
boxplot(Data1$horsepower, horizontal = T, col = "red")
boxplot(Data1$horsepower ~ Data1$origin, horizontal = T, col = "red")
boxplot(Data1$weight, horizontal = T, col = "red")
boxplot(Data1$weight ~ Data1$origin, horizontal = T, col = "red")
boxplot(Data1$acceleration, horizontal = T, col = "red")
boxplot(Data1$acceleration ~ Data1$origin, horizontal = T, col = "red")


ggplot(Data1, aes(x=cylinders, y=mpg, col = factor(origin))) + geom_point() 
ggplot(Data1, aes(x=displacement, y=mpg, col = factor(origin))) + geom_point() 
ggplot(Data1, aes(x=1/displacement, y=mpg, col = factor(origin))) + geom_point() 
ggplot(Data1, aes(x=horsepower, y=mpg, col = factor(origin))) + geom_point() 
ggplot(Data1, aes(x=1/horsepower, y=mpg, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=weight, y=mpg, col = factor(origin))) + geom_point() 
ggplot(Data1, aes(x=1/weight, y=mpg, col = factor(origin))) + geom_point() 
ggplot(Data1, aes(x=acceleration, y=mpg, col = factor(origin))) + geom_point()

ggplot(Data1, aes(x=displacement, y=horsepower, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=displacement, y=weight, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=displacement, y=acceleration, col = factor(origin))) + geom_point
ggplot(Data1, aes(x=horsepower, y=weight, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=horsepower, y=acceleration, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=weight, y=acceleration, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=weight , y=age, col = factor(origin))) + geom_point()


plot(Data1)
y<-seq(1, 100,by = 0.5)
x<-1/y + 0.8
plot(x,y)

library(corrplot)
corrplot(round(cor(Data1[,Quant_colnames]),2), method = "number")
cor2pcor(cor(Data1[,Quant_colnames]))

## spliting teh data train - 90% and validate - 10%
colnames(Data_new)

# Data_new[,c("origin.1")] <- NULL 

ind <- createDataPartition( Data_new$mpg, p = 0.9)
train <- Data_new[unlist(ind),]
validate <-Data_new[-unlist(ind),]

cor(train$mpg, I(1/train$displacement))
cor(train$mpg, I(1/train$horsepower))
cor(train$mpg, I(1/train$weight))
##("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration")
train$I_displacement <- 1/train$displacement
train$I_horsepower <- 1/train$horsepower
train$I_weight <- 1/train$weight
model1<- lm(mpg ~ cylinders + I_displacement+ I_horsepower  + I_weight + acceleration + origin.2 + origin.3 , data = train)
##model1<- lm(mpg ~ cylinders + displacement + horsepower  + weight + acceleration + origin.2 + origin.3 , data = train)
summary(model1)
anova(model1)
par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))
CustomResidualPlot(model1)
par(mfrow=c(1,1))
cutoff <- 4/((nrow(train)-length(model1$coefficients)-2)) 
DeletionDiagonsitics(model1, cutoff)
vif(model1)
colldiag(model1)

##20,72,103,112,244,310,335,388
train$obs_num <- c(1:nrow(train))
train$obs_num <- NULL
influential_obs<-c(19,63,90,98,216,278,300,346)
train[influential_obs,]

model2<- lm(mpg ~ cylinders + I_displacement + I_horsepower  + I_weight + 
              acceleration + origin.2 + origin.3 , data = train[-influential_obs,])
summary(model2)
anova(model2)
par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(1,1))
CustomResidualPlot(model2)
par(mfrow=c(1,1))
DeletionDiagonsitics(model2, cutoff)
vif(model2)
colldiag(model2)

## Approximate mean centering of variables 
summary(train[-influential_obs,])
train$cylinders <- train$cylinders - 5.51
train$I_displacement <- train$displacement - 0.006654
train$I_horsepower <- train$I_horsepower - 0.010663
train$I_weight <- train$I_weight - 0.0003605
train$acceleration <- train$acceleration - 15.51

model3<- lm(mpg ~ cylinders + I_displacement + I_horsepower  + I_weight + 
              acceleration + origin.2 + origin.3 , data = train[-influential_obs,])
summary(model3)
anova(model3)
par(mfrow=c(2,2))
plot(model3)
par(mfrow=c(1,1))
CustomResidualPlot(model3)
par(mfrow=c(1,1))
DeletionDiagonsitics(model3, cutoff)
vif(model3)
colldiag(model3)



stepAIC(model3,direction = "backward")$Anova
# mpg ~ cylinders + I_displacement + I_horsepower + acceleration + 
# origin.3


colnames(train)
xtrain <- train[-influential_obs,-c(1,3:5,7,13)]
ytrain <- train[-influential_obs,1]
#Lasso Regression
lambda=10^seq(10,-2,length=100)
Lmod<-glmnet(as.matrix(xtrain),as.matrix(ytrain),alpha = 1,lambda = lambda)
cv.out1<-cv.glmnet(as.matrix(xtrain),as.matrix(ytrain),alpha = 1)
Lasso_best_lambda<-cv.out1$lambda.min
coef(cv.out1,s<-Lasso_best_lambda)

# 8 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept)      25.0480662
# cylinders        -0.6829325
# acceleration     -0.4637740
# origin.2         -0.1456637
# origin.3          1.9298075
# I_displacement   -0.0105138
# I_horsepower   1274.9330205
# I_weight       8731.5430083

model4 <- lm( mpg ~ cylinders + I_displacement + I_horsepower + acceleration + 
                origin.3,  data = train[-influential_obs,])
# model4 <- lm( mpg ~ cylinders + I_displacement + I_horsepower + I_weight + 
#                 acceleration + origin.2 + origin.3 ,, data = train[-influential_obs,])
summary(model4)
anova(model4)
par(mfrow=c(2,2))
plot(model4)
par(mfrow=c(1,1))
CustomResidualPlot(model4)
par(mfrow=c(1,1))
DeletionDiagonsitics(model4, cutoff)
vif(model4)
colldiag(model4)


gh<-boxcox(model4 )
gh$x[which.max(gh$y)]
model5 <- lm(log(mpg) ~ cylinders + I_displacement + I_horsepower + acceleration + 
               origin.3, data = train[-influential_obs,])
summary(model5)
anova(model5)
par(mfrow=c(2,2))
plot(model5)
par(mfrow=c(1,1))
CustomResidualPlot(model5)
par(mfrow=c(1,1))
DeletionDiagonsitics(model5, cutoff)
vif(model5)
colldiag(model5)

model6 <- lm(log(mpg) ~ cylinders + I_displacement + I_horsepower + I_weight + 
               acceleration + origin.3 + origin.2,, data = train[-influential_obs,])
summary(model6)
anova(model6)
par(mfrow=c(2,2))
plot(model6)
par(mfrow=c(1,1))
CustomResidualPlot(model6)
par(mfrow=c(1,1))
DeletionDiagonsitics(model6, cutoff)
vif(model6)
colldiag(model6)


validate$I_displacement <- 1/validate$displacement
validate$I_horsepower <- 1/validate$horsepower
validate$I_weight <- 1/validate$weight
Rsquare <- function(yactual, ypredict){
  SSE<-sum((yactual- ypredict)^2)
  SST <- sum((yactual-mean(yactual))^2)
  RMSE<-sqrt(SSE/nrow(validate)) 
  R_Sq<-1-SSE/SST
  return(c(SSE, SST, RMSE, R_Sq))
}
MAPE <- function(yactual, ypredict){
  return(mean(abs(yactual - ypredict)/yactual))
}
  
predict_log <- predict( model5, newdata = validate)
Rsquare(validate$mpg, exp(predict_log))
MAPE(validate$mpg, exp(predict_log))


## Allvariables based on LASSO
predict_log <- predict( model6, newdata = validate)
Rsquare(validate$mpg, exp(predict_log))
MAPE(validate$mpg, exp(predict_log))

