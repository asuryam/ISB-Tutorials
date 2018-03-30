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
library(dummies)

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

## Data preparation - Assuming that this analysis is being done in the year 88
Data$age <- 88-Data$model.year
Data$Car_Maker <- sapply(strsplit(Data$car_name," "), `[`, 1)
Data$Car_Maker <- gsub("\t", "", Data$Car_Maker)
sort(unique(Data$Car_Maker))
table(Data$Car_Maker, Data$origin)
## Car Maker : One car maker belongs to origin - Will skip car maker as of now 
final_Data <- Data[complete.cases(Data),]


Quant_colnames <- c ("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "age")
Qual_colnames <- c("origin")

Data1 <- dplyr::select(final_Data, c(Quant_colnames, Qual_colnames))
Data1$origin<- as.factor(Data1$origin)
##Clustering 
Data_new <- dummy.data.frame(Data1, sep = ".")
Data_Clust <- Data_new
Data_Clust[,Quant_colnames] <- lapply(Data_Clust[,Quant_colnames], range02)
Data_Clust<-Data_Clust[,-1]## Knocking off the Response variable
Clustering_dummy(Data_Clust, 10)

Data1 %>% gather(mpg:age, key = "variable", value = "value") %>%
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~ variable, scales = 'free_x')

Data1 %>% gather(mpg:age, key = "variable", value = "value") %>%
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
boxplot(Data1$age, horizontal = T, col = "red")
boxplot(Data1$age ~ Data1$origin , horizontal = T, col = "red")

ggplot(Data1, aes(x=cylinders, y=mpg, col = factor(origin))) + geom_point() 
ggplot(Data1, aes(x=displacement, y=mpg, col = factor(origin))) + geom_point() 
ggplot(Data1, aes(x=1/displacement, y=mpg, col = factor(origin))) + geom_point() 
ggplot(Data1, aes(x=horsepower, y=mpg, col = factor(origin))) + geom_point() 
ggplot(Data1, aes(x=1/horsepower, y=mpg, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=weight, y=mpg, col = factor(origin))) + geom_point() 
ggplot(Data1, aes(x=1/weight, y=mpg, col = factor(origin))) + geom_point() 
ggplot(Data1, aes(x=acceleration, y=mpg, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=age, y=mpg, col = factor(origin))) + geom_point()

ggplot(Data1, aes(x=displacement, y=horsepower, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=displacement, y=weight, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=displacement, y=acceleration, col = factor(origin))) + geom_point
ggplot(Data1, aes(x=displacement, y=age, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=horsepower, y=weight, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=horsepower, y=acceleration, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=horsepower, y=age, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=weight, y=acceleration, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=weight , y=age, col = factor(origin))) + geom_point()
ggplot(Data1, aes(x=age, y=acceleration, col = factor(origin))) + geom_point()

plot(Data1)
y<-seq(1, 100,by = 0.5)
x<-5/y 
plot(x,y)

library(corrplot)
corrplot(round(cor(Data1[,Quant_colnames]),2), method = "number")
cor2pcor(cor(Data1[,Quant_colnames]))
cor(Data1$mpg, I(1/Data1$displacement))
cor(Data1$mpg, I(1/Data1$horsepower))
cor(Data1$mpg, I(1/Data1$weight))
cor(Data1$mpg, I(Data1$weight/Data1$displacement))

## spliting teh data train - 90% and validate - 10%
colnames(Data_new)

# Data_new[,c("origin.1")] <- NULL 

ind <- createDataPartition( Data_new$mpg, p = 0.9)
train <- Data_new[unlist(ind),]
validate <-Data_new[-unlist(ind),]

train$I_displacement <- 1/train$displacement
train$I_horsepower <- 1/train$horsepower
train$I_weight <- 1/train$weight
# train$w2d_ratio <- train$weight/train$displacement

##("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration")
model1<- lm(mpg ~ cylinders + I_displacement + I_horsepower + I_weight + acceleration + age + origin.2 + origin.3
              , data = train)
# model1<- lm(mpg ~ cylinders +  I_horsepower + w2d_ratio + acceleration + age + origin.2 + origin.3, data = train)
#model1<- lm(mpg ~ cylinders + I_displacement + I_horsepower + weight + acceleration + age + origin.2 + origin.3, data = train)
##model1<- lm(mpg ~ cylinders + displacement + horsepower  + weight + acceleration + age + origin.2 + origin.3 , data = train)
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

# boxplot(train$cylinders^2)
# train$cylinders_SQR <- train$cylinders^2
# boxplot(sqrt(train$I_displacement))
# train$I_displacement_SQRT <- sqrt(train$I_displacement)
boxplot(train$acceleration^2)
train$acceleration_SQR <- train$acceleration ^2
boxplot(train$age^2)
train$age_SQR <- train$age ^2
colnames(train)

model2<- lm(mpg ~ cylinders + I_displacement + I_horsepower + I_weight + acceleration + age + origin.2 + origin.3 +
              + acceleration_SQR + age_SQR, data = train)

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

boxplot(sqrt(train$I_displacement))
train$I_displacement_SQRT <- sqrt(train$I_displacement)

model2_1 <- lm(mpg ~ cylinders + I_displacement + I_horsepower + I_weight + acceleration + age + origin.2 + origin.3 +
                 + acceleration_SQR + age_SQR + I_displacement_SQRT, data = train)
summary(model2_1)
anova(model2_1)
par(mfrow=c(2,2))
plot(model2_1)
par(mfrow=c(1,1))
CustomResidualPlot(model2_1)
par(mfrow=c(1,1))
DeletionDiagonsitics(model2_1, cutoff)
vif(model2_1)
colldiag(model2_1)


##20,112,300,310,327,335,388,395
train$obs_num <- c(1:nrow(train))
# train$obs_num <- NULL
influential_obs<-c(19,98,268,278,294,300,346, 352)
train[influential_obs,]
# ##Knocking out 300 from influential point high Hat
# influential_obs<-c(19,98,278,294,300,346, 352)
# train[influential_obs,]

model3<- lm(mpg ~ cylinders + I_displacement + I_horsepower + I_weight + acceleration + age + origin.2 + origin.3 +
              + acceleration_SQR + age_SQR + I_displacement_SQRT, data = train[-influential_obs,])
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



## Approximate mean centering of variables 
summary(train[-influential_obs,])
train$cylinders <- train$cylinders - 5.50
train$I_displacement <- train$I_displacement - 0.006679
train$I_horsepower <- train$I_horsepower - 0.010625
train$I_weight <- train$I_weight - 0.0003608
train$acceleration <- train$acceleration - 15.46
train$age <- train$age - 12.01
train$I_displacement_SQRT <- train$I_displacement_SQRT - 0.07919
train$acceleration_SQR <- train$acceleration_SQR -245.5
train$age_SQR <- train$age_SQR - 157.4

model4<- lm(mpg ~ cylinders + I_displacement + I_horsepower + I_weight + acceleration + age + origin.2 + origin.3 +
              + acceleration_SQR + age_SQR + I_displacement_SQRT, data = train[-influential_obs,])

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


stepAIC(model4,direction = "backward")$Anova
stepAIC(model4)

# Call:
#   lm(formula = mpg ~ I_horsepower + I_weight + acceleration + age + 
#        origin.3 + acceleration_SQR + age_SQR + I_displacement_SQRT, 
#      data = train[-influential_obs, ])

colnames(train)
xtrain <- train[-influential_obs,-c(1,3:5,8,17)]
ytrain <- train[-influential_obs,1]
#Lasso Regression
lambda=10^seq(10,-2,length=100)
Lmod<-glmnet(as.matrix(xtrain),as.matrix(ytrain),alpha = 1,lambda = lambda)
cv.out1<-cv.glmnet(as.matrix(xtrain),as.matrix(ytrain),alpha = 1)
Lasso_best_lambda<-cv.out1$lambda.min
coef(cv.out1,s<-Lasso_best_lambda)

# 12 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept)          2.288448e+01
# cylinders           -1.841503e-01
# acceleration        -9.519706e-01
# age                 -2.143243e+00
# origin.2             4.207524e-01
# origin.3             1.016924e+00
# I_displacement       .           
# I_horsepower         6.524985e+02
# I_weight             2.642822e+04
# acceleration_SQR     1.908581e-02
# age_SQR              5.976945e-02
# I_displacement_SQRT  5.613928e+01

model5 <- lm(mpg ~ I_horsepower + I_weight + acceleration + age + 
               origin.3 + acceleration_SQR + age_SQR + I_displacement_SQRT, 
               data = train[-influential_obs, ]  )
              
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

model5_LASSO <- lm(mpg ~ cylinders  + I_horsepower + I_weight + acceleration + age + origin.2 + origin.3 +
                     + acceleration_SQR + age_SQR + I_displacement_SQRT, data = train[-influential_obs, ]  )

summary(model5_LASSO)
anova(model5_LASSO)
par(mfrow=c(2,2))
plot(model5_LASSO)
par(mfrow=c(1,1))
CustomResidualPlot(model5_LASSO)
par(mfrow=c(1,1))
DeletionDiagonsitics(model5_LASSO, cutoff)
vif(model5_LASSO)
colldiag(model5_LASSO)

#Box Cox tranformation 
gh<-boxcox(model5 )
gh$x[which.max(gh$y)]



model6 <- lm(sqrt(mpg) ~ I_horsepower + I_weight + acceleration + age + 
               origin.3 + acceleration_SQR + age_SQR + I_displacement_SQRT, 
             data = train[-influential_obs,] )
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
StudentResidualPlot(model6, "model6")

stepAIC(model6)
model6_1 <- lm(sqrt(mpg) ~ I_horsepower + I_weight + acceleration + 
                 age + age_SQR + I_displacement_SQRT, 
             data = train[-influential_obs, ]  )
summary(model6_1)
anova(model6_1)
par(mfrow=c(2,2))
plot(model6_1)
par(mfrow=c(1,1))
CustomResidualPlot(model6_1)
par(mfrow=c(1,1))
DeletionDiagonsitics(model6_1, cutof)
vif(model6_1)
colldiag(model6_1)
StudentResidualPlot(model6_1, "model6-1")


validate$I_displacement <- 1/validate$displacement
validate$I_horsepower <- 1/validate$horsepower
validate$I_weight <- 1/validate$weight
validate$I_displacement_SQRT <- sqrt(validate$I_displacement)
validate$acceleration_SQR <- validate$acceleration ^2
validate$age_SQR <- validate$age ^2

predict_sqrt <- predict( model6_1, newdata = validate)
predict_model5<- predict(model5, newdata = validate)
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
## SQRT model
Rsquare(validate$mpg,predict_sqrt^2 )
MAPE(validate$mpg, predict_sqrt^2)
## Linear Model 
Rsquare(validate$mpg, predict_model5)
MAPE(validate$mpg, predict_model5)


