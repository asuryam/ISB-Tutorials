setwd("D://forecasting//")
library(forecast)
library(fpp)

data("ausbeer")
plot(ausbeer)
seasonplot(ausbeer, ylab = "Beer Production", xlab = "Quarter", main = "Seasonal Plot - Sales", year.labels = T, year.labels.left = T,col=1:20, pch=19  )
monthplot(ausbeer, ylab = "Beer Production", xlab = "Quarter",  main = "Seasonal Deviation plot: Sales")
boxplot(ausbeer~cycle(ausbeer))

Forecast_Horizon <- 40
## Spliting of data 
train <- window(ausbeer, end=c(1998,3))
validate <- window(ausbeer, start = c(1998,4))
mean_fit <- meanf(train, h = Forecast_Horizon)
naive_fit <- snaive(train, h=Forecast_Horizon)

plot(validate, main = "Forecasts of Quarterly Beer Production using Naive forecasts Vs Validate")
lines (mean_fit$mean, col = 2)
lines(naive_fit$mean, col = 3)
legend("topleft", lty = 1, col=c(1,2,3), legend = c( "Validate", "Mean Method", "Seasonal Naive Method"))
accuracy(mean_fit, validate, test = T)
accuracy(naive_fit, validate, test = T)
adf.test(train, alternative = "stationary")

plot(diff(train))
adf.test(diff(train), alternative = "stationary")
tsdisplay(diff(train))


##  p [1,2,3] d = 1 q =[1,2,3,4]

model1 <- arima(train, order = c(3,1,2))
summary(model1)
acf(residuals(model1))
hist(residuals(model1))

forecast_beer<-forecast(model1, h = Forecast_Horizon)
accuracy(forecast_beer, validate, test = T)

tsdisplay(diff(ausbeer))
## ARIMA model is best 
final_model <- arima(ausbeer, order = c(3,1,4))
summary(final_model)
acf(residuals(final_model))
hist(residuals(final_model))

beer_forecast<-forecast(final_model, h = 40)
plot(beer_forecast)


# data("hsales") ### Monthly Housing Sales 
# plot(hsales)
# data("ustreas") ## US tresury Bills 
# plot(ustreas)
# data("elec")## Australian Electric Production
# plot(elec) 
# 
# data(AirPassengers)
# summary(AirPassengers)
# plot(AirPassengers)
# 
# plot(AirPassengers)
# plot(log(AirPassengers)) ## trying to variance constant 
# plot(diff(log(AirPassengers))) ## trying to mean constant
# 
# acf(diff(log(AirPassengers)),) ## to fix q what ever lag crosses the upper confidecne level 
# pacf(diff(log(AirPassengers))) ## to fix p what ever lag crosses teh first upper confidence level 
# tsdisplay(diff(log(AirPassengers)))
# 
# ## Build 3 models p,1,q (0,1,1), (1,1,0), (1,1,1) compare the results
# 
# 
# model1 <- arima(log(AirPassengers), order=c(0,1,1))
# summary(model1)
# model2 <- arima(log(AirPassengers), order=c(1,1,0))
# summary(model2)
# acf(residuals(model2))
# model3 <- arima(log(AirPassengers), order = c(1,1,1))
# summary(model3)
# acf(residuals(model3) )
# plot(residuals(model3))
# 
# # ## Auto Arima results 
# # auto.arima(log(AirPassengers), d=1)
# 
# plot(log(elec))
# plot(diff(log(elec)))
# tsdisplay(diff(log(elec))) ## p is 2, q is 2 
# 
# # Build 3 models p,1,q (2,1,0), (0,1,2),(2,1,2),  compare the results
# 
# auto.arima(log(elec), d=1)
# emodel1 <- arima(log(AirPassengers), order=c(2,1,0))
# summary(emodel1)
# emodel2 <- arima(log(AirPassengers), order=c(0,1,2))
# summary(emodel2)
# acf(residuals(model2))
# emodel3 <- arima(log(AirPassengers), order = c(1,1,3))
# summary(emodel3)
# acf(residuals(emodel3) )
# plot(residuals(emodel3))




