setwd("C://ISB-Tutorials//forecasting/")
library(forecast)
library(fpp)

data("fancy")
plot(fancy)

fancy1 <- read.csv("fancy.csv", header = F)
colnames(fancy1) <- "Sales"
ts_fancy1<-ts(fancy1$Sales, start= c(1987,1), end = c(1993, 12), frequency = 12)
plot.ts(ts_fancy1)


seasonplot(fancy, ylab = "Sales", xlab = "Month", main = "Seasonal Plot - Sales", year.labels = T, year.labels.left = T,col=1:20, pch=19  )
monthplot(fancy, ylab = "Sales", xlab = "Month",  main = "Seasonal Deviation plot: Sales")
boxplot(fancy~cycle(fancy))

plot.ts(log(fancy))
seasonplot(log(fancy), ylab = "Log of Sales", xlab = "Month", main = "Seasonal Plot - Log of Sales", year.labels = T, year.labels.left = T,col=1:20, pch=19  )
monthplot(log(fancy), ylab = "Log of Sales", xlab = "Month", main = "Seasonal Deviation plot: LOg of Sales")
boxplot(log(fancy)~cycle(fancy))

Forecast_Horizon <- 12
## Spliting of data 
train <- window(fancy, end=c(1992,12))
validate <- window(fancy, start = c(1993,1))
mean_fit <- meanf(train, h = 12)
naive_fit <- snaive(train, h=12)

plot(validate, main = "Forecasts of Quarterly Beer Production using Naive forecasts Vs Validate")
lines (mean_fit$mean, col = 2)
lines(naive_fit$mean, col = 3)
legend("topleft", lty = 1, col=c(1,2,3), legend = c( "Validate", "Mean Method", "Seasonal Naive Method"))
accuracy(mean_fit, validate, test = T)
accuracy(naive_fit, validate, test = T)



mean(abs(naive_fit$mean - validate)/validate)

fit1 <- tslm(train ~ trend +season)
summary(fit1)
## Ploting 
tsdisplay(residuals(fit1))
additive<- forecast.lm(fit1, h = Forecast_Horizon)
accuracy(additive, validate, test = T)
acf(residuals(fit1))





fit2 <- tslm(log(train) ~ trend + season)
summary(fit2)
multiplicative<- forecast.lm(fit2, h = Forecast_Horizon)
tsdisplay(residuals(fit2))
accuracy(exp(multiplicative$mean), validate, test = T)
acf(residuals(fit2))
seasonplot(residuals(fit2))
seasonplot(residuals(fit2), ylab = "Residuals - Multiplicative Model", xlab = "Month", main = "Seasonal Plot - Multiplicative Residuals", year.labels = T, year.labels.left = T,col=1:20, pch=19  )
monthplot(residuals(fit2))
hist(residuals(fit2))
x<-seq(-0.5, 0.5, by = 0.001)
lines(x,8*dnorm(x,0,sd(residuals(fit2))), ylim = c(0,15), col = 2 )

plot(validate, main = "Forecasts of Montly Sales using Naive and Regression based forecasts Vs Validate", lty=1)
lines (mean_fit$mean, col = 2,lty=1)
lines(naive_fit$mean, col = 3, lty=1)
lines(additive$mean, col = 4, lty = 1)
lines(exp(multiplicative$mean), col = 5, lty = 1 )
legend("topleft", lty = 1, col=c(1,2,3,4,5), legend = c( "Validate", "Mean Method", "Seasonal Naive Method", "Additive ", "Multiplicative"))


dummy_fest = rep(0, length(fancy))
dummy_fest[seq_along(dummy_fest)%%12 == 3] <- 1
dummy_fest[3] <- 0 #festival started one year later
dummy_fest = ts(dummy_fest, freq = 12, start=c(1987,1))
my_data <- data.frame(
  log_facny=log(fancy),
  dummy_fest
)
trainfest <- my_data[1:72, ]
validatefest <- my_data[73:84,]


fit3 <-  tslm(log(train) ~ trend + season + dummy_fest,data = trainfest)
summary(fit3)
multiplicativefest<- forecast.lm(fit3, h = Forecast_Horizon, newdata = validatefest )
accuracy(exp(multiplicativefest$mean), validate, test = T)
acf(residuals(fit3))
seasonplot(residuals(fit3))
monthplot(residuals(fit3))


fit4 <- arima(log(train), order = c(12,0,0))
summary(fit4)
ar3model <- forecast(fit4, h=Forecast_Horizon)
accuracy(exp(ar3model$mean), validate)



final_model <- tslm(log(fancy) ~ trend + season)
summary(final_model)
ForecastSales<- forecast.lm(final_model, h = Forecast_Horizon)
hist(residuals(final_model))
x<-seq(-0.5, 0.5, by = 0.001)
lines(x,8*dnorm(x,0,sd(residuals(final_model))), ylim = c(0,15), col = 2 )

acf(residuals(final_model))
seasonplot(residuals(final_model), ylab = "Residuals - Multiplicative Model", xlab = "Month", main = "Seasonal Plot - Multiplicative Residuals", year.labels = T, year.labels.left = T,col=1:20, pch=19  )

plot()
dwtest(final_model)

plot(exp(ForecastSales))
plot(exp(ForecastSales$mean))

# h1 <- ses(log(train))
# h2 <- holt(log(train))
# h3 <- holt(log(train), exponential = T)
# h4 <- holt(log(train), damped = T )
# h5 <- holt(log(train), exponential = T, damped = T)
# 
# accuracy(exp(h1$mean), validate, test = T )
# accuracy(exp(h2$mean), validate, test = T )
# accuracy(exp(h3$mean), validate, test = T )
# accuracy(exp(h4$mean), validate, test = T )
# accuracy(exp(h5$mean), validate, test = T )
# 
# hw1<- hw(log(train), seasonal = "additive")
# accuracy(exp(hw1$mean), validate, test = T )

hw2<-hw(train, seasonal = "multiplicative")
accuracy(hw2, validate, test = T )
acf(residuals(hw2))

hw_final_model <- hw(fancy, seasonal="multiplicative",h = Forecast_Horizon)
acf(residuals(hw_final_model))
hist(residuals(hw_final_model))
x<-seq(-0.5, 0.5, by = 0.001)
lines(x,9*dnorm(x,0,sd(residuals(hw_final_model))), ylim = c(0,20), col = 2 )


plot(hw_final_model)


