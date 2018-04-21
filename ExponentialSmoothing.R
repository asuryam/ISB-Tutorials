setwd("C://ISB-Tutorials//forecasting//Tutorial2")

library(forecast)
library(fpp)

data("visitors")
plot(visitors)
tsdisplay(visitors)
boxplot(visitors~cycle(visitors))
seasonplot(visitors, ylab = "Visitors", xlab = "Month", main = "Seasonal Plot - Visitors", year.labels = T, year.labels.left = T,col=1:20, pch=19  )
monthplot(visitors, ylab = "Visitors", xlab = "Month",  main = "Seasonal Deviation plot: Visitors")


Forecast_Horizon <- 36 ## Next 3years
train_visitors <- window(visitors, end = c(2002,4))
validate_visitors <- window(visitors, start = c(2002,5))

mean_fit <- meanf(train_visitors, h = Forecast_Horizon)
naive_fit <- snaive(train_visitors, h = Forecast_Horizon )

plot(validate_visitors, main = "Forecasts of Quarterly  Production using Naive forecasts Vs Validate")
lines (mean_fit$mean, col = 2)
lines(naive_fit$mean, col = 3)
legend("topleft", lty = 1, col=c(1,2,3), legend = c( "Validate", "Mean Method", "Seasonal "),
       bty = 'o')
forecast::accuracy(mean_fit, validate_visitors, test = T)
forecast::accuracy(naive_fit, validate_visitors, test = T)

fit1 <- ses(train_visitors,h=Forecast_Horizon, initial = "simple")
fit1$model
plot(visitors)
lines(fitted(fit1), col= "green", type = "o")
lines(fit1$mean, col= "red", type = "o")
forecast::accuracy(fit1, validate_visitors, test = T)

fit2 <- holt(train_visitors,h=Forecast_Horizon, initial = "simple")
fit2$model
plot(visitors)
lines(fitted(fit2), col= "green", type = "o")
lines(fit2$mean, col= "red", type = "o")
forecast::accuracy(fit2, validate_visitors, test = T)

fit3 <- holt(train_visitors,h=Forecast_Horizon, initial = "simple", exponential = T)
fit3$model
plot(visitors)
lines(fitted(fit3), col= "green", type = "o")
lines(fit3$mean, col= "red", type = "o")
forecast::accuracy(fit3, validate_visitors, test = T)


fit4 <- hw(train_visitors,h=Forecast_Horizon, initial = "simple", seasonal = "additive")
fit4$model
plot(visitors)
lines(fitted(fit4), col= "green", type = "o")
lines(fit4$mean, col= "red", type = "o")
forecast::accuracy(fit4, validate_visitors, test = T)

fit5 <- hw(train_visitors,h=Forecast_Horizon, initial = "simple", seasonal = "multiplicative")
fit5$model
plot(visitors)
lines(fitted(fit5), col= "green", type = "o")
lines(fit5$mean, col= "red", type = "o")
forecast::accuracy(fit5, validate_visitors, test = T)

fit6 <- hw(train_visitors,h=Forecast_Horizon, initial = "optimal", seasonal = "multiplicative", damped = T)
fit6$model
plot(visitors)
lines(fitted(fit6), col= "green", type = "o")
lines(fit6$mean, col= "red", type = "o")
forecast::accuracy(fit6, validate_visitors, test = T)

final_model <- hw(visitors, initial = "simple", seasonal = "multiplicative", h=Forecast_Horizon)
plot(final_model)
plot(visitors)
lines(fitted(final_model), col= "green", type = "o")
lines(final_model$mean, col= "red", type = "o")
