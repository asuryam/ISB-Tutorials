setwd("C://ISB-Tutorials//forecasting//Tutorial2")
library(forecast)
library(fpp)
library(neuralnet)
library(caret)
library(zoo)
library(dummies)
library(Metrics)

set.seed(789)

MAPE <- function(yactual, ypredict){
  return(mean(abs(yactual - ypredict)/yactual))
}


train<- data.frame(Input=runif(50, min=50, max=150))
train$Output <- sqrt(train$Input)


net.sqrt1 <- neuralnet(Output ~ Input, data = train, hidden = 1, threshold = 0.01 )
plot(net.sqrt1)
net.sqrt1


net.sqrt5 <- neuralnet(Output ~ Input, data = train, hidden = 5, threshold = 0.01 )
plot(net.sqrt5)

net.sqrt7 <- neuralnet(Output ~ Input, data = train, hidden =7, threshold = 0.01 )
plot(net.sqrt7)

test <- data.frame(Input=runif(10, min=50, max=150))
test$Output <- sqrt(test$Input)

predict1<- compute(net.sqrt1,test$Input)
RMSE(test$Output, predict1$net.result)
mae(test$Output, predict1$net.result)
MAPE(test$Output, predict1$net.result)*100


predict5<- compute(net.sqrt5,test[,1])
RMSE(test$Output, predict5$net.result)
mae(test$Output, predict5$net.result)
MAPE(test$Output, predict5$net.result)*100

predict7 <- compute(net.sqrt7, test[,1])
RMSE(test$Output, predict7$net.result)
mae(test$Output, predict7$net.result)
MAPE(test$Output, predict7$net.result)*100


data(credit)
creditlog <- data.frame(score = credit$score, 
                        log.savings = log(credit$savings +1),
                        log.income = log(credit$income +1),
                        log.address = log(credit$time.address +1),
                        log.employed = log(credit$time.employed +1),
                        fte = credit$fte,
                        single=credit$single)

ind <- createDataPartition(creditlog$score, p = 0.7)
train_credit <- creditlog[unlist(ind),]
validate_credit <- creditlog[-unlist(ind),]

model1 <- avNNet(score ~ log.savings + log.income + log.address + log.employed, 
                 data = creditlog,
                 repeats = 25,
                 size=3,
                 decay=1,
                 linout = TRUE)
summary(model1)

predict_m1<-predict(model1, validate_credit)
RMSE(validate_credit$score, predict_m1)
mae(validate_credit$score, predict_m1)
MAPE(validate_credit$score, predict_m1)*100


model2 <- avNNet(score ~ log.savings + log.income + log.address + log.employed + fte + single, 
                 data = creditlog,
                 repeats = 25,
                 size=3,
                 decay=1,
                 linout = TRUE)
summary(model2)

predict_m2<-predict(model2, validate_credit)
RMSE(validate_credit$score, predict_m2)
mae(validate_credit$score, predict_m2)
MAPE(validate_credit$score, predict_m2)*100
## forecasting begins

data("ukcars")
plot(ukcars)
tsdisplay(ukcars)
boxplot(ukcars~cycle(ukcars))
seasonplot(ukcars, ylab = "Production", xlab = "Month", main = "Seasonal Plot - UK Cars Production", year.labels = T, year.labels.left = T,col=1:20, pch=19  )
monthplot(ukcars, ylab = "Production", xlab = "Month",  main = "Seasonal Deviation plot: UK Cars Production")

ukcars_df <-cbind(as.data.frame(ukcars), as.data.frame(as.yearqtr(time(ukcars))))
colnames(ukcars_df) <- c("Production", "Quarter")
ukcars_df$Quarter <- as.factor(substr(ukcars_df$Quarter,6,7))

ukcars_df <- dummy.data.frame(ukcars_df, sep = ".")
colnames(ukcars_df)
ukcars_df$Quarter.Q1 <- NULL
ukcars_df$Trend <- c(1:nrow(ukcars_df))




Forecast_Horizon <- 8 ## Next 2years
train_ukcars <- window(ukcars, end = c(2003,1))
validate_ukcars <- window(ukcars, start = c(2003,2))

train_ukcars_df <- ukcars_df[1:(nrow(ukcars_df)-Forecast_Horizon), ]
validate_ukcars_df <- ukcars_df[(nrow(ukcars_df)-Forecast_Horizon+1):nrow(ukcars_df),]


mean_fit <- meanf(train_ukcars, h = Forecast_Horizon)
naive_fit <- snaive(train_ukcars, h = Forecast_Horizon )

plot(validate_ukcars, main = "Forecasts of Quarterly  Production using Naive forecasts Vs Validate")
lines (mean_fit$mean, col = 2)
lines(naive_fit$mean, col = 3)
legend("topleft", lty = 1, col=c(1,2,3), legend = c( "Validate", "Mean Method", "Seasonal "),
       bty = 'o')
forecast::accuracy(mean_fit, validate_ukcars, test = T)
forecast::accuracy(naive_fit, validate_ukcars, test = T)

## using model trend seasonality 

model_trend_seasonality <- avNNet(Production ~ Trend + Quarter.Q2 + Quarter.Q3 + Quarter.Q4, data = train_ukcars_df,
                                  repeats = 25,
                                  size=3,
                                  decay=1,
                                  linout = TRUE )
predict_trend_seasonality<-predict(model_trend_seasonality, validate_ukcars_df)

## RMSE
RMSE(validate_ukcars_df$Production, predict_trend_seasonality)
mae(validate_ukcars_df$Production, predict_trend_seasonality)
MAPE(validate_ukcars_df$Production, predict_trend_seasonality)*100

tsdisplay(train_ukcars)
model1<- nnetar(train_ukcars,p= 1, P =0, k=2)
forecast_validate <- forecast(model1, h= Forecast_Horizon)
forecast::accuracy(forecast_validate, validate_ukcars, test = T)
plot(forecast_validate)
lines(validate_ukcars)

model2<- nnetar(train_ukcars,p= 2, P =0, k=2)
forecast_validate <- forecast(model2, h= Forecast_Horizon)
forecast::accuracy(forecast_validate, validate_ukcars, test = T)
plot(forecast_validate)
lines(validate_ukcars)


model3<- nnetar(train_ukcars,p= 1, P =1, k=2)
forecast_validate <- forecast(model3, h= Forecast_Horizon)
forecast::accuracy(forecast_validate, validate_ukcars, test = T)
plot(forecast_validate)
lines(validate_ukcars)


model5 <- nnetar(train_ukcars, p=2, P=1, k=3)
forecast_validate <- forecast(model5, h= Forecast_Horizon)
forecast::accuracy(forecast_validate, validate_ukcars, test = T)
plot(forecast_validate)
lines(ukcars)


model6<- nnetar(train_ukcars, p = 1, P=2, k=2)
forecast_validate <- forecast(model6, h= Forecast_Horizon)
forecast::accuracy(forecast_validate, validate_ukcars, test = T)
plot(forecast_validate)
lines(ukcars)


model_auto <- nnetar(train_ukcars)
forecast_validate <- forecast(model_auto, h= Forecast_Horizon)
forecast::accuracy(forecast_validate, validate_ukcars, test = T)
plot(forecast_validate)
lines(ukcars)



final_model <- nnetar(ukcars,p = 1, P=2, k=2 )
final_forecast <- forecast(final_model, h= Forecast_Horizon)
plot(final_forecast)
lines(ukcars)

