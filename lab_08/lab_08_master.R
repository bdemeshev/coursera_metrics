
# с этого начнем с листа
library("lubridate") # работа с датами

library("zoo") # временные ряды
library("xts") # еще ряды
library("dplyr") # манипуляции с данными
library("ggplot2") # графики
library("forecast")

library("quantmod") # загрузка с finance.google.com
library("sophisthse") # загрузка с sophist.hse.ru


y <- arima.sim (n=100, list(ar=0.7))
plot(y)
Acf(y)
Pacf(y)
tsdisplay(y)

y <- arima.sim (n=100, list(ma=-0.7))
tsdisplay(y)


y <- arima.sim (n=100, list(ma=-0.7, ar=-0.5))
tsdisplay(y)



y <- arima.sim(n=100, list(order=c(0,1,0)))
tsdisplay(y)

dy <- diff(y)
tsdisplay(dy)



y <- seq(0,10, length=100)
tsdisplay(y)

y <- seq(0,10,length=100) + arima.sim (n=100, list(ma=-0.7))
tsdisplay(y)

y <- seq(0,1,length=100) + arima.sim (n=100, list(ma=-0.7))
tsdisplay(y)




y <- arima.sim (n=300, list(order=c(0,1,1), ma=-0.7))
tsdisplay(y)




y <- LakeHuron
tsdisplay(y)

model_0 <- Arima(y, order=c(2,0,0))
summary(model_0)
AIC(model_0)
model_1 <- Arima(y, order=c(0,1,0))
summary(model_1)
AIC(model_1)
model_2 <- Arima(y, order=c(0,1,0), include.drift = TRUE)
summary(model_2)
AIC(model_2)

auto_mod <- auto.arima(y)
summary(auto_mod)
AIC(auto_mod)

future <- forecast(model_0, h=20)
plot(future)

future <- forecast(auto_mod, h=20)
plot(future)



getSymbols(Symbols = "GOOG")
head(GOOG)
y <- GOOG$GOOG.Close
tsdisplay(y)
tsdisplay(diff(y))

mod_auto <- auto.arima(y)
summary(mod_auto)
AIC(mod_auto)
future <- forecast(mod_auto, h=20)
str(future)
plot(future)



y <- sophisthse("POPNUM_Y")
tsdisplay(y)

model_0 <- Arima(y, order=c(1,1,0), include.drift = TRUE)
summary(model_0)

future <- forecast(model_0, h=5)
plot(future)


y <- sophisthse("CPI_M_CHI")
tsdisplay(as.ts(y))
tsdisplay(diff(y))

y2 <- y[97:nrow(y),]
tsdisplay(y2)
mod_a <- auto.arima(y2)
summary(mod_a)

mod_0 <- Arima(y2, order=c(1,0,1), seasonal = c(1,0,1))
AIC(mod_0)
AIC(mod_a)
future <- forecast(mod_a)
plot(future)

future <- forecast(mod_0)
plot(future)


