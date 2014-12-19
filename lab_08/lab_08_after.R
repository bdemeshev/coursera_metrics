library("lubridate") # работа с датами

library("zoo") # временные ряды
library("xts") # еще ряды
library("dplyr") # манипуляции с данными
library("ggplot2") # графики
library("forecast")

library("quantmod") # загрузка с finance.google.com
library("sophisthse") # загрузка с sophist.hse.ru

y <- arima.sim(n=100, list(ar=0.7))
plot(y)
Acf(y)
Pacf(y)
tsdisplay(y)

y <- arima.sim(n=100, list(ma=-0.8))
tsdisplay(y)

y <- arima.sim(n=100, list(ma=-0.8, ar=0.5))
tsdisplay(y)

y <- arima.sim(n=100, list(ma=-0.8, ar=-0.5))
tsdisplay(y)

y <- arima.sim(n=100, list(order=c(0,1,0)))
tsdisplay(y)

y <- arima.sim(n=500, list(order=c(0,1,0)))
tsdisplay(y)

y <- seq(0, 10, length=100) + arima.sim(n=100, list(ar=0.7))
tsdisplay(y)

y <- seq(0, 2, length=100) + arima.sim(n=100, list(ar=0.7))
tsdisplay(y)

y <- LakeHuron
tsdisplay(y)

mod_1 <- Arima(y, order=c(2,0,0))
mod_2 <- Arima(y, order=c(1,0,1))
summary(mod_1)
summary(mod_2)

AIC(mod_1)
AIC(mod_2)

mod_3 <- Arima(y, order=c(2,0,1))
summary(mod_3)
AIC(mod_3)

prognoz <- forecast(mod_2, h=5)
prognoz

plot(prognoz)

mod_4 <- Arima(y, order=c(1,1,0))
AIC(mod_4)

prognoz_4 <- forecast(mod_4, h=5)
plot(prognoz_4)

mod_a <- auto.arima(y)
summary(mod_a)

prognoz_a <- forecast(mod_a, h=5)
plot(prognoz_a)

Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "GOOG", from="2014-01-01", to="2014-12-01")

head(GOOG)
y <- GOOG$GOOG.Close

tsdisplay(y)
dy <- diff(y)
tsdisplay(dy)

mod_1 <- Arima(y, order=c(0,1,0))
summary(mod_1)

prognoz_1 <- forecast(mod_1, h=20)
prognoz_1

plot(prognoz_1)

mod_a <- auto.arima(y)
summary(mod_a)

y <- sophisthse("POPNUM_Y")
tsdisplay(y)

mod_1 <- Arima(y, order=c(1,1,0), include.drift = TRUE)
summary(mod_1)

prognoz_1 <- forecast(mod_1, h=5)
plot(prognoz_1)

y <- sophisthse("CPI_M_CHI")
tsdisplay(as.ts(y))

time(y)
ym <- y[97:nrow(y),]
tsdisplay(as.ts(ym))

mod_1 <- Arima(ym, order=c(1,0,0), seasonal = c(1,0,0))
summary(mod_1)
AIC(mod_1)

prognoz_1 <- forecast(mod_1, h=12)
plot(prognoz_1)

mod_a <- auto.arima(ym)
prognoz_a <- forecast(mod_a, h=12)
plot(prognoz_a)
mod_a <- auto.arima(ym, approximation = FALSE)
