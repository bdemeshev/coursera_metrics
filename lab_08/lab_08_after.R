# Esli russkie bukvi prevratilitis v krakozyabry, to File - Reopen with
# encoding... - UTF-8 - Set as default - OK

# lab 08

library("lubridate")  # работа с датами

library("zoo")  # временные ряды
library("xts")  # еще ряды
library("dplyr")  # манипуляции с данными
library("ggplot2")  # графики
library("forecast")

library("quantmod")  # загрузка с finance.google.com
library("sophisthse")  # загрузка с sophist.hse.ru

# симулируем процесс AR(1) y_t=0.7y_{t-1}+\e_t
y <- arima.sim(n = 100, list(ar = 0.7))
plot(y)  # график ряда
Acf(y)
Pacf(y)
tsdisplay(y)  # все три графика одним махом

# симулируем процесс MA(1) y_t=\e_t-0.8\e_{t-1}
y <- arima.sim(n = 100, list(ma = -0.8))
tsdisplay(y)

# симулируем процесс ARMA(1,1) y_t=0.5y_{t-1}+\e_t-0.8\e_{t-1}
y <- arima.sim(n = 100, list(ma = -0.8, ar = 0.5))
tsdisplay(y)

# симулируем процесс ARMA(1,1) y_t=-0.5y_{t-1}+\e_t-0.8\e_{t-1}
y <- arima.sim(n = 100, list(ma = -0.8, ar = -0.5))
tsdisplay(y)

# симулируем процесс случайного блуждания y_t=y_{t-1}+\e_t
y <- arima.sim(n = 100, list(order = c(0, 1, 0)))
tsdisplay(y)

# то же случайное блуждание, только 501 наблюдение
y <- arima.sim(n = 500, list(order = c(0, 1, 0)))
tsdisplay(y)

# добавим в AR(1) процессу тренд
y <- seq(0, 10, length = 100) + arima.sim(n = 100, list(ar = 0.7))
tsdisplay(y)

# добавим тренд послабее к AR(1) процессу
y <- seq(0, 2, length = 100) + arima.sim(n = 100, list(ar = 0.7))
tsdisplay(y)

# динамика уровня воды в озере Гурон
y <- LakeHuron
tsdisplay(y)

# оценим AR(2)
mod_1 <- Arima(y, order = c(2, 0, 0))
# оценим ARMA(1,1)
mod_2 <- Arima(y, order = c(1, 0, 1))

# результаты оценивания:
summary(mod_1)
summary(mod_2)

# штрафной критерий AIC
AIC(mod_1)
AIC(mod_2)

# оценим ARMA(2,1)
mod_3 <- Arima(y, order = c(2, 0, 1))
summary(mod_3)
AIC(mod_3)

# прогнозируем по модели 2 на 5 шагов вперед
prognoz <- forecast(mod_2, h = 5)
prognoz

# строим график прогноза
plot(prognoz)

# оценим ARIMA(1,1,0)
mod_4 <- Arima(y, order = c(1, 1, 0))
AIC(mod_4)

# прогноз по модели ARIMA(1,1,0)
prognoz_4 <- forecast(mod_4, h = 5)
plot(prognoz_4)

# автоматический подбор модели по штрафному критерию
mod_a <- auto.arima(y)
summary(mod_a)

# прогноз по автоматически подбираемой модели
prognoz_a <- forecast(mod_a, h = 5)
plot(prognoz_a)

# шаманское заклинание для перевода дат на английский чтобы корректно работала
# загрузка данных под русскоязычной windows
Sys.setlocale("LC_TIME", "C")

# загружаем данные по стоимости акций Гугла
getSymbols(Symbols = "GOOGL", from = "2014-01-01", to = "2016-05-11", src = "google")



head(GOOG)  # начало набора данных
y <- GOOG$GOOG.Close  # поместим цену закрытия в переменную y

tsdisplay(y)  # три графика для исходного ряда
dy <- diff(y)
tsdisplay(dy)  # три графика для приращений исходного ряда (для первой разности)

# похоже на случайное блуждание, оценим ARIMA(0,1,0)
mod_1 <- Arima(y, order = c(0, 1, 0))
summary(mod_1)

# прогнозируем на 20 шагов вперед
prognoz_1 <- forecast(mod_1, h = 20)
prognoz_1

# строим график прогноза
plot(prognoz_1)

# автоматический подбор модели
mod_a <- auto.arima(y)
summary(mod_a)

# численность населения России
y <- sophisthse("POPNUM_Y")
tsdisplay(y)

# ARIMA(1,1,0) со смещением
mod_1 <- Arima(y, order = c(1, 1, 0), include.drift = TRUE)
summary(mod_1)

# прогноз на 5 шагов вперед
prognoz_1 <- forecast(mod_1, h = 5)
plot(prognoz_1)

# индекс потребительских цен (ежемесячные данные)
y <- sophisthse("CPI_M_CHI")
tsdisplay(as.ts(y))


time(y)  # из временного ряда достанет номера моментов времени
ym <- y[97:nrow(y), ]  # возьмем наблюдения с 97 по последнее
tsdisplay(as.ts(ym))

# оценим модель AR(1) с сезонной составляющей SAR(1)
mod_1 <- Arima(ym, order = c(1, 0, 0), seasonal = c(1, 0, 0))
summary(mod_1)
AIC(mod_1)

# прогнозируем на год вперед
prognoz_1 <- forecast(mod_1, h = 12)
plot(prognoz_1)

# оцениваем автоматически подбираемую модель
mod_a <- auto.arima(ym)

# упрощенная версия максимального правдоподобия не прошла запускаем полную, это
# чуть подольше
mod_a <- auto.arima(ym, approximation = FALSE)

# прогнозируем на год вперед
prognoz_a <- forecast(mod_a, h = 12)
plot(prognoz_a)
