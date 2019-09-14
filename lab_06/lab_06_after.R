# Esli russkie bukvi prevratilitis v krakozyabry, to File - Reopen with
# encoding... - UTF-8 - Set as default - OK

# lab 6

# подключаем пакеты
library(lubridate)  # работа с датами
library(sandwich)  # vcovHC, vcovHAC
library(lmtest)  # тесты
library(car)  # ещё тесты
library(zoo)  # временные ряды
library(xts)  # ещё ряды
library(broom)  # манипуляции с моделями
library(estimatr) # модели с робастными стандартными ошибками
library(tidyverse) # графики и манипуляции с данными, подключаются пакеты dplyr, ggplot2, etc
library(quantmod)  # загрузка с finance.google.com
library(rusquant)  # загрузка с finam.ru
library(sophisthse)  # загрузка с sophist.hse.ru
library(Quandl)  # загрузка с Quandl

# задаём даты в виде простого текста
x <- c("2012-04-15", "2011-08-17")

y <- ymd(x)  # конвертируем в специальный формат дат
y

y + days(20)  # прибавим 20 дней
y - years(10)  # вычтем 10 лет
day(y)  # вытащим из даты только число
month(y)  # ... только месяц
year(y)  # ... только год
vignette("lubridate")  # более подробная справка про даты

# создадим временной ряд
x <- rnorm(5)  # пять N(0,1) случайных величин
x

y <- ymd("2014-01-01") + days(0:4)  # даты к этим величинам
y

ts <- zoo(x, order.by = y)  # склеим числа и даты в один временной ряд
ts


# функция lag есть в разных пакетах, и действует, она, увы, по-разному
# функция lag из пакета stats:
stats::lag(ts, -1)  # лаг, то есть прошлое значение ряда
stats::lag(ts, 1)  # форвардный лаг, то есть будущее значение
# функция lag из пакета dplyr
dplyr::lag(ts, -1)  # не должна сработать
dplyr::lag(ts, 1)  # лаг, то есть прошлое значение ряда


diff(ts)  # приращение ряда

# те же пять чисел, только оформленные как квартальные данные
ts2 <- zooreg(x, start = as.yearqtr("2014-01"), freq = 4)
ts2

# те же пять чисел, только оформленные как месячные данные
ts3 <- zooreg(x, start = as.yearmon("2014-01"), freq = 12)
ts3

data("Investment")  # встроенный набор данных
help("Investment")

start(Investment)  # момент начала временного ряда
end(Investment)  # окончания
time(Investment)  # только моменты времени
coredata(Investment)  # только сами числа без дат

dna <- Investment  # скопируем набор данных Investment
dna[1, 2] <- NA  # и внесем туда искусственно пропуски
dna[5, 3] <- NA
na.approx(dna)  # линейная аппроксимация
na.locf(dna)  # заполнение последним известным значением

# загрузка данных с sophist.hse.ru это численность населения России
a <- sophisthse("POPNUM_Y")
a
# другие названия рядов можно глянуть на http://sophist.hse.ru/hse/nindex.shtml
# например, CPI_Y_CHI --- индекс потребительских цен

# загрузка данных с quandl
b <- Quandl("FRED/GNP")
b
# это огромная база, по ней есть удобный поиск https://www.quandl.com/

# загрузка данных finance.yahoo.com (Гугл прекратил предоставление финансовых данных в марте 2018)
Sys.setlocale("LC_TIME", "C")  # это шаманское заклинание позволяет избежать проблем с русской кодировкой месяцев под windows
# цены акций компании Apple:
getSymbols(Symbols = "AAPL", from = "2010-01-01", to = "2014-02-03", src = "yahoo")
head(AAPL)
tail(AAPL)

# загрузка данных с finam.ru цены акций компании Газпром
getSymbols(Symbols = "GAZP", from = "2011-01-02", to = "2014-09-09", src = "Finam")
head(GAZP)
tail(GAZP)

# несколько вариантов графиков:
plot(GAZP)
autoplot(GAZP[, 1:4])
autoplot(GAZP[, 1:4], facets = NULL)
chartSeries(GAZP)

# возвращаемся к набору данных с инвестициями в R есть два популярных формата
# хранения табличных данных это data.frame для невременных рядов и zoo или xts
# для временных рядов некоторые функции заточены под один формат, некоторые - под
# другой мы превращаем data.frame Investment в zoo временной ряд
d <- as.zoo(Investment)
autoplot(d[, 1:2], facets = NULL)

# простая линейная модель
model <- lm(data = d, RealInv ~ RealInt + RealGNP)


summary(model)  # краткий отчет по модели
coeftest(model)  # тесты на коэффициенты
confint(model)  # доверительные интервалы для коэффициентов
# в этих трех командах по умолчанию используются некорректные для автокорреляции
# станадртные ошибки

# добавим к исходных данным остатки и прогнозы
d_aug <- augment(model, as.data.frame(d))
glimpse(d_aug)
qplot(data = d_aug, lag(.resid), .resid)  # график остатка от предыдущего значения

vcov(model)  # обычная оценка ковариационной матрицы
# не годная в условиях автокорреляции

vcovHAC(model)  # робастная оценка ковариационной матрицы
# годная в условиях автокорреляции

# тестируем гипотезы о равенстве коэффициентов нулю с помощью правильной оценки
# ковариационной матрицы
coeftest(model, vcov. = vcovHAC(model))

# строим корректные при автокоррреляции доверительные интервалы
conftable <- coeftest(model, vcov. = vcovHAC(model))
ci <- data.frame(estimate = conftable[, 1], se_ac = conftable[, 2])
ci <- mutate(ci, left_95 = estimate - 1.96 * se_ac, right_95 = estimate + 1.96 *
  se_ac)
ci

# Durbin-Watson H0: нет автокорреляции Ha: автокорреляции 1-го порядка
dwt(model)
res <- dwt(model)
res$dw  # значение статистики DW
res$p  # симуляционное p-value.
# В силу небольшого количества наблюдений и симуляционных свойств алгоритма может
# колебаться
res$r  # оценка корреляции

# Тест Бройша-Годфри H0: нет автокорреляции Ha: автокорреляция k-го порядка
bgtest(model, order = 2)
# H0 не отвергается
res <- bgtest(model, order = 2)
res$statistic  # значение статистики BG
res$p.value  # P-значение
