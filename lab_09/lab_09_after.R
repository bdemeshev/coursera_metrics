# Esli russkie bukvi prevratilitis v krakozyabry, to File - Reopen with
# encoding... - UTF-8 - Set as default - OK

# lab 09

# подключаем пакеты
library(caret) # стандартизованный подход к регрессионным и классификационным моделям
library(AER) # инструментальные переменные
library(sandwich) # робастные стандартные ошибки
library(ivpack) # дополнительные плющки для инструментальных переменных
library(memisc) # табличка mtable
library(rio) # импорт файлов разных форматов
library(tidyverse) # графики и манипуляции с данными, подключаются пакеты dplyr, ggplot2, etc


######################### задача прогнозирования

# прочитаем данные из .txt файла 
h <- import("flats_moscow.txt")

glimpse(h)  # бросим взгляд на данные

# добавим логарифмы цены и площадей
h2 <- mutate(h, logprice = log(price), logtotsp = log(totsp), logkitsp = log(kitsp),
  loglivesp = log(livesp))

# создадим разбиение данных, отберем 75% случайных номеров
in_train <- createDataPartition(y = h2$logprice, p = 0.75, list = FALSE)
h2_train <- h2[in_train, ]  # отберем обучающую часть выборки
h2_test <- h2[-in_train, ]  # оставшееся пойдет в тестовую часть выборки

# оценим две модели с помощью МНК
model_1 <- lm(data = h2_train, logprice ~ logkitsp + logtotsp + loglivesp)
model_2 <- lm(data = h2_train, logprice ~ logtotsp)

# построим прогнозы по двум моделям на тестовой выборке
pred_1 <- predict(model_1, h2_test)
pred_2 <- predict(model_2, h2_test)

# посчитаем руками суммы квадратов остатков по тестовой выборке
sum((pred_1 - h2_test$logprice)^2)
sum((pred_2 - h2_test$logprice)^2)


############################################### оценивание заданной формы модели с эндогенностью

## данные
data("CigarettesSW", package = "AER")  # активируем набор данных
help("CigarettesSW")  # читаем справку


# для удобства назовем покороче
h <- CigarettesSW
glimpse(h)  # посмотрим на структуру данных

# построим диаграмму рассеяния
qplot(data = h, price, packs)

# отберем данные относящиеся к 1995 году
h2 <- filter(h, year == "1995")
# создадим новые переменные
h2 <- mutate(h2, rprice = price/cpi, 
             rincome = income/cpi/population, 
             tdiff = (taxs - tax)/cpi)
# снова глянем на диаграмму рассеяния
qplot(data = h2, price, packs)

# и бросим взгляд на набор данных
glimpse(h2)



# оценим функцию спроса с помощью МНК забыв, что имеет место эндогенность
model_0 <- lm(data = h2, log(packs) ~ log(rprice))
summary(model_0)

# двухшаговый МНК руками Шаг 1. Строим регрессию эндогенного регрессора на
# инструментальную переменную
st_1 <- lm(data = h2, log(rprice) ~ tdiff)
# сохраняем прогнозы из регрессии первого шага
h2$logprice_hat <- fitted(st_1)

# Шаг 2. Строим регрессию зависимой переменной на прогнозы с первого шага
st_2 <- lm(data = h2, log(packs) ~ logprice_hat)
coeftest(st_2)
# здесь функция coeftest использует неверные стандартные ошибки 
# (даже при гомоскедастичности)


help(ivreg)  # документация по команде ivreg

# двухшаговый МНК в одну строчку
model_iv <- ivreg(data = h2, log(packs) ~ log(rprice) | tdiff)
coeftest(model_iv)  # здесь стандартные ошибки — корректные

# сравним три модели в одной табличке
mtable(model_0, model_iv, st_2)

# используем для проверки гипотез робастные стандартные ошибки
coeftest(model_iv, vcov = vcovHC)


# модель с одной экзогенной, log(rincome), и одной эндогенной переменной,
# log(rprice)
iv_model_2 <- ivreg(data = h2, 
        log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff)
# тестируем гипотезы с использованием робастных стандартных ошибок
coeftest(iv_model_2, vcov = vcovHC)

# модель с одной экзогенной, одной эндогенной и двумя инструментальными
# переменными для эндогенной
iv_model_3 <- ivreg(data = h2, 
      log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi))
# тестируем гипотезы с использованием робастных стандартных ошибок
coeftest(iv_model_3, vcov = vcovHC)
