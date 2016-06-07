# Esli russkie bukvi prevratilitis v krakozyabry, to File - Reopen with
# encoding... - UTF-8 - Set as default - OK

# lab 10

library("spikeslab")  # регрессия пик-плато
library("ggplot2")  # графики
library("dplyr")  # манипуляции с данными
library("reshape2")  # перевод таблиц: широкая-длинная
library("MCMCpack")  # байесовский подход к популярным моделям
library("quantreg")  # квантильная регрессия
library("randomForest")  # случайный лес
library("rattle")  # визуализация деревьев
library("caret")  # стандартный подход к регрессионным и классификационным задачам
library("rpart")  # классификационные и регрессионные деревья

#################################### медианная и квантильная регрессия

# прочитаем данные из .txt файла есть заголовок, header = TRUE разделитель данных
# - табуляция, sep='\t' разделитель дробной части - точка, dec='.'
f <- read.table("flats_moscow.txt", header = TRUE, sep = "\t", dec = ".")
glimpse(f)  # бросим взгляд чтобы убедиться, что загрузка прошла успешно

# квантильная регрессия для квантилей 0.1, 0.5 (медианная), 0.9
model_q01 <- rq(data = f, price ~ totsp, tau = c(0.1, 0.5, 0.9))
summary(model_q01)  # отчет по модели

# базовый график --- диаграмма рассеяния
base <- qplot(data = f, totsp, price)
base

# добавляем к базовому графику две линии квантильной регрессии
base_q <- base + stat_smooth(method = "rq", method.args = list(tau = 0.1), se = FALSE) +
  stat_smooth(method = "rq", method.args = list(tau = 0.9), se = FALSE)
base_q

# !!!! отличие от видеолекции !!!!
# по сравнению с видеофрагментом 10.2.1 опция tau передается пакету quantreg с внутри method.args
# необходимость method.args появилась при обновлении пакета ggplot2 до версии 2.0


# добавляем к графику дележку в зависимости от того, кирпичный дом или нет
base_q + aes(colour = factor(brick))

#################################### алгоритм случайного леса

# делим набор данных на две части: обучающую (75%) и тестовую получаем номера
# наблюдений из обучающей части
in_sample <- createDataPartition(f$price, p = 0.75, list = FALSE)
head(in_sample)  # несколько номеров наблюдений, входящих в обучающую выборку

f_train <- f[in_sample, ]  # отбираем наблюдения с номерами из in_sample
f_test <- f[-in_sample, ]  # отбираем наблюдения с номерами не из in_sample

# обычная регрессия
model_lm <- lm(data = f_train, price ~ totsp + kitsp + livesp + brick)
# случайный лес
model_rf <- randomForest(data = f_train, price ~ totsp + kitsp + livesp + brick)

# поместим цену в отдельную переменную
y <- f_test$price
# прогнозы по МНК
yhat_lm <- predict(model_lm, f_test)
# прогнозы по случайному лесу
yhat_rf <- predict(model_rf, f_test)

# сумма квадратов остатков прогнозов по тестовой выборке: МНК
sum((y - yhat_lm)^2)
# сумма квадратов остатков прогнозов по тестовой выборке: случайные лес
sum((y - yhat_rf)^2)

#################################### Байесовский подход: логит-модель

# создаем плохой набор данных с 'идеальной классификацией'
bad <- data.frame(y = c(0, 0, 1), x = c(1, 2, 3))
bad

# классический логит в теории максимум правдоподобия не существует чем больше
# бета при икс, тем больше будет правдоподобия однако из-за ограниченной
# компьютерной точности R находит некоторые оценки
model_logit <- glm(data = bad, y ~ x, family = binomial(link = "logit"))
summary(model_logit)  # отчет по модели
# яркий симптом такой ситуации --- огромные стандартные ошибки

# байесовский подход априорное распределение: beta ~ N(0, 50^2)
model_mcmc_logit <- MCMClogit(data = bad, y ~ x, b0 = 0, B0 = 1/50^2)
summary(model_mcmc_logit)  # отчет по байесовской логит-модели

# байесовский подход априорное распределение: beta ~ N(0, 10^2)
model_mcmc_logit <- MCMClogit(data = bad, y ~ x, b0 = 0, B0 = 1/10^2)
summary(model_mcmc_logit)  # отчет по байесовской логит-модели

#################################### Регрессия пик-плато (spike and slab regression)

# переведем скорость и длину тормозного пути во встроенном наборе данных в
# привычные единицы измерения
h <- mutate(cars, speed = 1.61 * speed, dist = 0.3 * dist)

# добавим квадрат скорости и мусорную переменную nrow(h) --- количество строк в
# таблице h
h <- mutate(h, speed2 = speed ^ 2, junk = rnorm(nrow(h)))

# обычная регрессия с мусорной переменной
model_lm <- lm(data = h, dist ~ speed + junk)
summary(model_lm)  # мусорная переменная незначима

# байесовская регрессия с мусорной переменной выборка из апостериорного
# распределения коэффициентов размера 4000
model_ss <- spikeslab(data = h, dist ~ speed + junk, n.iter2 = 4000)
print(model_ss)  # отчет по модели

model_ss$summary  # еще немного

# посмотрим, равнялся ли идеально нулю каждый коэффициент бета в выборке из
# апостериорного распределения
included_regressors <- melt(model_ss$model)

# глядим на полученный результат
head(included_regressors)
included_regressors

# апостериорные вероятности того, что коэффициент не равен нулю:

# сколько раз не был равен нулю бета при скорости / 4000
sum(included_regressors$value == 1)/4000
# сколько раз не был равен нулю бета при мусорной переменной / 4000
sum(included_regressors$value == 2)/4000
