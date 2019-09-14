# if you see KRAKOZYABRY then do File-Reopen with encoding - UTF-8 - (Set as
# default) - OK

# подключаем пакеты
library(tidyverse) # графики + манипуляции с данными, включает dplyr, ggplot2, ...
library(skimr) # описательные статистики — стильная замена для psych
library(GGally)  # ещё графики

d <- cars  # встроенный набор данных по машинам
glimpse(d)  # что там?
help(cars)  # справка. действует для встроенных наборов данных
head(d)  # начало таблички d (первые 6 строк)
tail(d)  # хвостик таблички d
skim(d)  # среднее, мода, медиана и т.д. (в видеолекция decribe() из пакета psych)
ncol(d)  # число столбцов
nrow(d)  # число строк
str(d)  # структура (похоже на glimpse)

# среднее арифметическое
mean(d$speed)

# создадим новую переменные и поместим их все в табличку d2
d2 <- mutate(d, speed = 1.61 * speed, dist = 0.3 * dist, ratio = dist/speed)
glimpse(d2)

# графики
qplot(data = d2, dist)
qplot(data = d2, dist, xlab = "Длина тормозного пути (м)", ylab = "Число машин",
  main = "Данные по машинам 1920х")

qplot(data = d2, speed, dist)

# оценим модель парной регрессии y_i = \beta_1 + \beta_2 x_i + \varepsilon_i
model <- lm(data = d2, dist ~ speed)
model

coef(model)  # оценки бет
residuals(model)  # остатки (эпсилон с крышкой)
y_hat <- fitted(model)  # прогнозы (игрек с крышкой)
y <- d2$dist  # значения зависимой переменной

RSS <- deviance(model)  # так называют RSS
TSS <- sum((y - mean(y))^2)  # TSS
TSS
R2 <- 1 - RSS/TSS
R2
cor(y, y_hat)^2  # квадрат выборочной корреляции

X <- model.matrix(model)  # матрица регрессоров
X

# создаем новый набор данных
nd <- data.frame(speed = c(40, 60))
nd

# прогнозируем
predict(model, nd)

# добавляем на график линию регрессии
qplot(data = d2, speed, dist) + stat_smooth(method = "lm")


t <- swiss  # встроенный набор данных по Швейцарии
help(swiss)
glimpse(t)
describe(t)
ggpairs(t)  # все диаграммы рассеяния на одном графике

# множественная регрессия
model2 <- lm(data = t, Fertility ~ Agriculture + Education + Catholic)
coef(model2)  # оценки бет
fitted(model2)  # прогнозы
residuals(model2)  # остатки
deviance(model2)  # RSS

report <- summary(model2)
report
report$r.squared  # R^2

# второй способ расчета R^2
cor(t$Fertility, fitted(model2))^2

# создаем новый набор данных
nd2 <- data.frame(Agriculture = 0.5, Catholic = 0.5, Education = 20)
# прогнозируем
predict(model2, nd2)
