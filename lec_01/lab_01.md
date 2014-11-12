
* R как калькулятор
- присваивание = и <- 
- заглавные и строчные
- + в строке
- tab
- NA, NaN, Inf
- вектора, номер сбоку

* скрипт
- векторные операции, mean(v) с NA и без
- 
- табличка
- адресация таблички
- список (чердак со всяким хламом)
- адресация списка
- сохранение/чтение скрипта: кодировка

* установка и подключение пакетов
ggplot2, GGally, dplyr, sophisthse, broom


* посмотри на данные
- на примере машинок
glimpse()
tail()
head()
describe()
str()
nrow()
ncol()

mean()
sd()
table()
cor()


* простые действия со встроенными наборами данных
data()
- mutate
mutate(dist=0.3*dist,speed=1.67*speed)
- filter
- select


* графики для количественных переменных:
- гистограмма 
- диаграмма рассеяния (xlab, ylab, main)
- много диаграмм рассеяния

* график --- для себя --- для публикации (или в лекцию?)

* линейная регрессия в R (cars)
model <- 
model.matrix(model)
coef(model)
deviance(model)
residuals(model)
fitted(model)
TSS (by hand)
R^2 (by hand)
(через report <- summary(model))
report$r.squared

* прогнозируем
nd <- 

* с набором данных swiss

* справка по R
- google/rseek
- stats.stackexchange
- stackoverflow
- help() 
- в скобках в каком пакете живет функция
