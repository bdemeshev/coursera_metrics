# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики

# в этот момент нужно установить рабочую папку
# Session - Set working directory - To source file location
# читаем данные из файла в таблицу h
h <- read.table("flats_moscow.txt", header=TRUE)

# смотрим, что всё корректно загрузилось
head(h) # носик
tail(h) # хвостик

qplot(data=h, x=totsp, y=price) # диаграмма рассеяния

# на первом шаге оценим модель с помощью МНК
# проигнорировав возможную гетероскедастичность
model <- lm(price~totsp, data=h) # простая модель парной регрессии

summary(model) # отчет по модели
coeftest(model) # тесты незначимости коэффициентов 
confint(model) # доверительные интервалы


# добавляем в исходную таблицу h прогнозы, остатки из модели model
h <- augment(model,h) 
glimpse(h) # смотрим, что добавилось в таблицу h

# строим зависимость модуля (функция abs) остатков от общей площади
qplot(data=h,totsp,abs(.resid))
# наличие любой зависимости на этом графике означает гетероскедастичность


# простая оценка ковариационной матрицы
# верная в условиях гомоскедастичности
# неверная в условиях гетероскедастичности
vcov(model) 



# робастная оценка ковариационной матрицы 
# устойчивая к гетероскедастичности
vcovHC(model,type="HC0") # формула Уайта
vcovHC(model) # современный вариант формулы Уайта "HC3"
vcovHC(model,type="HC2") # еще один вариант


# проверяем незначимость коэффициентов с помощью:
coeftest(model) # обычной оценки ковариационной матрицы

# робастной оценки ковариационной матрицы:
coeftest(model,vcov. = vcovHC(model))

# строим руками доверительные интервалы
# робастные к гетероскедастичности

# сначала сохраним таблицу с коэффициентами и робастными ст. ошибками
conftable <- coeftest(model,vcov. = vcovHC(model))

# возьмем из этой таблицы два столбика (1-ый и 2-ой) и поместим в таблицу ci
ci <- data.frame(estimate=conftable[,1],
                 se_hc=conftable[,2])
ci # глянем на таблицу ci
# добавим в ci левую и правую границу доверительного интервала
ci <- mutate(ci,left_ci=estimate-1.96*se_hc,
             right_ci=estimate+1.96*se_hc)
ci # смотрим на результат

# для сравнение доверительные интервалы
confint(model) # по формулам корректным для гомоскедастичности

# тест Бройша-Пагана
# Во вспомогательной регрессии квадраты остатков зависят от исходных регрессоров 
bptest(model)

# тест Уайта
# Во вспомогательной регрессии квадраты остатков 
# зависят от totsp и totsp^2
bptest(model, data=h, varformula = ~ totsp + I(totsp^2) )
# альтернативный вариант включить totsp и totsp^2
bptest(model, data=h, varformula = ~ poly(totsp, 2) )

# тест Голдфельда-Квандта
gqtest(model, order.by = ~totsp, data=h, fraction = 0.2)

# диаграмма рассеяния в логарифмах
qplot(data=h, log(totsp), log(price))
# визуально гетероскедастичность меньше выражена

# модель парной регрессии в логарифмах
model2 <- lm(data=h, log(price)~log(totsp))

# тест Голдфельда-Квандта для модели в логарифмах
gqtest(model2, order.by = ~totsp, data=h, fraction = 0.2)
# гетероскедастичность есть, но гораздо менее выраженная