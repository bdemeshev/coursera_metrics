library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")

# ESLI RUSSKIE BUKVI NE VIDNI ---> File -- Reopen with encoding --- utf8 --- set
# as default --- ok

# помещаем встроенный набор данных по бриллиантам в табличку h
h <- diamonds
glimpse(h)
help(diamonds)

# диаграмма рассеяния:
qplot(data = h, carat, price)
bg <- qplot(data = h, log(carat), log(price))
bg + geom_hex()  # диаграмма рассеяния и шестиугольники плотности

# загружаем данные по стоимости квартир в Москве предварительно нужно установить
# рабочую папку Session --- Set working directory --- To source file location
f <- read.csv("flats_moscow.txt", sep = "\t", header = TRUE, dec = ".")

glimpse(f)  # краткое содержимое таблички f
qplot(data = f, totsp, price)  # диаграмма рассеяния
str(f)  # структура таблички f
qplot(data = f, log(totsp), log(price))  # диаграмма рассеяния в логарифмах

# мозаичный график
mosaic(data = f, ~walk + brick + floor, shade = TRUE)

# преобразуем переменны walk, brick, floor, code в факторные
f <- mutate_each(f, "factor", walk, brick, floor, code)
glimpse(f)  # краткое содержимое таблички f

qplot(data = f, log(price))  # гистограмма

# гистограмма для кирпичных и некирпичных домов
qplot(data = f, log(price), fill = brick)  # вариант А
qplot(data = f, log(price), fill = brick, position = "dodge")  # вариант Б

# оцененные функции плотности
qplot(data = f, log(price), fill = brick, geom = "density")

# добавляем прозрачность
g2 <- qplot(data = f, log(price), fill = brick, geom = "density", alpha = 0.5)

# несколько графиков получаемых путем видоизменения графика g2
g2 + facet_grid(walk ~ floor)
g2 + facet_grid(~floor)

# оценим три модели
model_0 <- lm(data = f, log(price) ~ log(totsp))
model_1 <- lm(data = f, log(price) ~ log(totsp) + brick)
model_2 <- lm(data = f, log(price) ~ log(totsp) + brick + brick:log(totsp))
# двоеточие в формуле модели в R --- произведение регрессоров

summary(model_0)  # базовый вариант отчета о модели
mtable(model_2)  # альтернативный вариант отчета


model_2b <- lm(data = f, log(price) ~ brick * log(totsp))
# умножение в формуле модели в R --- сами регрессоры и их произведение

# сравнение двух моделей
mtable(model_2, model_2b)

# оценки коэффициентов визуально
sjp.lm(model_2)

# создаем новый набор данных для прогнозирования
nw <- data.frame(totsp = c(60, 60), brick = factor(c(1, 0)))
nw

# точечный прогноз логарифма цены
predict(model_2, newdata = nw)
# переходим от логарифма к цене
exp(predict(model_2, newdata = nw))

# доверительный интервал для среднего значения y
predict(model_2, newdata = nw, interval = "confidence")  # для логарифма
exp(predict(model_2, newdata = nw, interval = "confidence"))  # для исходной переменной

# предиктивный интервал для конкретного значения y
predict(model_2, newdata = nw, interval = "prediction")
exp(predict(model_2, newdata = nw, interval = "prediction"))


# F-тест
waldtest(model_0, model_1)  # H_0: model_0 H_a: model_1
# H_0 отвергается

waldtest(model_1, model_2)  # H_0: model_1 H_a: model_2
# H_0 отвергается

waldtest(model_0, model_2)  # # H_0: model_0 H_a: model_2
# H_0 отвергается


# добавляем линию регрессии на диаграмму рассеяния
gg0 <- qplot(data = f, log(totsp), log(price))
gg0 + stat_smooth(method = "lm")
gg0 + stat_smooth(method = "lm") + facet_grid(~walk)
gg0 + aes(col = brick) + stat_smooth(method = "lm") + facet_grid(~walk)

# по-другому зададим дамми переменную
f$nonbrick <- memisc::recode(f$brick, 1 <- 0, 0 <- 1)
glimpse(f)

# сознательно попробуем попасть в ловушку дамми-переменных
model_wrong <- lm(data = f, log(price) ~ log(totsp) + brick + nonbrick)
summary(model_wrong)

# сравним три модели в одной табличке:
mtable(model_0, model_1, model_2)

# тест Рамсея
resettest(model_2)
