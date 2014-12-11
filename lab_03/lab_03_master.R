library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("ggplot2")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")

# Переход к логарифмам 1.
h <- diamonds
qplot(data=h, carat, price)
qplot(data=h, log(carat), log(price))

# Переход к логарифмам 2.
f <- read.csv("flats_moscow.txt", sep="\t", dec=".", header=TRUE)
glimpse(f)
qplot(data=f, totsp, price)
qplot(data=f, log(totsp), log(price))

# особенности графиков
mosaic(data=f, ~walk+brick+floor, shade = TRUE)
# найти черную кошку в черной комнате:
qplot(data=h, log(carat), log(price)) + geom_hex()

# количественная и качественная
qplot(data=f, x=log(price), geom="histogram", fill=factor(brick))
qplot(data=f, x=log(price), fill=factor(brick), geom="histogram", 
      position="dodge")
qplot(data=f, x=log(price), geom="density", fill=factor(brick))
qplot(data=f, x=log(price), geom="density", fill=factor(brick), alpha=0.5)
qplot(data=f, x=log(price), geom="histogram") + facet_grid(brick~walk)
qplot(data=f, x=log(price), geom="histogram") + facet_grid(brick~walk) + aes(y=..density..)
qplot(data=f, x=log(price), geom="density") + facet_grid(brick~walk)


# Пример с дамми переменными
model_0 <- lm(data=f, log(price)~log(totsp))
model_1 <- lm(data=f, log(price)~log(totsp)+brick)
model_2 <- lm(data=f, log(price)~log(totsp)+brick + log(totsp):brick)



# смысл двоеточия и умножения в формуле
model_2b <- lm(data=f, log(price)~log(totsp)*brick)

summary(model_2)
summary(model_2b)

# все вместе
mtable(model_0, model_1, model_2)
sjp.lm(model_2)


# интерпретация у чудо доски!!!



# прогнозирование!!!!


# три теста Вальда и вывод
waldtest(model_0, model_1)
waldtest(model_1, model_2)
waldtest(model_0, model_2)


# график с линиями
qplot(data=f, log(totsp), log(price), col=factor(walk)) + facet_grid(~brick) +
  stat_smooth(method="lm")

# неправильное включение дамми

f <- mutate(f, nonbrick = 1 - brick)
head(f)
model_wrong <- lm(data=f, log(totsp)~log(price)+brick+nonbrick)
summary(model_wrong)
mtable(model_wrong)


# reset-test
model_2 <- lm(data=f, log(totsp)~log(price)+brick + log(price):brick)
resettest(model_2)



# разное невошедшее :)


# http://eu.wiley.com/legacy/wileychi/verbeek2ed/datasets.html
# google: data Verbeek
# чтение данных из Stata
d <- read.dta("wages_in_the_USA/wages1.dta")
str(d)

m <- lm(wage~school+exper, data=d)
summary(m)
mtable(m)


model_1 <- lm(data=f, log(price)~log(totsp)+log(livesp)+log(kitsp)+brick+metrdist+walk)
model_2 <- lm(data=f, log(price)~log(totsp)+log(livesp)+log(kitsp)+brick)
mtable(model_1, model_2)
waldtest(model_1, model_2)


# RESET-test
# d <- read.dta("wages1.dta")
# glimpse(d)

model_0 <- lm(data=f, log(totsp)~log(price))
resettest(model_0)

