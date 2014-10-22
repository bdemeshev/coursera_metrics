library("lubridate") # работа с датами

library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("bstats") # больше тестов
library("zoo") # временные ряды
library("xts") # еще ряды
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики

library("quantmod") # загрузка с finance.google.com
library("rusquant") # загрузка с finam.ru
library("stathse") # загрузка с sophist.hse.ru

# гетероскедастичность

# отличная документация к пакету sandwich: и теория, и инструкция
vignette("sandwich")

data("PublicSchools")
help(PublicSchools)
d <- PublicSchools
str(d)
qplot(data=d,Income,Expenditure)
d <- mutate(d,Income=Income/1000,Income2=Income^2)
model <- lm(data=d,Expenditure~Income+Income2)
summary(model)

coeftest(model)
coeftest(model,vcov. = vcovHC)
coeftest(model,vcov. = vcovHC(model,type="HC2"))

# графики
d_plus <- augment(model,d)
str(d_plus)
qplot(data=d_plus,x=Income,y=abs(.resid))

# тест Голдфельда-Квандта
d_ord <- arrange(d,Income)
model_ord <- lm(data=d_ord,Expenditure~Income+Income2)
gqtest(model_ord,fraction = 0.05)

# тест Уайта
white.test(model)

# гетероскедачичность и неверная спецификация
h <- read.table("flats_moscow.txt",header = TRUE)
str(h)
glimpse(h)
qplot(totsp, price, data = h)
model <- lm(price ~ totsp, data = h)
# отчеты
summary(model)
coeftest(model)
confint(model)

# график остатков
h_plus <- augment(model,h)
glimpse(h_plus)
qplot(data=h_plus,totsp,abs(.resid))
qplot(data=h_plus,totsp,abs(.stdresid))


# разные оценки дисперсий
vcov(model)
vcovHC(model)
vcovHC(model,type="HC2")


# корректируем
coeftest(model,vcov. = vcovHC(model))
conftable <- coeftest(model, vcov = vcovHC(model))
ci <- data.frame(estimate = conftable[, 1], sd_hc = conftable[, 2])
ci$left_95 <- ci$estimate - qnorm(0.975) * ci$sd_hc
ci$right_95 <- ci$estimate + qnorm(0.975) * ci$sd_hc
ci

# тест Голдфельда-Квандта
h_ord <- arrange(h,totsp)
model_ord  <- lm(price ~ totsp, data = h_ord)
gqtest(model_ord,fraction = 0.05)
res <- gqtest(model_ord,fraction = 0.05)
res$statistic

# тест Уайта
white.test(model)

# переход к логарифмам
h_ord <- mutate(h_ord, ln_price=log(price),ln_totsp=log(totsp))
qplot(data=h_ord,ln_totsp,ln_price)

model2 <- lm(ln_price ~ ln_totsp, data = h_ord)

# два теста 
white.test(model2)
gqtest(model2,fraction=0.05)
# сравнить результаты
