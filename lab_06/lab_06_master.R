
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
library("sophisthse") # загрузка с sophist.hse.ru

# работаем с датами
x <- c("2010-01-17","2011-02-25")
y <- ymd(x)
str(y)
y + days(56)
y - months(2)
day(y)
month(y)
year(y)
vignette(package="lubridate")
vignette("lubridate")



# создание zoo объекта с нуля
x <- rnorm(5)
y <- ymd("2010-05-07") + days(1:5)
x
y

x_zoo <- xts(x,order.by = y)
x_zoo

# первые плюсы zoo
lag(x_zoo,-1)
merge(x_zoo,lag(x_zoo,-1))
merge(x_zoo,lag(x_zoo,+1))
merge(x_zoo,diff(x_zoo))

# регулярные ряды
xq <- zooreg(x,start = as.yearqtr("2010-02") , freq=4)
xq
xm <- zooreg(x,start = as.yearmon("2010-02") , freq=12)
xm

# перевод существующего объекта
data("Investment")
d <- as.zoo(Investment)
d_df <- as.data.frame(d)
coredata(d)
time(d)
start(d)
end(d)

# автоматическая замена NA
dna <- d
dna[2,3] <- NA
na.locf(dna)
na.approx(dna)





# sophist.hse.ru
a <- stathse("WAG_Q")
a

# finance.google.com
# finance.yahoo.com
# Sys.setlocale("LC_MESSAGES", "C") # смена локали
Sys.setlocale("LC_TIME", "C")
getSymbols(Symbols="AAPL",src="google",from="2012-01-01",to="2013-08-08")
str(AAPL)
head(AAPL)
tail(AAPL)

# finam.ru
getSymbols(Symbols="GAZP",src="Finam",from="2012-01-01",to="2013-08-08")
head(GAZP)
tail(GAZP)

# автокорреляция
data("Investment")
help(Investment)
d <- as.zoo(Investment)
head(d)
glimpse(as.data.frame(d))

# графики
autoplot(d)
autoplot(d[,1:2])
autoplot(d[,1:2], facets = NULL)  

# estimation
model <- lm(data=d, RealInv~RealInt+RealGNP)

# отчеты
summary(model)
coeftest(model)
confint(model)

# остатки
d_plus <- augment(model,as.data.frame(d))
glimpse(d_plus)
qplot(data=d_plus,lag(.resid),.resid)


# разные оценки дисперсий
vcov(model)
vcovHAC(model)


# корректируем
coeftest(model,vcov. = vcovHAC(model))
conftable <- coeftest(model, vcov. = vcovHAC(model))
ci <- data.frame(estimate = conftable[, 1], sd_hc = conftable[, 2])
ci$left_95 <- ci$estimate - qnorm(0.975) * ci$sd_hc
ci$right_95 <- ci$estimate + qnorm(0.975) * ci$sd_hc
ci

# Durbin-Watson
dwt(model)
result <- dwt(model)
result$dw
result$p

# Breusch–Godfrey test
bgtest(model)
bgtest(model,order = 3)
results <- bgtest(model)
results$p.value
