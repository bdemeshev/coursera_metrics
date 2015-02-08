# library("devtools")
# install_github("dgrtwo/broom")
# install_github("cran/bstats")
# install_github("bdemeshev/rusquant")
# install_github("bdemeshev/sophisthse")

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
library("Quandl") # загрузка с Quandl

# 
x <- c("2012-04-15","2011-08-17")
y <- ymd(x)
y
y + days(20)
y - years(10)
day(y)
month(y)
year(y)
vignette("lubridate")

# 
x <- rnorm(5)
x
y <- ymd("2014-01-01")+days(0:4)
y
ts <- zoo(x,order.by=y)
ts

lag(ts,-1)
lag(ts,1)
diff(ts)

ts2 <- zooreg(x,start=as.yearqtr("2014-01"),freq=4)
ts2
ts3 <- zooreg(x,start=as.yearmon("2014-01"),freq=12)
ts3

data("Investment")
help("Investment")

start(Investment)
end(Investment)
time(Investment)
coredata(Investment)

dna <- Investment
dna[1,2] <- NA
dna[5,3] <- NA
na.approx(dna)
na.locf(dna)

# загрузка данных
a <- sophisthse("POPNUM_Y")
a

# quandl
b <- Quandl("FRED/GNP")
b

# finance.google.com
Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "AAPL",from="2010-01-01",
           to="2014-02-03",src="google")
head(AAPL)
tail(AAPL)

# finam.ru
getSymbols(Symbols="GAZP",from="2011-01-02",
           to="2014-09-09",src="Finam")
head(GAZP)
tail(GAZP)

plot(GAZP)
autoplot(GAZP[,1:4])
autoplot(GAZP[,1:4],facets = NULL)
chartSeries(GAZP)

d <- as.zoo(Investment)
autoplot(d[,1:2],facets = NULL)

model <- lm(data=d, RealInv~RealInt+RealGNP)

summary(model)
coeftest(model)
confint(model)

d_aug <- augment(model, as.data.frame(d))
glimpse(d_aug)
qplot(data=d_aug,lag(.resid),.resid)

vcov(model)
vcovHAC(model)

coeftest(model,vcov. = vcovHAC(model))
conftable <- coeftest(model,vcov. = vcovHAC(model))
ci <- data.frame(estimate=conftable[,1],
                 se_ac=conftable[,2])
ci <- mutate(ci,left_95=estimate-1.96*se_ac,
             right_95=estimate+1.96*se_ac)
ci

# Durbin-Watson
# H0: нет автокорреляции
# Ha: автокорреляции 1-го порядка
dwt(model)
res <- dwt(model)
res$dw
res$p
res$r

# BG-test
# H0: нет автокорреляции
# Ha: автокорреляция k-го порядка
bgtest(model,order = 2)
# H0 не отвергается
res <- bgtest(model,order = 2)
res$statistic
res$p.value



