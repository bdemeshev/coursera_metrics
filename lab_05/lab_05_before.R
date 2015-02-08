# library("devtools")
# install_github("dgrtwo/broom")


library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("bstats") # больше тестов
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики

# гетероскедастичность
h <- read.table("flats_moscow.txt",header=TRUE)
head(h)
tail(h)

qplot(data=h,x=totsp, y=price)

model <- lm(price~totsp,data=h)
summary(model)
coeftest(model)
confint(model)

vcov(model)

h <- augment(model,h)
glimpse(h)
qplot(data=h,totsp,abs(.resid))

vcov(model)
vcovHC(model)
vcovHC(model,type="HC2")

coeftest(model)
coeftest(model,vcov. = vcovHC(model))

conftable <- coeftest(model,vcov. = vcovHC(model))
ci <- data.frame(estimate=conftable[,1],
                 se_hc=conftable[,2])
ci
ci <- mutate(ci,left_ci=estimate-1.96*se_hc,
             right_ci=estimate+1.96*se_hc)
ci

confint(model)

# White test
white.test(model)
# H0: Var(epsilon_i)=const
res <- white.test(model)
res$statistic
res$p.value

# GQ test
h <- arrange(h,totsp)
model <- lm(price~totsp,data=h)
gqtest(model,fraction = 0.20,)
# H0: Var(epsilon_i)=const
# H0 отвергается

qplot(data=h,totsp,price)
h <- mutate(h, ln_totsp = log(totsp), ln_price=log(price))
qplot(data=h,ln_totsp,ln_price)

model2 <- lm(data=h, ln_price~ln_totsp)
white.test(model2)
white.test(model)
gqtest(model2)
gqtest(model)

summary(model)
summary(model2)
