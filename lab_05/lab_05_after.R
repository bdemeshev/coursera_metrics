library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики

h <- read.table("flats_moscow.txt", header=TRUE)
head(h)
tail(h)

qplot(data=h, x=totsp, y=price)

model <- lm(price~totsp, data=h)
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

bptest(model)
bptest(model, data=h, varformula = ~ totsp + I(totsp^2) )
bptest(model, data=h, varformula = ~ poly(totsp, 2) )

gqtest(model, order.by = ~totsp, data=h, fraction = 0.2)

qplot(data=h, log(totsp), log(price))

model2 <- lm(data=h, log(price)~log(totsp))
gqtest(model2, order.by = ~totsp, data=h, fraction = 0.2)
