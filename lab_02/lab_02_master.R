# lab 2
library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")

# случайные величины в R

# сгенерировать
z <- rnorm(100, mean=3, sd=2)
z[1:10]
qplot(z)

# нарисовать функцию плотности
x <- seq(-10,10,by=0.1)
y <- dnorm(x, mean=3, sd=2)
qplot(x,y,geom="line")

# посчитать вероятность
# P(Z<5)
pnorm(5, mean=3, sd=2)
# P(Z \in [0;5])
pnorm(5, mean=3, sd=2) - pnorm(0, mean=3, sd=2)

# посчитать квантиль
# P(Z<a)=0.9 a?
qnorm(0.9, mean=5, sd=2)
# P(Z>a)=0.3 a?
# P(Z<a)=0.7
qnorm(0.7, mean=5, sd=2)

# распределения
# хи-квадрат
# rchisq, pchisq, dchisq, qchisq

# rt, pt, dt, qt

# rf, pf, df, qf

# P(t_{38}>a)=0.025
qt(0.975, df=38)



# модель множественной регрессии
h <- swiss
model <- lm(data=h, Fertility~Agriculture+Catholic+Examination)
summary(model)

coeftest(model)
confint(model)
sjp.lm(model)

vcov(model)

# линейная гипотеза

# способ 1:
model_mod <- lm(data=h, Fertility~Agriculture+I(Catholic+Agriculture)+Examination)
summary(model_mod)
# способ 2:
linearHypothesis(model, "Catholic-Agriculture=0")

# сравним несколько моделей рядом
model2 <- lm(data=h, Fertility~Agriculture+Catholic)
comp <- mtable("Model A"=model,"Model B"=model2)
comp

# стандартизированные коэффициенты
h_st <- mutate_each(h,"scale")
model_st <- lm(data=h_st, Fertility~Agriculture+Catholic+Examination)
summary(model_st)
sjp.lm(model,showStandardBeta = TRUE)

# проблема множественного сравнения

rnd <- matrix(rnorm(100*41),nrow=100)
random_df <- data.frame(rnd)
glimpse(random_df)

m3 <- lm(data=random_df,X1~.)
summary(m3)

# корректировка

pvals <- sort(coeftest(model)[,4])
pvals_corrected <- BH(pvals)
pvals_corrected$Adjusted.pvalues
pvals
summary(pvals_corrected)

# сохранение --- чтение данных

stuff <- list(data=h, model=model)
str(stuff)
saveRDS(stuff,"mystuff.RDS")

all <- readRDS("mystuff.RDS")
all$data
summary(all$model)

# csv

# если повезет:
h <- read.csv("flats_moscow.txt")
str(h)

h <- read.csv("flats_moscow.txt",sep="\t", header=TRUE, dec=".")
str(h)


# из экселя в R
# csv в России --- больше, чем csv (!)
# русский эксель: sep=";", dec=",", header=TRUE


mod <- lm(data=h, price~totsp+brick)
summary(mod)

# RLMS

df <- read.spss("r21i_os24a.sav", to.data.frame=TRUE)
glimpse(df)

df_sel <- select(df, qm1, qm2, qh6)
describe(df_sel)

df_clean <- mutate(df_sel, qm1=ifelse(qm1>10^6,NA,qm1),
                   qm2=ifelse(qm2>10^6,NA,qm2))
df2 <- mutate(df_clean, age=2012-qh6)
describe(df2)


qplot(data=df2, x=qm1, y=qm2)
qplot(data=df2, x=qm1, y=qm2) + geom_hex()

df3 <- filter(df2, age>30)
qplot(data=df3, x=qm1, y=qm2) 
qplot(data=df3, x=qm1, y=qm2) + geom_hex()



