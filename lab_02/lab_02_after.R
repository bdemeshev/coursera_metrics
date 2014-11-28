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

# генерируем случайные величины
# Z_1, ...., Z_100 ~ N(5, 9)
z <- rnorm(100, mean=5, sd=3)
z[56]
z[2:9]

qplot(z)

# построим функцию плотности

x <- seq(-10,15,by=0.5)
y <- dnorm(x, mean=5, sd=3)
qplot(x,y)
qplot(x,y, geom="line")

# P(Z<3)
# P(Z<3)=F(3)

pnorm(3, mean=5, sd=3)

# P(Z \in [4;9])
# P(Z<9)-P(Z<4)
pnorm(9, mean=5, sd=3) - pnorm(4, mean=5, sd=3)

# P(Z<a)=0.7 a?
qnorm(0.7, mean=5, sd=3)

# chisq, t, f
rchisq, dchisq, pchisq, qchisq
rt, dt, pt, qt


# множественная регрессия. проверка гипотез

h <- swiss
glimpse(h)
help(swiss)

model <- lm(data=h, Fertility~Catholic+Agriculture+Examination)
summary(model)

coeftest(model)
confint(model)
sjp.lm(model)

# проверка гипотезы b_Cath=b_Agri

model_aux <- lm(data=h, 
      Fertility~Catholic+I(Catholic+Agriculture)+Examination)
summary(model_aux)

linearHypothesis(model, "Catholic-Agriculture=0")

# стандартизированные коэффициенты

h_st <- mutate_each(h, "scale")
glimpse(h_st)
model_st <- lm(data=h_st, 
               Fertility~Catholic+Agriculture+Examination)
summary(model_st)
sjp.lm(model, showStandardBeta = TRUE)


# искусственный эксперимент

D <- matrix(nrow=100, rnorm(100*41,mean=0,sd=1))
df <- data.frame(D)
glimpse(df)

model_pusto <- lm(data=df, X1~. )
summary(model_pusto)

# сравнить несколько моделей

model2 <- lm(data=h, Fertility~Catholic+Agriculture)
compar_12 <- mtable(model, model2)
compar_12

# сохранение результатов работы
stuff <- list(data=h, model=model2)
saveRDS(file = "mydata.RDS",stuff)

mylist <- readRDS("mydata.RDS")
summary(mylist$model)

# csv. comma separated values

t <- read.csv("flats_moscow.txt")
glimpse(t)

t <- read.csv("flats_moscow.txt", sep="\t", dec=".", header=TRUE)
glimpse(t)

mod_3 <- lm(data=t, price~totsp+brick)
summary(mod_3)

# RLMS 

df <- read.spss("r21i_os24a.sav", to.data.frame=TRUE,
                reencode="UTF-8")
glimpse(df)

df_sel <- select(df, qm1, qm2, qh6)
glimpse(df_sel)

describe(df_sel)

df2 <- mutate(df_sel,
              age=2012-qh6,
              ves=ifelse(qm1>10^6,NA,qm1),
              rost=ifelse(qm2>10^6,NA,qm2))
describe(df2)

qplot(data=df2, x=rost, y=ves)
qplot(data=df2, x=rost, y=ves) + geom_hex()

df3 <- select(df2, rost, ves, age)
df4 <- filter(df3, age>50)
qplot(data=df4, x=rost, y=ves) + geom_hex()
