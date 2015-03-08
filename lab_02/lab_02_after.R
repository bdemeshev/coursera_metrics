# if you see KRAKOZYABRY then do 
# "File" - "Reopen with encoding" - "UTF-8" - (Set as default) - OK


# lab 2

# загружаем пакеты
library("memisc") # две и более регрессий в одной табличке
library("dplyr") # манипуляции с данными
library("psych") # описательные статистики
library("lmtest") # тестирование гипотез в линейных моделях
library("sjPlot") # графики
library("sgof")
library("ggplot2") # графики
library("foreign") # загрузка данных в разных форматах
library("car")
library("hexbin") # графики
library("rlms") # загрузка данных в формате rlms (spss)

# генерируем случайные величины
# Z_1, ...., Z_100 ~ N(5, 9)
z <- rnorm(100, mean=5, sd=3)
z[56] # z номер 56
z[2:9] # вектор из z_2, ..., z_9

qplot(z) # гистограмма

# построим функцию плотности

x <- seq(-10,15,by=0.5) # задаем последовательности чисел от -10 до 15 с шагом 0.5
y <- dnorm(x, mean=5, sd=3) # считаем в этих точках значение плотности для N(5,3^2)
qplot(x,y) # график точками
qplot(x,y, geom="line") # график линией

# Найдем P(Z<3)
# P(Z<3)=F(3). Данная вероятность --- это функция распределения
pnorm(3, mean=5, sd=3)

# Найдем P(Z \in [4;9])
# P(Z<9)-P(Z<4). Данная вероятность --- разность функций распределения 
pnorm(9, mean=5, sd=3) - pnorm(4, mean=5, sd=3)

# Найдем а, при котором P(Z<a)=0.7.
qnorm(0.7, mean=5, sd=3)

# аналогичные функции для других распределений: chisq, t, f
# rchisq, dchisq, pchisq, qchisq
# rt, dt, pt, qt
# rf, df, pf, qf


# множественная регрессия. проверка гипотез

h <- swiss # набор данных по кантонам Швейцарии (встроенный в R)
glimpse(h) # бросим взгляд на данные
help(swiss) 

# оценим модель множественной регрессии
model <- lm(data=h, Fertility~Catholic+Agriculture+Examination)
# посмотрим результаты оценивания
summary(model)

# отдельно табличка с тестами
coeftest(model)

confint(model) # доверительные интервалы для коэффициентов
sjp.lm(model) # графическое представление интервалов

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
