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
# построение вспомогательной модели
model_aux <- lm(data=h, Fertility~Catholic+I(Catholic+Agriculture)+Examination)
summary(model_aux)

# проверка гипотезы без построения вспомогательной модели
linearHypothesis(model, "Catholic-Agriculture=0")

# стандартизированные коэффициенты

# масштабируем каждую переменную (вычитаем среднее, делим на стандартную ошибку)
h_st <- mutate_each(h, "scale")
glimpse(h_st) # смотрим на новый набор данных
# оцениваем модель по стандартизированным данным
model_st <- lm(data=h_st, Fertility~Catholic+Agriculture+Examination)
summary(model_st) # отчет о новой модели

# графическое представление стандартизованных коэффициентов
sjp.lm(model, showStandardBeta = TRUE) # в старой версии пакета sjPlot
sjp.lm(model, type = "std") # в новой версии пакета sjPlot:
# должна сработать одна из двух команд в зависимости от установленной версии

# искусственный эксперимент

# матрица в 100 строк, слепленная из вектора в котором 4100 элементов
D <- matrix(nrow=100, rnorm(100*41,mean=0,sd=1))
df <- data.frame(D) # переводим матрицу в табличку данных 
glimpse(df)

# пытаемся определить зависимость X1 от остальных переменных
model_pusto <- lm(data=df, X1~. ) 
summary(model_pusto)

# сравниваем несколько моделей
model2 <- lm(data=h, Fertility~Catholic+Agriculture)
compar_12 <- mtable(model, model2)
compar_12

# сохранение результатов работы
stuff <- list(data=h, model=model2) # список ценных объектов
saveRDS(file = "mydata.RDS",stuff) # сохраняем всё ценное в файл

mylist <- readRDS("mydata.RDS") # читаем из файла что там есть
summary(mylist$model)

# формат csv. comma separated values

# первая (сознательно неудачная) попытка чтения файла
t <- read.csv("flats_moscow.txt")
glimpse(t)

# попытка чтения файла с указанием параметров:
t <- read.csv("flats_moscow.txt", sep="\t", dec=".", header=TRUE)
glimpse(t)

# простая модель зависимости цены от общей площади и кирпичности дома
mod_3 <- lm(data=t, price~totsp+brick)
summary(mod_3)

# читаем данные RLMS 

# предварительно их нужно скачать с http://www.hse.ru/rlms/spss
# для примера взята 21 волна, индивиды, репрезентативная выборка

# командой из пакета rlms
h <- read.rlms("r21i_os24a.sav")
# если пакет rlms почему-то не установился, то 
# нужно убрать комментарий и выполнить следующую строку:
# h <- read.spss("r21i_os24a.sav", to.data.frame=TRUE, reencode="UTF-8")
glimpse(h)

# отбираем часть переменных из таблички h в табличку h2
h2 <- select(h, qm1, qm2, qh6, qh5)
glimpse(h2) # смотрим на df_sel

# переименовываем переменные
h3 <- rename(h2, ves=qm1, rost=qm2, sex=qh5, b_year=qh6)
# добавляем возраст
h3 <- mutate(h3, vozrast=2012-b_year)

describe(h3) # описательные статистики

summary(h3$sex) # таблица частот

# отберём мужчин в отдельную табличку
h4 <- filter(h3, sex=="МУЖСКОЙ")

# диаграмма рассеяния
qplot(data=h4, rost, ves)

# гистограмма
qplot(data=h4, ves)
