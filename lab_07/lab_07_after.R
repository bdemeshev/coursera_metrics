# Esli russkie bukvi prevratilitis v krakozyabry, to File - Reopen with
# encoding... - UTF-8 - Set as default - OK

# lab 07

library("dplyr")  # манипуляции с данными
library("erer")  # расчет предельных эффектов
library("vcd")  # графики для качественных данных
library("ggplot2")  # графики
library("reshape2")  # манипуляции с данными
library("AUC")  # для ROC кривой

# при загрузке файлов R автоматом переделывает все строковые переменные в
# факторные эта шаманская команда просит R так не делать :)
options(stringsAsFactors = FALSE)

# читаем данные по пассажирам Титаника
t <- read.csv("titanic3.csv")
# источник и описание:
# http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html

# смотрим на набор данных
glimpse(t)

# объясняем R, какие переменные считать факторными
t <- mutate(t, sex = as.factor(sex), pclass = as.factor(pclass), survived = as.factor(survived))
summary(t)

# мозаичный график
mosaic(data = t, ~sex + pclass + survived, shade = TRUE)

# график-виолончель
qplot(data = t, x = survived, y = age, geom = "violin")

# и старый добрый 'ящик с усами'
qplot(data = t, x = survived, y = age, geom = "boxplot")

# два варианта сглаженной функции плотности
ggplot(data = t, aes(x = age, y = ..count.., fill = survived)) + geom_density(position = "stack")
ggplot(data = t, aes(x = age, y = ..count.., fill = survived)) + geom_density(position = "fill")

# !!!! отличие от видеолекции !!!!
# вместо ggplot() в видеолекции 7.2.1. используется qplot()
# опция `position` исчезла из команды qplot() при обновлении пакета ggplot2 до версии 2.0


# Оценивание логит и пробит моделей
m_logit <- glm(data = t, survived ~ sex + age + pclass + fare, family = binomial(link = "logit"),
  x = TRUE)
m_probit <- glm(data = t, survived ~ sex + age + pclass + fare, family = binomial(link = "probit"),
  x = TRUE)
# отчеты об оценке моделей
summary(m_logit)
summary(m_probit)

# оценка ковариационной матрицы оценок коэффициентов
vcov(m_logit)

# создаём новый массив данных для прогнозирования
newdata <- data.frame(age = seq(from = 5, to = 100, length = 100), sex = "male",
  pclass = "2nd", fare = 100)
# посмотрим на начало этой таблички
head(newdata)

# прогнозируем по логит модели
pr_logit <- predict(m_logit, newdata, se = TRUE)
# соединим прогнозы и новый массив данных в единую табличку:
newdata_pr <- cbind(newdata, pr_logit)
head(newdata_pr)  # глянем на начало таблички

# применив логистическую функцию распределения получим границы доверительного
# интервала
newdata_pr <- mutate(newdata_pr, prob = plogis(fit), left_ci = plogis(fit - 1.96 *
  se.fit), right_ci = plogis(fit + 1.96 * se.fit))
head(newdata_pr)  # глянем на результат

# посмотрим на графике как меняется доверительный интервал для вероятности
qplot(data = newdata_pr, x = age, y = prob, geom = "line") + geom_ribbon(aes(ymin = left_ci,
  ymax = right_ci), alpha = 0.2)


# проведем LR тест R при построении разных моделей автоматом использует
# максимальное количество полных наблюдений поэтому часто выходит, что
# ограниченная и неограниченная модель оцениваются на разном наборе данных но в
# таком случае их нельзя сравнивать с помощью LR теста поэтому мы сначала
# создадим набор данных t2 без пропущенных значений и на нем оценим короткую и
# длинную модели H0: beta(pclass)=0, beta(fare)=0
t2 <- select(t, sex, age, pclass, survived, fare) %>% na.omit()
# оцениваем ограниченную модель
m_logit2 <- glm(data = t2, survived ~ sex + age, family = binomial(link = "logit"),
  x = TRUE)
# проводим LR тест
lrtest(m_logit, m_logit2)


maBina(m_logit)  # предельные эффекты для среднестатистического пассажира

# усредненные предельные эффекты по всем пассажирам
maBina(m_logit, x.mean = FALSE)

# обычный МНК
m_ols <- lm(data = t, as.numeric(survived) ~ sex + age + pclass + fare)
summary(m_ols)

# прогнозы по обычному МНК
pr_ols <- predict(m_ols, newdata)
head(pr_ols)

# ROC кривая спрогнозируем скрытую переменную для исходного набора данных
pr_t <- predict(m_logit, t, se = TRUE)
# соединим прогнозы с исходным набором данных
t <- cbind(t, pr_t)
# применим логистическую функцию распределения, чтобы получить вероятности
t <- mutate(t, prob = plogis(fit))
# глянем выборочно на результат:
select(t, age, survived, prob)

# получим все данные для ROC кривой:
roc.data <- roc(t$prob, t$survived)
str(roc.data)

# три графика для выбора порога отсечения по горизонтали --- пороги, по вертикали
# --- чувствительность чувствительность = число выживших верно предсказанных
# выжившими / общее количество выживших
qplot(x = roc.data$cutoffs, y = roc.data$tpr, geom = "line")

# по горизонтали --- пороги, по вертикали --- процент ложноположительных
# прогнозов процент ложно положительных прогнозов = число погибших ошибочно
# предсказанных выжившими/общее число погибших
qplot(x = roc.data$cutoffs, y = roc.data$fpr, geom = "line")

# по горизонтали --- процент ложноположительных прогнозов по вертикали ---
# чувствительность
qplot(x = roc.data$fpr, y = roc.data$tpr, geom = "line")
