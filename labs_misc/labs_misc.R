# этот блок отдельным файлом! возможно в лекцию 1!
# блок: установка пакетов

library("devtools")
install_github("bdemeshev/rusquant")
install_github("bdemeshev/stathse")
install_github("dgrtwo/broom")


# блок: R вместо таблиц
library("ggplot2")

# построим график функции плотности
x <- seq(from=0,to=10,length=100)
head(x)

y <- dchisq(x,df=3)
qplot(x,y)
qplot(x,y,geom="line")

y <- dchisq(x,df=1)
qplot(x,y,geom="line")

# Найдем вероятности
# X~N(4,8^2), P(X <= 5)?
# X~N(3,2^2), P( 0< X < 5)?
# X~N(0,1), P(X>2)?

# Найдем квантили
# X~N(1,3^2), P(X<a)=0.95, a?
# X~chi2(5), P(X>a)=0.025, a?

# блок: написание функции

## мой квадрат

## с параметром

## доля NA в data.frame (и заодно как писать функцию)

sum(is.na(d))/nrow(d)/ncol(d)

## проверка корректности (функция в окружении врагов)


# цикл

## игрушечный
for (i in 5:10) {
  r <- i^2
  cat("i=",i," r=",r,"\n")
}

## для новичка: если ты используешь цикл, скорее всего есть более эффективная функция в R

## загрузка пачки файлов 
library("dplyr")
all_data <- NULL
for (fname in c("file01.csv","file02.csv")) {
  df <- read.csv(fname,header=TRUE)
  all_data <- rbind_list(all_data,df)
}
all_data



