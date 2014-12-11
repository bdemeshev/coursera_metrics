# ex 03 data
library("lmtest")
h <- read.csv("flats_moscow.txt", sep="\t")
library("ggplot2")
str(h)
qplot(data=h, log(totsp), log(price))

model <- lm(data=h, log(price)~log(totsp))
summary(model)
model <- lm(data=h, price~totsp)

vcov(model)
report <- summary(model)
report$sigma^2
34*34
d <- c(1,60)
t(d) %*% vcov(model) %*% d
nd <- data.frame(totsp=60)
predict(model, newdata = nd, se.fit = TRUE, interval = "prediction")
?predict.lm

colnames(h)
m2 <- lm(log(price)~log(totsp)+ brick + log(kitsp) + metrdist + walk  +log(livesp)   ,data=h)
summary(m2)

m3 <- lm(log(price)~log(totsp)+ brick + log(kitsp)   +log(livesp)   ,data=h)
summary(m3)
deviance(m2)
deviance(m3)
waldtest(m3,m2)


d <- read.dta("wages_in_the_USA/wages1.dta")
str(d)

m <- lm(wage~school+exper, data=d)
summary(m)
mtable(m)


