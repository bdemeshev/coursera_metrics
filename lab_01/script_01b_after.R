library("psych")
library("dplyr")
library("ggplot2")
library("GGally")

d <- cars
glimpse(d)
help(cars)
head(d)
tail(d)
describe(d)
ncol(d)
nrow(d)
str(d)

mean(d$speed)

d2 <- mutate(d, speed=1.67*speed, 
             dist=0.3*dist, ratio=dist/speed)
glimpse(d2)

qplot(data=d,dist)
qplot(data=d,dist,xlab="Длина тормозного пути (м)",
      ylab="Число машин",main="Данные по машинам 1920х")

qplot(data=d,speed,dist)


model <- lm(data=d, dist~speed)
model

coef(model)
residuals(model)
fitted(model)
deviance(model)
TSS <- sum((d$dist-mean(d$dist))^2)
TSS
r2 <- 1-deviance(model)/TSS
r2

model.matrix(model)

nd <- data.frame(speed=c(40,60))
nd

predict(model,nd)

t <- swiss
help(swiss)
glimpse(t)
describe(t)
ggpairs(t)

model2 <- lm(data=t,
             Fertility~Agriculture+Education+Catholic)
coef(model2)
fitted(model2)
residuals(model2)
deviance(model2)

report <- summary(model2)
report
report$r.squared

cor(t$Fertility,fitted(model2))^2

nd2 <- data.frame(Agriculture=0.5,Catholic=0.5,
                  Education=20)
predict(model2,nd2)


