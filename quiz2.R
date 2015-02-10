data()
data(package = .packages(all.available = TRUE))

library(ggplot2)
data <- diamonds
model <- lm(data=data, price ~ carat)
summary(model)
model1 <- glm(data=data, price ~ 0 + carat)

model2 <- glm(data=data, price ~ carat + depth)
summary(model2)

model3 <- lm(data=data, price ~ carat+x+y)
summary(model3)

model_ln <- lm(data=data, log(price) ~ carat)
summary(model_ln)

model_ln1 <- lm(data=data, price ~ log(carat))
summary(model_ln1)

model_ln2 <- lm(data=data, log(price) ~ log(carat))
summary(model_ln2)

model_d <- glm(data=data, price ~ carat + color)
summary(model_d)

10117.99-62.56*1.645
10117.99+62.56*1.645

126.00-25.76*1.645
126.00+25.76*1.645

-1148.61-36.31*1.645
-1148.61+36.31*1.645

# Res 
200-105
# Unres 
200-175

(95-25)*(25-5)/(2*25)

200-125
200-175

(75-25)*(40-5)/(2*25)

210-130
210-180

(80-30)*(35-5)/(2*30)

h <- read.table("flats_moscow.txt", header=TRUE, sep = "\t", dec = ".")
glimpse(h)

model <- lm(data=h, price~livesp)
vcov(model)
coef(model)

V <- matrix(c(21.9,-0.46,-0.46,0.01),nrow=2)
V

s2 <- deviance(model)/(nrow(h)-2)
s2

library("dplyr")
x <- c(1,150)

t(x) %*% V %*% x
t(x) %*% V %*% x + s2



19+(100^2)*0.01+(80^2)*0.03+2*0.03*100+2*(-0.02)*100*80+2*80*(-0.45)
19+(90^2)*0.01+(70^2)*0.03+2*0.03+2*(-0.02)+2*(-0.45)
19+(80^2)*0.01+(50^2)*0.03+2*0.03+2*(-0.02)+2*(-0.45)

900+310.12
900+246.12
900+157.12

library(memisc)
library(lmtest)
model1 <- lm(data=data, price ~ carat)
summary(model1)
model2 <- lm(data=data, price ~ carat + depth)
model3 <- lm(data=data, price ~ carat + depth + cut)

mtable(model1, model2, model3)

resettest(model1)
resettest(model2)
resettest(model3)

qplot(data = data, log(price))
qplot(data = data, log(price), fill=cut)
qplot(data = data, log(price), fill=cut, position = "dodge")
qplot(data = data, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_grid(~cut)

qplot(data = data, log(price), fill=clarity, geom = "density", alpha = 0.5) + facet_wrap(~clarity)
qplot(data = data, log(price), fill=clarity, geom = "density", alpha = 0.5) + facet_grid(~clarity)

qplot(data = data, log(price), fill=color, geom = "density", alpha = 0.5) + facet_wrap(~color)


waldtest(model1, model2)
waldtest(model1, model3)
waldtest(model2, model3)

qplot(data=data, log(carat), log(price), color = clarity) + facet_wrap(~cut) 

round(pnorm(9, mean=7, sd=2), digits = 2)

0.2*2+0.2*4
1-(70/7)/(100/9)
1-(9/18)/(100/20)
1-(50/5)/(100/(7))

data <- sleep
mean(data$extra)
a <- subset(sleep, group==1)
b <- subset(sleep, group==2)
mean(a$extra)-mean(b$extra)
c <- var(a$extra)-var(b$extra)
round(c, digits = 2)

data(mtcars)
model <- lm(data=mtcars, mpg ~ disp + hp + wt + am)
summary(model)

a <- subset(mtcars, am==1)
b <- subset(mtcars, am==0)
model1 <- lm(data=a, mpg ~ hp + wt)
model2 <- lm(data=b, mpg ~ hp + wt)
summary(model1)
summary(model2)
round(-7.62486/-1.85591, digits = 1)

model1 <- lm(data=mtcars, mpg ~ disp + hp + wt + am)
model2 <- lm(data=mtcars, mpg ~ hp + wt + am)
model3 <- lm(data=mtcars, mpg ~ disp + wt + am)
model4 <- lm(data=mtcars, mpg ~ wt + am)
mtable(model1, model2, model3, model4)
