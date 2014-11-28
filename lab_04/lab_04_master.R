library("HSAUR")
library("dplyr")
library("psych")
library("lmtest")
library("glmnet") # LASSO
library("car") # compute vif()
library("MASS") # ridge regression

# Multicollinearity

h <- cars
plot(h)
h <- mutate(h, speed2=speed^2, speed3=speed^3)
m.poly3 <- lm(dist~speed+I(speed^2)+I(speed^3),data=h)
# тут рассказать про смысл I(speed)
summary(m.poly3)
# отдельные коэф против регрессии
X0 <- model.matrix(~0+speed+speed2+speed3,data=h)
cor(X0)
vif(m.poly3)

# не было в лекции
X <- model.matrix(m.poly3)
XX <- t(X) %*% X
eigen <- eigen(XX)
eigen$values
CI <- sqrt(max(eigen$values)/min(eigen$values))
CI

vcov(m.poly3)
confint(m.poly3)
coeftest(m.poly3)

new <- data.frame(speed=10)
predict(m.poly3,newdata=new,interval='prediction')


m.line <- lm(dist~speed,data=h)
predict(m.line,newdata=new,interval="prediction")
summary(m.line)



# LASSO + Ridge
# убывающая последовательность (!)
lambdas <- seq(50,0.1,length=30) # для этих лямбд будем делать RR

y <- h$dist
X0 <- model.matrix(~0+speed+speed2+speed3,data=h)
fit <- glmnet(X0,y,alpha=1,lambda = lambdas)

plot(fit,xvar="lambda",label=TRUE)
plot(fit,xvar="dev",label=TRUE)
plot(fit,xvar="norm",label=TRUE)

coef(fit,s=c(0.1,0.9))

predict(fit,newdata=nx,s=0.1)

# отбор лямбы

cv.fit <- cv.glmnet(X0,y,alpha=0)
plot(cv.fit)

coef(cv.fit,s="lambda.min")
cv.fit$lambda.min
cv.fit$lambda.1se

predict(cf.fit,newdata=nx,s="lambda.1se")

# PCA

h <- heptathlon
glimpse(h)
help(heptathlon)

h <- select(h,-score)
cor(h)
describe(h)

h.pca <- prcomp(h,scale=TRUE)
pc1 <- h.pca$x[,1]
v1 <- h.pca$rotation[,1]

summary(h.pca)
cor(heptathlon$score,pc1)

plot(h.pca)
biplot(h.pca,xlim = c(-1,1))
