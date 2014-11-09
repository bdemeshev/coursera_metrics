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
m.poly3 <- lm(dist~speed+I(speed^2)+I(speed^3),data=h)
# тут рассказать про смысл I(speed)
summary(m.poly3)
# отдельные коэф против регрессии
X <- model.matrix(m.poly3)
cor(X)
vif(m.poly3)

# не было в лекции
XX <- t(X) %*% X
eigen <- eigen(XX)
eigen$values
CI <- sqrt(max(eigen$values)/min(eigen$values))
CI

vcov(m.poly3)
confint(m.poly3)
coeftest(m.poly3)

new <- data.frame(speed=10)
predict(m.poly3,newdata=new,interval='confidence')


m.line <- lm(dist~speed,data=h)
summary(m.line)

# RR
lambdas <- seq(50,0.1,length=30) # для этих лямбд будем делать RR
m.rr <- lm.ridge(dist~speed+I(speed^2)+I(speed^3),lambda=lambdas,data=h)

head(coef(m.rr))
plot(m.rr)

# LASSO
X <- model.matrix(~0+speed+I(speed^2)+I(speed^3),data=h)
y <- h$dist
fit <- glmnet(X,y,alpha=1)

fit$a0

t(fit$beta)

cv.fit <- cv.glmnet(X,y,alpha=0)
coef(cv.fit)
plot(cv.fit)
cv.fit$lambda.min
plot(cv.fit$glmnet.fit,xvar="lambda",label=TRUE)
plot(cv.fit,xvar="dev",label=TRUE)
plot(fit,xvar="norm",label=TRUE)

best <- cv.fit$glmnet.fit
best$beta
best$lambda
coef(cv.fit)
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
v1

summary(h.pca)
cor(heptathlon$score,pc1)

plot(h.pca)


biplot(h.pca,xlim = c(-1,1))
