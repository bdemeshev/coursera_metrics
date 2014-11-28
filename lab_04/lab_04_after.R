library("HSAUR")
library("dplyr")
library("psych")
library("lmtest")
library("glmnet")
library("ggplot2")
library("car")

h <- cars
qplot(data=h, speed, dist)

h <- mutate(h, speed2=speed^2, speed3=speed^3)
model_mk <- lm(data=h, dist~speed+speed2+speed3)
summary(model_mk)

vif(model_mk)
X0 <- model.matrix(data=h, dist~0+speed+speed2+speed3)
head(X0)
cor(X0)

confint(model_mk)
coeftest(model_mk)

nd <- data.frame(speed=10,speed2=100,speed3=1000)
predict(model_mk,newdata=nd,interval="prediction")

model <- lm(data=h, dist~speed)
coeftest(model)

predict(model,newdata=nd,interval="prediction")

confint(model)
confint(model_mk)

# Ридж и LASSO
y <- h$dist
X0 <- model.matrix(data=h, dist~0+speed+speed2+speed3)

# LASSO
lambdas <- seq(50,0.1,length=30)
m_lasso <- glmnet(X0,y,alpha=1, lambda=lambdas)

plot(m_lasso,xvar="lambda",label=TRUE)
plot(m_lasso,xvar="dev",label=TRUE)
plot(m_lasso,xvar="norm",label=TRUE)

coef(m_lasso,s=c(0.1,1))

m_rr <- glmnet(X0,y,alpha=0, lambda=lambdas)

# cross validation
cv <- cv.glmnet(X0,y,alpha=1)
plot(cv)

cv$lambda.min
cv$lambda.1se

coef(cv,s="lambda.1se")

# PCA

h <- heptathlon
help(heptathlon)
glimpse(h)
h <- select(h,-score)
describe(h)

cor(h)

h.pca <- prcomp(h,scale=TRUE)
pca1 <- h.pca$x[,1]
v1 <- h.pca$rotation[,1]
v1

head(pca1)
summary(h.pca)

cor(heptathlon$score,pca1)
plot(h.pca)
biplot(h.pca,xlim=c(-1,1))

