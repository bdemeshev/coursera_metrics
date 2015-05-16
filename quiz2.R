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

qnorm(0.95, mean=0,sd=1)
10117.99-62.56*1.645
10117.99+62.56*1.645

round(126.00-25.76*1.645, digits = 0)
round(126.00+25.76*1.645, digits = 0)

round(-1148.61-36.31*1.645, digits = 0)
round(-1148.61+36.31*1.645, digits = 0)

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

library("RColorBrewer")
qplot(data=data, log(carat), log(price), color = clarity) + facet_wrap(~cut)
# scale_color_brewer(palette="Spectral")
# scale_color_manual(values=c("#CC6666", "#9999CC", "#66CC99"))

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

mean(data$extra)^2
mean(data$extra)^3
mean(data$extra)^(1/2)
max(data$extra)-min(data$extra)
max(data$extra)+min(data$extra)
max(data$extra)*min(data$extra)
var(data$extra[1:10])
var(data$extra[5:14])
var(data$extra[10:20])

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
model2 <- lm(data=mtcars, mpg ~ cyl + hp + wt + am)
model3 <- lm(data=mtcars, mpg ~ disp + cyl + wt + am)
model4 <- lm(data=mtcars, mpg ~ disp + hp + cyl + am)
mtable(model1, model2, model3, model4)

model1 <- lm(data=mtcars, mpg ~ hp + wt + am)
model2 <- lm(data=mtcars, mpg ~ cyl + hp + wt)
model3 <- lm(data=mtcars, mpg ~ cyl + wt + am)
model4 <- lm(data=mtcars, mpg ~ hp + cyl + am)
mtable(model1, model2, model3, model4)


model1 <- lm(data=mtcars, mpg ~ disp + hp + wt)
model2 <- lm(data=mtcars, mpg ~ cyl + hp + wt)
model3 <- lm(data=mtcars, mpg ~ disp + cyl + wt)
model4 <- lm(data=mtcars, mpg ~ disp + cyl + hp)
mtable(model1, model2, model3, model4)

data(sleep)

library(ggplot2)
data(airquality)
qplot(airquality, x=airquality$Ozone, y=airquality$Solar.R)

model1 <- lm(data=airquality, Ozone ~ Solar.R + Wind + Temp)
library(car)
vif(model1)

library(glmnet)
d <- airquality
d <- na.omit(d)
y<-d$Ozone
X<-model.matrix(data = d, Ozone ~ 0 + Wind + Solar.R + Temp)
lambdas <- seq(50,0.1,length=30)
m_l<-glmnet(X,y,alpha=1,lambda = lambdas)
coef(m_l, s = 1)
plot(m_l,xvar="dev")


d <- na.omit(airquality)
y<-d$Ozone
X<-model.matrix(data = d, Ozone ~ 0 + Wind + Solar.R + Temp) 
lambdas <- seq(50,0.1,length=30) 
m_l<-glmnet(X,y,alpha=1,lambda = lambdas) 
coef(m_l, s = 1)

m_l<-glmnet(X,y,alpha=0,lambda = lambdas)
coef(m_l, s = 2)
round(,digits=3)

p<-prcomp(X,scale=TRUE)
p$x
qplot(x=p$x[,1], y=p$x[,2])

# Midterm
a<-(5+6-10)/(0.7^2+0.5^2+0.25*2)^(0.5)
round(a,digits=3)
qplot(data = diamonds, log(price),color=cut)+facet_grid(~cut)
qplot(data = diamonds, log(price),fill=cut)+facet_grid(~cut)
qplot(data = diamonds, price,fill=cut)+facet_grid(~clarity)
qplot(data = diamonds, price,fill=cut)+facet_wrap(~clarity)
model <- lm(data=data, price ~ carat + table + x + y + z + depth)
summary(model)
model2 <- lm(data=data, price ~ carat + table + x + y + depth)
summary(model2)
confint(model2,level=0.90)
round(-107.56266,digits=2)


((230-150)-(230-190))*(75-5)/(2*(230-190))


library("Ecdat")
library(lmtest)
data("BudgetFood")
model <- lm(data=BudgetFood, wfood~totexp+size)
reset(model)
nd <- data.frame(size=4, totexp=70000)
predict(model, newdata = nd, interval = "prediction",level=0.9)

library(car)
model <- lm(data=mtcars, mpg ~ disp + hp + wt)
summary(model)
vif(model)

X<-model.matrix(data = mtcars, mpg ~ 0 + disp + hp + wt)
p<-prcomp(X,scale=TRUE)
max(p$x[,1])

model1 <- lm(data=mtcars, mpg ~ p$x[,1] + p$x[,2])
model2 <- lm(data=mtcars, mpg ~ p$x[,1] + p$x[,2] + p$x[,3])

library(Ecdat)
library(sandwich)
library(lmtest)
data("Griliches")
model1 <- lm(data=Griliches, lw80 ~ age80 + iq + school80 + expr80)
vcov(model1)

vcovHC(model1, type = "HC3")
coeftest(model1,vcov. = vcovHC(model1,type="HC0"))
coeftest(model1,vcov. = vcovHC(model1,type="HC1"))
coeftest(model1,vcov. = vcovHC(model1,type="HC2"))
coeftest(model1,vcov. = vcovHC(model1,type="HC3"))

coeftest(model1,vcov. = vcovHC(model1,type="HC4m"))
coeftest(model1,vcov. = vcovHC(model1,type="HC5"))
coeftest(model1,vcov. = vcovHC(model1,type="HC3"))
coeftest(model1,vcov. = vcovHC(model1,type="HC1"))

bptest(data=Griliches, lw80 ~ age80 + iq + school80 + expr80, varformula= ~ age80)
bptest(data=Griliches, lw80 ~ age80 + iq + school80 + expr80, varformula= ~ iq)
bptest(data=Griliches, lw80 ~ age80 + iq + school80 + expr80, varformula= ~ expr80)

gqtest(data=Griliches, lw80 ~ age80 + iq + school80 + expr80, 
       order.by = ~ age80, fraction=0.2)
gqtest(data=Griliches, lw80 ~ age80 + iq + school80 + expr80, 
       order.by = ~ iq, fraction=0.2)
gqtest(data=Griliches, lw80 ~ age80 + iq + school80 + expr80, 
       order.by = ~ expr80, fraction=0.2)

data("Solow")
model1 <- lm(data=Solow, q ~ k + A)
vcov(model1)
vcovHAC(model1)
dwtest(model1)

model2 <- lm(data=Solow, q ~ k)
dwtest(model2)

model3 <- lm(data=Solow, q ~ A)
dwtest(model3)

bgtest(model1,order=3)
bgtest(model3,order=3)
bgtest(model2,order=3)

library(quantmod)
Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "AAPL",from="2010-01-01", to="2014-02-03",src="google")
plot(AAPL$AAPL.Close, main = "")

Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "GOOG",from="2010-01-01", to="2014-02-03",src="google")
plot(GOOG$GOOG.Close, main = "")

Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "MSFT",from="2010-01-01", to="2014-02-03",src="google")
plot(MSFT$MSFT.Close, main = "")

Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "GOOG",from="2010-01-01", to="2014-02-03",src="google")
a <- GOOG$GOOG.Close
a1 <- lag(a,1)
a2 <- lag(a,2)
model5 <- lm(data=a, a ~ a1 + a2)
summary(model5)

Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "MSFT",from="2010-01-01", to="2014-02-03",src="google")
a <- MSFT$MSFT.Close
a1 <- lag(a,1)
a2 <- lag(a,2)
model6 <- lm(data=a, a ~ a1 + a2)
summary(model6)

Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "INTC",from="2010-01-01", to="2014-02-03",src="google")
a <- INTC$INTC.Close
a1 <- lag(a,1)
a2 <- lag(a,2)
model6 <- lm(data=a, a ~ a1 + a2)
summary(model6)

library(forecast)
set.seed(40)
y <- arima.sim(n=100, list(ar=0.7))
tsdisplay(y)

# set.seed(40)
# z <- arima.sim(n=100, list(ar=0.7))
# tsdisplay(z)

set.seed(1)
y <- arima.sim(n=100, list(ar=0.99))
tsdisplay(y)

t<-c(1:100)
t2 <- t^2
t3 <- t^3

model <- lm(data=y, y ~ t + t2 + t3)
summary(model)

set.seed(10)
y <- arima.sim(n=100, list(ar=0.5))
m <- Arima(x=y, order=c(0.5,0,2))
summary(m)

library("sophisthse")
data <- sophisthse("HHI_Q_I")
data <- data[1:89]
m <- Arima(x=data$HHI_Q_DIRI, order=c(0,0,2))
summary(m)

775.41
698.12 
752.81

# INVESTMENT<- sophisthse("INVFC_M_CHI_SA")

auto.arima(data$HHI_Q_DIRI[1:29])
auto.arima(data$HHI_Q_DIRI[30:61])
auto.arima(data$HHI_Q_DIRI[62:89])

# y <- sophisthse("HHI_Q_I")
# y1 <- data.frame(y$HHI_Q_DIRI)
# y3 <- y1[62:89,]
# mod_a <- auto.arima(y3)
# summary(mod_a)
# 
# y <- sophisthse("HHI_Q_I")
# y4 <- data.frame(y[30:61,1])
# y4
# mod_a <- auto.arima(y4)

m <- Arima(x=data$HHI_Q_DIRI, order=c(2,1,0))
forecast(m,h=3)

library(hydroGOF)
model <- Arima(x=data$HHI_Q_DIRI[1:86], order=c(1,1,2))
pr <- as.numeric(forecast(model, h=3)$mean)
mse(as.numeric(data$HHI_Q_DIRI[87:89]),pr)


model <- Arima(x=data$HHI_Q_DIRI[1:89], order=c(1,1,1), seasonal=c(0,0,1))
summary(model)

data <- sophisthse("HHI_Q_I")
data <- data[1:89]
data$dum <- replicate(0, n=89)
data$dum[62:69] <- replicate(1, n=8)
model <- Arima(x=data$HHI_Q_DIRI, order=c(1,1,1), xreg=data$dum)
summary(model)

# Arima(x=a$HHI_Q_DIRI, order=c(1,1,1), xreg=a$dum)
# M12 <- arima(x=a$HHI_Q_DIRI, order=c(1,1,1), xreg = a$dummy)

# data <- sophisthse("HHI_Q_I")
# a <- data$HHI_Q_DIRI
# am <- a[1:89] # возьмем наблюдения с 1 по 89
# am$dummy <- replicate(0,n=89)

# data <- sophisthse("HHI_Q_I")
# a <- data[1:89] # возьмем наблюдения с 1 по 89
# a$dummy <- replicate(0,n=89)

set.seed(70)
  y1 <- arima.sim(n=100, list(ar=0.7))
  plot(y1,type="l",axes=T, ylab = "variable Y")
  rect(20,-1000,25,1000,col="#FFCCEE",border="#FFCCEE")
  rect(70,-1000,80,1000,col="#FFCCEE",border="#FFCCEE")
  par(new=TRUE)
  plot(y1,type="l",ylab="")


round(6*25/(25+16), digits=2)
round(8*9/(9+16),digits=2)
round(5*9/(9+36),digits=2)

(6*25+4)/25
(3*16+24)/16
round((4*9+24)/9,digits=2)

library(caret)
library(ggplot2)
df <- diamonds
a <- createDataPartition(y = df$price, p = 0.8, list=FALSE)

library(erer)
setwd("~/Documents/University/master_1_year/coursera_metrics/lab_07")
data <- read.csv("titanic3.csv")
model <- glm(data=data, survived~age+I(age^2)+sex+pclass+sibsp, 
             family=binomial(link="probit"),x=TRUE)
vcov(model)
maBina(model)
confint(model, level= 0.95)

# library(dplyr)
# t <- read.csv("titanic3.csv")
# t <- mutate(t,sex=as.factor(sex),pclass=as.factor(pclass),survived=as.factor(survived))
# m_probit1 <- glm(data=t, survived~age+I(age^2)+sex+pclass+sibsp,
#                  family=binomial(link="probit"),x=TRUE)
# maBina(m_probit1)


library("AER")
data("CollegeDistance")
h <- CollegeDistance

round(20-4*1.645, digits = 2)


4.5+0.05*3.4^2+2*3.4*(-0.23)+1200


library(ggplot2)
data <- read.csv("titanic3.csv")
qplot(data = data, age, geom = "histogram", fill=pclass) + facet_wrap(~pclass)


df <- diamonds
ggplot(data=df, aes(price, carat)) + geom_point(color="lightblue") + facet_wrap(~color)


library("AER")
data("CollegeDistance")
h <- CollegeDistance
set.seed(42)
train_ind <- createDataPartition(h$wage, p=0.9, list=FALSE)
h_train <- h[train_ind,]
h_test <- h[-train_ind,]

model <- lm(data=h_train, wage~region+gender+unemp+ethnicity+education)
summary(model)


model1 <- ivreg(data=h_train, wage~region+gender+unemp+ethnicity+education|region+gender+unemp+ethnicity+distance)
summary(model1)
round(0.626923, digits=2)

h_test$y_hat <- predict(model1, newdata = h_test, type = "response")
round(10.085335, digits=3)
res <- coeftest(model1)

library(lmtest)
m <- mtcars
model <- lm(data=m, mpg~hp+wt+am)
m$hp2 <- m$hp^2
m$wt2 <- m$wt^2

bptest(data=m, mpg~hp+wt+am, varformula= ~ hp+hp2+wt+wt2)

gqtest(data=m, mpg~hp+wt+am, 
       order.by = ~ wt, fraction=0.3)

library(sandwich)
vcovHC(model, type = "HC2")
coeftest(model,vcov. = vcovHC(model,type="HC0"))

set.seed(12)
  y<-arima.sim(model = list (ar = c(0.1, 0.6), ma = -0.3), n=100)
  x1<-rnorm(100, 15, 5)
  x2<-runif(100, 45, 50)
model1 <- lm(data=y, y~x1+x2)
coeftest(model1,vcov. = vcovHAC)
bgtest(model1,order=3)

ggplot(data=model1, aes(lag(resid(model1),1),resid(model1))) + geom_point()


set.seed(123)
y<-arima.sim(model = list(ar = c(0.5, 0.1), ma = c(0.3,0.2)), n = 100)
model3 <- Arima(x=y, order=c(0,1,2))
summary(model3)

model4 <- Arima(x=y, order=c(3,0,3), fixed=c(0, NA, NA, 0, NA, NA, NA))
summary(model4)


4.5+0.05*20^2+2*20*(-0.23)+1200

# library(forecast)
# library(lmtest)
# set.seed(2)
# y <- arima.sim(n=100, list(ar=0.99))
# tsdisplay(y)
# df1 <- as.data.frame(y)
# df1$t <- as.numeric(rownames(df1))
# a <- poly(df1$t, degree = 3, raw=TRUE)
# model <- lm(data=df1, x~a)
# model_2 <- lm(data=df1, x~t+I(t^2)+I(t^3))
# coeftest(model)
# coeftest(model_2)

setwd("~/Desktop")
fl <- read.csv("data.csv")
d <- ts(fl[,2],frequency = 12, start = c(2005,10))
print(ts(d,start=c(2005,10),frequency=12),calendar=F)
arima(d, order=c(1,1,1))

# setwd("~/Documents/University/master_1_year/coursera_metrics/lab_07")
# data <- read.csv("titanic3.csv")
# t <- data 
# m_logit<-glm(data=t,survived~age+I(age^2)+sex+fare+sibsp,family=binomial(link="logit"))
# nd2<-data.frame(age=50,sex="male",sibsp=2,fare=200)
# prognoz <- predict(m_logit, newdata = nd2, type="response", se.fit=TRUE)
# prognoz$fit - 1.96*prognoz$se.fit

# library(ggplot2)
# library(caret)
# h <- diamonds
# set.seed(12345) 
# train_ind <- createDataPartition(h$price, p=0.8, list=FALSE) 
# h_train <- h[train_ind,] 
# h_test <- h[-train_ind,] 
# mod1 <- lm(data=h_train, log(price)~log(carat)+log(depth)+clarity)
# pred_1 <- predict(mod1, h_test)
# h2 <- exp(pred_1)
# x <- (sum(h2-h_test$price)^2)/10^9
# x
# 
# set.seed(12345) 
# df <-diamonds
# train_ind <- createDataPartition(df$price, p=0.8, list=FALSE) 
# df_train <- df[train_ind,] 
# df_test <- df[-train_ind,] 
# model_1=lm(data=df_train,log(price)~log(carat)+log(depth)+log(table)+clarity)
# y=(df_test$price)
# y_hat_1=predict(model_1,df_test)
# y2=exp(y_hat_1)
# sum((y-y2)^2)/(10^9)

