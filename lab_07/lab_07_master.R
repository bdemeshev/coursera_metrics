# lab_07

library("dplyr")
library("erer")
library("vcd")
library("ggplot2")
library("reshape2")
library("AUC")

options(stringsAsFactors=FALSE)

t <- read.csv("titanic3.csv")
# http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html
glimpse(t)

t <- mutate_each(t,"as.factor",survived,pclass,sex,embarked)

# проверка загрузки
glimpse(t)
head(t)
tail(t)

# графики!!!
mosaic(~sex+pclass+survived,data=t,shade = TRUE)

qplot(data=t, x=survived, y=age, geom="violin") 

qplot(data=t, x=survived, y=age, geom="boxplot") 

qplot(data=t, x=age, y=..count.., fill=survived, 
      geom="density", position="stack")

qplot(data=t, x=age, y=..count.., fill=survived, 
      geom="density", position="fill")


# модели

m_logit <- glm(data=t,
               survived~age+fare+sex+pclass,
               family=binomial(link="logit"),x=TRUE)
m_probit <- glm(data=t,
                survived~age+fare+sex+pclass,
                family=binomial(link="probit"),x=TRUE)
summary(m_logit)
summary(m_probit)

# ковариационная матрица коэффициентов
vcov(m_logit)

# прогнозируем вероятности


# вставка про OLS


# сравнение вложенных моделей:
m_logit2 <- glm(data=t,
                survived~age+sex,
                family=binomial(link="logit"),x=TRUE)
summary(m_logit2)

lrtest(m_logit,m_logit2)

t2 <- select(t,survived,age,sex,fare,pclass) %>% na.omit()
m_logit2 <- glm(data=t2,
                survived~age+sex,
                family=binomial(link="logit"),x=TRUE)

summary(t$fare)
summary(t$sex)
newdata <- data_frame(age=seq(from=5, to=100,length=100),fare=100,
                      sex="male",pclass="2nd")
head(newdata)
pred_logit <- predict(m_logit,newdata, se=TRUE)
newdata_pr <- cbind(newdata,pred_logit)
head(newdata_pr)

newdata_pr <- select(newdata_pr,-residual.scale) %>% 
  mutate(prob=plogis(fit),
                  ci_left=plogis(fit-1.96*se.fit),
                  ci_rigth=plogis(fit+1.96*se.fit))
head(newdata_pr)

qplot(data=newdata_pr, x = age, y = prob, geom="line") + 
  geom_ribbon(aes(ymin = ci_left, ymax = ci_rigth), alpha = 0.2) 
  #geom_line()

maBina(m_logit)
maBina(m_logit,x.mean = FALSE)
qplot(data=t,age)

# ols

m_ols <- lm(data=t,as.numeric(survived)~age+fare+sex+pclass)
summary(m_ols)

pred_ols <- predict(m_ols,newdata)
pred_ols

# roc
t_pr <- cbind(t2,predict(m_logit,t2,se=TRUE))
t_pr <- mutate(t_pr,prob=plogis(fit))
roc.data <- roc(t_pr$prob,t_pr$survived)
glimpse(roc.data)
attr(roc.data,"class") <- "list"
melt(roc.data)
library(reshape2)
cbind(NULL,roc.data)

qplot(roc.data$cutoffs,roc.data$fpr,geom="line")
qplot(roc.data$cutoffs,roc.data$tpr,geom="line")

attr(roc.data,"class") <- "list"
roc.data <- as.data.frame(roc.data)
qplot(data=roc.data,x=cutoffs,y=tpr) + geom_line(aes(y=fpr),col="red")
