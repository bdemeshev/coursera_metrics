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

t <- mutate(t,sex=as.factor(sex),pclass=as.factor(pclass),survived=as.factor(survived))
summary(t)

mosaic(data=t,~sex+pclass+survived,shade=TRUE)
qplot(data=t,x=survived,y=age,geom="violin")
qplot(data=t,x=survived,y=age,geom="boxplot")

qplot(data=t,x=age,y=..count..,fill=survived,geom="density",position="stack")
qplot(data=t,x=age,y=..count..,fill=survived,geom="density",position="fill")

# Оценивание моделей

m_logit <- glm(data=t, survived~sex+age+pclass+fare,
               family=binomial(link="logit"),x=TRUE)
m_probit <- glm(data=t, survived~sex+age+pclass+fare,
               family=binomial(link="probit"),x=TRUE)
summary(m_logit)
summary(m_probit)

# ков матрица
vcov(m_logit)

newdata <- data.frame(age=seq(from=5,to=100,length=100),
                      sex="male",pclass="2nd",fare=100)
head(newdata)

pr_logit <- predict(m_logit,newdata,se=TRUE)
newdata_pr <- cbind(newdata,pr_logit)
head(newdata_pr)

newdata_pr <- mutate(newdata_pr,prob=plogis(fit),
                     left_ci=plogis(fit-1.96*se.fit),
                     right_ci=plogis(fit+1.96*se.fit))
head(newdata_pr)

qplot(data=newdata_pr,x=age,y=prob,geom="line") +
  geom_ribbon(aes(ymin=left_ci,ymax=right_ci),alpha=0.2)



t2 <- select(t,sex,age,pclass,survived,fare) %>% na.omit()
m_logit2 <- glm(data=t2, survived~sex+age,
                family=binomial(link="logit"),x=TRUE)
lrtest(m_logit,m_logit2)


maBina(m_logit)
maBina(m_logit,x.mean = FALSE)

# ols
m_ols <- lm(data=t, as.numeric(survived)~sex+age+pclass+fare)
summary(m_ols)

pr_ols <- predict(m_ols,newdata)
head(pr_ols)

pr_t <- predict(m_logit,t,se=TRUE)
t <- cbind(t,pr_t)
t <- mutate(t,prob=plogis(fit))
select(t,age,survived,prob)

roc.data <- roc(t$prob,t$survived)
str(roc.data)

qplot(x=roc.data$cutoffs,y=roc.data$tpr,geom="line")
qplot(x=roc.data$cutoffs,y=roc.data$fpr,geom="line")

qplot(x=roc.data$fpr,y=roc.data$tpr,geom="line")


