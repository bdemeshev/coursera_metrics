library("spikeslab")
library("ggplot2")
library("dplyr")
library("reshape2")
library("MCMCpack")
library("quantreg")
library("randomForest")
library("rattle")
library("caret")
library("rpart")

f <- read.table("flats_moscow.txt", header=TRUE, sep="\t", dec=".")
glimpse(f)

model_q01 <- rq(data=f, price~totsp, tau=c(0.1,0.5,0.9))
summary(model_q01)

base <- qplot(data=f, totsp, price)
base

base_q <- base + stat_smooth(method="rq", tau=0.1, se=FALSE) + 
  stat_smooth(method="rq", tau=0.9, se=FALSE)

base_q + aes(colour=factor(brick))


in_sample <- createDataPartition(f$price, p=0.75, list=FALSE)
head(in_sample)

f_train <- f[in_sample,]
f_test <- f[-in_sample,]

model_lm <- lm(data=f_train, price~totsp+kitsp+livesp+brick)
model_rf <- randomForest(data=f_train, price~totsp+kitsp+livesp+brick)

y <- f_test$price
yhat_lm <- predict(model_lm, f_test)
yhat_rf <- predict(model_rf, f_test)

sum( (y - yhat_lm)^2   )
sum( (y - yhat_rf)^2   )

# Bayesian approach

bad <- data.frame(y=c(0,0,1), x=c(1,2,3))
bad

model_logit <- glm(data=bad, y~x, family=binomial(link="logit"))
summary(model_logit)

# prior: beta ~ N(0, 50^2)
model_mcmc_logit <- MCMClogit(data=bad, y~x, b0=0, B0=1/50^2 )
summary(model_mcmc_logit)

model_mcmc_logit <- MCMClogit(data=bad, y~x, b0=0, B0=1/10^2 )
summary(model_mcmc_logit)

# spike and slab regression
h <- mutate(cars, speed=1.61*speed, dist=0.3*dist)

h$junk <- rnorm(nrow(h))
h <- mutate(h, speed2=speed^2)

model_lm <- lm(data=h, dist~speed+junk)
summary(model_lm)

model_ss <- spikeslab(data=h, dist~speed+junk, n.iter2=4000)
print(model_ss)

model_ss$summary

included_regressors <- melt(model_ss$model)
included_regressors

head(included_regressors)
sum( included_regressors$value==1 )/4000
sum( included_regressors$value==2 )/4000
nrow(h)
