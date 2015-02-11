library("spikeslab")
# library("spikeSlabGAM")
library("ggplot2")
library("dplyr")
library("reshape2")
# library("BayesLogit")
library("MCMCpack")
library("quantreg")
library("randomForest")
library("rattle")
library("caret")
library("rpart")

# quantile regression

f <- read.table("flats_moscow.txt", header = TRUE, sep="\t")


model <- rq(data=f, price~totsp, tau=c(0.1,0.5,0.9))
summary(model)

base <- qplot(data=f, y=price, x=totsp) 
base
base_q <- base + stat_smooth(se=FALSE, method = "rq", tau=0.1) + 
  stat_smooth(se=FALSE, method = "rq", tau=0.9) 
base_q 

base_q + aes(colour=factor(brick))

# random forest

f <- read.table("flats_moscow.txt", header = TRUE, sep="\t")

in_sample <- createDataPartition(f$price, p=0.75, list=FALSE)
f_train <- f[in_sample,]
f_test <- f[-in_sample,]

model_lm <- lm(data=f_train, price~totsp+livesp+kitsp+brick)
model_rf <- randomForest(data=f_train, price~totsp+livesp+kitsp+brick)

yhat_lm <- predict(model_lm, f_test)
yhat_rf <- predict(model_rf, f_test)

y <- f_test$price

sum( (y-yhat_lm)^2 )
sum( (y-yhat_rf)^2 )

# draw tree

t <- data.frame(y=c(1,1,2,10,20), x=c(1,0,0,0,1), z=c(-2,3,-4,9,9))
t

fit <- rpart(data=t, y~x+z, control = rpart.control(minsplit = 2))

fancyRpartPlot(fit)

# MCMC 

# mcmc обычная регрессия

h <- cars
h <- mutate(h, speed_rus = speed * 1.6)
model_lm <- lm(data=h, dist~speed+speed_rus)
summary(model_lm)

# фигушки!!!!
model_b <- MCMCregress(data=h, dist~speed+speed_rus)

# logit

x <- c(1,2,3)
y <- c(0,0,1)

g <- data.frame(x=x, y=y)
model_logit <- glm(data=g, y~x, family=binomial(link="logit"))
summary(model_logit)

model_b <- MCMClogit(data=g, y~x, B0=10^(-2))
# prior sd = 10
summary(model_b)

model_b <- MCMClogit(data=g, y~x, B0=10^(-4))
# prior sd = 100
summary(model_b)

# spike and slab

h <- mutate(cars, speed=1.61*speed, dist=0.3*dist)



h <- mutate(h, speed2=speed^2, junk=rnorm(nrow(cars)), junk2 = junk^2)
glimpse(h)

model_lm <- lm(data=h, dist~speed+junk)
#model_ssg <- spikeSlabGAM(data=h, dist~lin(speed)+lin(junk), family = "gaussian")
#model_ssg_f <- spikeSlabGAM(data=h, dist~speed + junk)
model_ss <- spikeslab(data=h, dist~speed+junk,n.iter2 = 2000)

summary(model_lm)
#summary(model_ssg)
#summary(model_ssg_f)
print(model_ss)
# look at bma.scale (posterior mean)

regressors_included <- melt(model_ss$model)
regressors_included

table(regressors_included$value)/2000

nw <- data.frame(speed=c(40,20),junk=c(1,-0.5))
nw <- mutate(nw, speed2=speed^2, junk2 = junk^2)

predict(model_lm, nw)
predict(model_ss, nw)

# для лекций

n_steps <- 10^4
model_ss <- spikeslab(data=h, dist~speed+speed2,n.iter2 = n_steps)

regressors_included <- melt(model_ss$model)
table(regressors_included$value)/n_steps
