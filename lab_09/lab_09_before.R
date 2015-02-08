library("dplyr")
library("caret")
library("AER")
library("ggplot2")
library("sandwich")
library("ivpack")

# четкое разграничение целей

# задача прогнозирования

h <- read.csv("flats_moscow.txt", header = TRUE, sep="\t", dec=".")
glimpse(h)

h2 <- mutate(h, logprice=log(price), logtotsp=log(totsp), 
             logkitsp=log(kitsp), loglivesp=log(livesp))

in_train <- createDataPartition(y = h2$logprice, p = 0.75, list=FALSE)
h2_train <- h2[in_train,]
h2_test <- h2[-in_train,]

model_1 <- lm(data=h2_train, logprice~logkitsp+logtotsp+loglivesp)
model_2 <- lm(data=h2_train, logprice~logtotsp)

pred_1 <- predict(model_1, h2_test)
pred_2 <- predict(model_2, h2_test)

sum( (pred_1 - h2_test$logprice)^2)
sum( (pred_2 - h2_test$logprice)^2)

# оценивание заданной формы модели с эндогенностью

help(ivreg)

## data
data("CigarettesSW", package = "AER")
help("CigarettesSW")

h <- CigarettesSW
glimpse(h)

qplot(data=h, price, packs)

h2 <- filter(h, year=="1995")
h2 <- mutate(h2, rprice=price/cpi, rincome=income/cpi/population, 
             tdiff=(taxs-tax)/cpi)
qplot(data=h2, price, packs)

glimpse(h2)


# lm

model_0 <- lm(data=h2, log(packs)~log(rprice))
summary(model_0)

# two stages by hands
st_1 <- lm(data=h2, log(rprice)~ tdiff)
h2$logprice_hat <- fitted(st_1)

st_2 <- lm(data=h2, log(packs)~logprice_hat)
coeftest(st_2)

# two stages automatically
model_iv <- ivreg(data=h2, log(packs)~log(rprice) | tdiff )
coeftest(model_iv)
mtable(model_0, model_iv, st_2)

coeftest(model_iv, vcov=vcovHC)


## model 
iv_model_2 <- ivreg(data=h2, 
            log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff )
coeftest(iv_model_2, vcov = vcovHC)

iv_model_3 <- ivreg(data=h2, 
                    log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi) )
coeftest(iv_model_3, vcov = vcovHC)


