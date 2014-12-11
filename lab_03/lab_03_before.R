library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")

h <- diamonds
glimpse(h)
help(diamonds)

qplot(data=h, carat, price)
bg <- qplot(data=h, log(carat), log(price))
bg + geom_hex()

f <- read.csv("flats_moscow.txt", sep="\t", header=TRUE, dec=".")
glimpse(f)
qplot(data=f, totsp, price)
str(f)
qplot(data=f, log(totsp), log(price))

mosaic(data=f, ~walk+brick + floor, shade=TRUE)

f <- mutate_each(f, "factor", walk, brick, floor, code)
glimpse(f)

qplot(data=f, log(price))
qplot(data=f, log(price), fill=brick)
qplot(data=f, log(price), fill=brick, position="dodge")
qplot(data=f, log(price), fill=brick, geom="density")
g2 <- qplot(data=f, log(price), fill=brick, geom="density", alpha=0.5)

g2 + facet_grid(walk~floor)
g2 + facet_grid(~floor)

model_0 <- lm(data=f, log(price)~log(totsp))
model_1 <- lm(data=f, log(price)~log(totsp)+brick)
model_2 <- lm(data=f, log(price)~log(totsp)+brick+brick:log(totsp))

summary(model_0)
mtable(model_2)

model_2b <- lm(data=f, log(price)~brick*log(totsp))
mtable(model_2, model_2b)

sjp.lm(model_2)

nw <- data.frame(totsp=c(60,60), brick=factor(c(1,0)))
nw

predict(model_2, newdata=nw)
exp(predict(model_2, newdata=nw))

predict(model_2, newdata=nw, interval="confidence")
exp(predict(model_2, newdata=nw, interval="confidence"))

predict(model_2, newdata=nw, interval="prediction")
exp(predict(model_2, newdata=nw, interval="prediction"))

waldtest(model_0, model_1) # H_0: true model_0 is rejected
waldtest(model_1, model_2) # H_0: true model_1 is rejected
waldtest(model_0, model_2) # H_0: true model_0 is rejected

gg0 <- qplot(data=f, log(totsp), log(price))
gg0 + stat_smooth(method="lm")
gg0 + stat_smooth(method="lm") + facet_grid(~walk)
gg0 + aes(col=brick) + stat_smooth(method="lm") + facet_grid(~walk)

f$nonbrick <- memisc::recode(f$brick, 1 <- 0, 0 <- 1)
glimpse(f)
model_wrong <- lm(data=f, log(price)~log(totsp)+brick+nonbrick)
summary(model_wrong)


mtable(model_0, model_1, model_2)


resettest(model_2)



