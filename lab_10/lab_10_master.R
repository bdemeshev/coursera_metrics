library("spikeslab")
library("spikeSlabGAM")
library("ggplot2")
library("dplyr")
library("reshape2")
# library("BayesLogit")
library("MCMCpack")
library("quantreg")

h <- cars

h <- mutate(h, speed2=speed^2, junk=rnorm(nrow(cars)), junk2 = junk^2)
glimpse(h)

model_lm <- lm(data=h, dist~speed+junk)
model_ssg <- spikeSlabGAM(data=h, dist~lin(speed)+lin(junk), family = "gaussian")
model_ssg_f <- spikeSlabGAM(data=h, dist~speed + junk)
model_ss <- spikeslab(data=h, dist~speed+junk)

summary(model_lm)
summary(model_ssg)
summary(model_ssg_f)
print(model_ss)
# look at bma.scale (posterior mean)

regressors_included <- melt(model_ss$model)
regressors_included

table(regressors_included$value)/500

nw <- data.frame(speed=c(40,20),junk=c(1,-0.5))
nw <- mutate(nw, speed2=speed^2, junk2 = junk^2)

predict(model_lm, nw)
predict(model_ssg, nw)
predict(model_ssg_f, nw)
predict(model_ss, nw)


h <- read.table("flats_moscow.txt", header = TRUE, sep="\t")


model <- rq(data=h, price~totsp, tau=c(0.1,0.5,0.9))
base <- qplot(data=h, y=price, x=totsp,  colour=factor(brick))
base
base + stat_smooth(se=FALSE, method = "rq", tau=0.1) + 
  stat_smooth(se=FALSE, method = "rq", tau=0.9) 
 