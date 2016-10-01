# графики к лекции 5 по гетероск

x <- rnorm(100,mean=3)
eps <- rnorm(100,mean=1)*x
y <- 3+2*x+eps

model <- lm(y~x)
library("ggplot2")

r <- resid(model)
qplot(x,r^2,xlab = "Регрессор, x", ylab="Квадрат остатка",
      main = "Гетероскедастичность") + theme_bw()

ggsave("graph_02_getero.png",scale=1)
eps2 <- rnorm(100,mean=1)
y2 <- 3+2*x+eps2

model2 <- lm(y2~x)
library("ggplot2")

r2 <- resid(model2)
qplot(x,r2^2,xlab = "Регрессор, x", ylab="Квадрат остатка", 
      main = "Гомоскедастичность") + theme_bw()
?ggsave
ggsave("graph_01_gomo.png",scale=1)




x <- seq(0,15,length=100)
ychi <- dchisq(x,df=6)
qplot(x,ychi,xlab="",ylab="",geom="line") + theme_bw()

xf <- seq(0,5,length=100)
yf <- df(xf,df1=5,df2=2)
qplot(x,yf,xlab="",ylab="",geom="line") + theme_bw()




