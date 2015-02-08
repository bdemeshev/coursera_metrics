
# P(X<4)? X~N(2,3^2) 

pnorm
pexp
pf
pt

pnorm(4,mean = 2, sd = 3)
# P(X<4) = 0.747

# P(3 < X < 5)? X~exp(4)

pexp(5,rate = 4) - pexp(3,rate=4)

# X~N(5,7^2), a? P(X<a)=0.95

qnorm
qexp
qt
qf
qchisq

qnorm(0.95,mean=5,sd=7)

# X ~ chi2 (df=2), a? P(X>a)=0.025

# P(X<a) = 0.975
qchisq(0.975,df=2)

# ф плотности
dnorm
dchisq
df
dt


# chi^2, df=2, df=3

library("ggplot2")
x <- seq(0,10,length=100)
head(x)
tail(x)

y <- dchisq(x,df=2)
qplot(x,y)
qplot(x,y,geom="line")

y <- dchisq(x,df=3)
qplot(x,y,geom="line")

