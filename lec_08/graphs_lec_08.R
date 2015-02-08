library(forecast)

n <- 200

y <- arima.sim(n=n, list(order=c(0,0,0)) )
?arima.sim
tsdisplay(y)
plot(y)
Acf(y)
Pacf(y)


y <- arima.sim(n=n, list(order=c(0,1,0)) )
plot(y)
Acf(y)
Pacf(y)

y <- 0.02*(1:n)+arima.sim(n=n, list(order=c(0,0,0)) )
plot(y)
Acf(y)
Pacf(y)

y <- arima.sim(n=n, list(ar=0.7) )
plot(y)
Acf(y)
Pacf(y)

y <- arima.sim(n=n, list(ar=c(0.9,-0.5)) )
plot(y)
Acf(y)
Pacf(y)

y <- arima.sim(n=n, list(ma=0.7) )
plot(y)
Acf(y)
Pacf(y)

y <- arima.sim(n=n, list(ma=c(0.9,+0.5)) )
plot(y)
Acf(y)
Pacf(y)

y <- arima.sim(n=n, list(ar=0.7, ma=0.5) )
plot(y)
Acf(y)
Pacf(y)

