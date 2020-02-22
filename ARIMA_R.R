#ARIMA

Nile
plot(Nile)
Nile.diff2 = diff(Nile, differences = 2)
plot(Nile.diff2)

acf(Nile.diff2, lag.max = 20)
acf(Nile.diff2, lag.max = 20, plot = FALSE)

pacf(Nile.diff2, lag.max = 20)
pacf(Nile.diff2, lag.max = 20, plot = FALSE)

#auto.arima()

library(forecast)
auto.arima(Nile)

Nile.arima <- arima(Nile, order = c(1,1,1))
Nile.arima

Nile.forecast <- forecast(Nile.arima, h = 50) #h means year
Nile.forecast
plot(Nile.forecast)
