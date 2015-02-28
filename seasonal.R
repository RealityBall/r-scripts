

mydb = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

#rs = dbSendQuery(mydb, "select battingAverageMov from hitterMovingStats where id = 'kempm001' order by date;");
rs = dbSendQuery(mydb, "select battingAverageMov from hitterMovingStats where date like '2014%' and id = 'kempm001' order by date;");

series = fetch(rs, n=-1);
ts <- ts(series$battingAverageMov);
plot(ts);

# Seasonal decomposition
fit <- stl(ts, s.window="period")
plot(fit)

# additional plots
monthplot(ts)
library(forecast)
seasonplot(ts)

# simple exponential - models level
fit <- HoltWinters(ts, beta=FALSE, gamma=FALSE, alpha=0.1);
plot(fit);
# double exponential - models level and trend
fit <- HoltWinters(ts, gamma=FALSE, alpha=0.05);
plot(fit);
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(ts)

# predictive accuracy
library(forecast)
accuracy(fit)
# predict next three future values
library(forecast)
forecast(fit, 20)
plot(forecast(fit, 20))

fit2 <- forecast.HoltWinters(fit, h=10)
plot.forecast(fit2)


# fit an ARIMA model of order P, D, Q
fit <- arima(ts) #, order=c(p, d, q))
# predictive accuracy
library(forecast)
accuracy(fit)
             
# predict next 5 observations
library(forecast)
forecast(fit, 20)
plot(forecast(fit, 20))