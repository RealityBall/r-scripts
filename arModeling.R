conn = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(conn, "
select 
  date, id, avg(RHfanDuel) as RHfanDuel, avg(LHfanDuel) as LHfanDuel, avg(fanDuel) as fanDuel,
  0.0 as RHfanDuelTrend, 0.0 as LHfanDuelTrend, 0.0 as fanDuelTrend
from 
  hitterFantasyMovingStats 
group by id, date 
order by date;");

fsTimeseries = fetch(rs, n=-1);
dbClearResult(rs)
dbDisconnect(conn)

library(foreach)
library(doParallel)
library(dlm)

fsTrend <- function(index, data, stream) {
  recentModel <- data[(i - lookback):i,stream]
  m1.filteredModel <- dlmFilter(recentModel, m1.dlm)
  #movAr <- ar(m1.filteredModel$f, FALSE, 10)
  movAr <- ar(m1.filteredModel$f, method = "burg")
  fsTrendPredict <- predict(movAr, n.ahead=lookahead)$pred[1:lookahead]
  trendLm <- lm(fsTrendPredict ~ seq(1, lookahead))
  trendLm$coefficients[2]
}

lookback <- 25
lookahead <- 10
m1.dlm <- dlm(FF = 1, V = 0.1, GG = 1, W = 0.01, m0 = 0, C0 = 100) # Started with V=0.8, W=0.1

cl <- makeCluster(1)
registerDoParallel(cl)

names = unique(fsTimeseries$id)
#r <- foreach(k=1:length(names), .combine=rbind) %dopar% {
for(k in 1:length(names)) {
  name <- names[k]
  print(k)
  #if (name == 'cabrm001') {
  playerData <- fsTimeseries[fsTimeseries$id == name,]
  i <- 1
  for (date in playerData$date) {
    if (i > lookback) {
      playerData[i,]$RHfanDuelTrend = try(fsTrend(i, playerData, 3), silent=T)
      playerData[i,]$LHfanDuelTrend = try(fsTrend(i, playerData, 4), silent=T)
      playerData[i,]$fanDuelTrend = try(fsTrend(i, playerData, 5), silent=T)     
    }
    i = i + 1
  }
  fsTimeseries[fsTimeseries$id == name,] = playerData
  #playerData
  #}
}

plotName <- 'cabrm001'
plot(fsTimeseries[fsTimeseries$id == plotName,]$RHfanDuel, ylim=c(-2,5))
lines(fsTimeseries[fsTimeseries$id == plotName,]$RHfanDuel)
lines(fsTimeseries[fsTimeseries$id == plotName,]$RHfanDuelTrend)
plot(fsTimeseries[fsTimeseries$id == plotName,]$LHfanDuel, ylim=c(-2,5))
lines(fsTimeseries[fsTimeseries$id == plotName,]$LHfanDuel)
lines(fsTimeseries[fsTimeseries$id == plotName,]$LHfanDuelTrend)
plot(fsTimeseries[fsTimeseries$id == plotName,]$fanDuel, ylim=c(-2,5))
lines(fsTimeseries[fsTimeseries$id == plotName,]$fanDuel)
lines(fsTimeseries[fsTimeseries$id == plotName,]$fanDuelTrend)

m1.dlm <- dlm(FF = 1, V = 0.1, GG = 1, W = 0.01, m0 = 0, C0 = 100) # Started with V=0.8, W=0.1
recent <- fsTimeseries$RHfanDuel[(nrow(fsTimeseries) - lookback * 1.5):(nrow(fsTimeseries) - 0)]
recentModel <- fsTimeseries$RHfanDuel[(nrow(fsTimeseries) - lookback * 1.5):(nrow(fsTimeseries) - lookback * 0.5)]
m1.filtered <- dlmFilter(recent, m1.dlm)
m1.filteredModel <- dlmFilter(recentModel, m1.dlm)
movAr <- ar(m1.filteredModel$f)
#movAr <- ar(m1.filteredModel$f, FALSE, 10)
#movAr <- ar(recentModel, FALSE, 10)
movLm <- lm(predict(movAr, n.ahead=lookahead)$pred[1:lookahead] ~ seq(1, lookahead))

plot(recent,  
     main = "Trailing 25 game fantasy score", 
     ylab = "Average",
     xlim = c(0, lookback * 1.5))
lines(recent)
lines(m1.filteredModel$f, col=2)
lines(seq(lookback + 1, lookback + lookahead), predict(movAr, n.ahead=lookahead)$pred[1:lookahead], col=3)
legend("topright", inset=.01,
       c("Actual","Kalman Filtered","10 game prediction"), fill=c(1,2,3), horiz=TRUE)