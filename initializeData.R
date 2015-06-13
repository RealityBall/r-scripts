conn = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(conn, "
                 select 
                 gameId, a.date, id, mlbPos, fanDuelSalary, startingPitcher, fanduelActual as actual, fanduelBase, 
                 pitcherAdj, parkAdj, oddsAdj, matchupAdj, productionRate, overUnder, overUnderML
                 from 
                 fantasyPrediction a, rotoGuruDailySalary b, rotoGuruPlayers c, idMapping d
                 where
                 a.id = d.retroId and d.mlbId = c.mlbComId and c.rotoGuruId = b.rotoGuruId and 
                 b.date = a.date
                 order by 
                 substr(a.gameId, 4, 8);");

series = fetch(rs, n=-1);
dbClearResult(rs)
dbDisconnect(conn)

# Compute production probabilities
series$production = rep(1.0, nrow(series));
series$production[series$actual <= 1.0] = 0.0;
series <- merge(series, fsTimeseries, by=c("date", "id"))
series$RHfanDuelTrend <- as.numeric(series$RHfanDuelTrend)
series$LHfanDuelTrend <- as.numeric(series$LHfanDuelTrend)
series$fanDuelTrend <- as.numeric(series$fanDuelTrend)
for(i in 1:nrow(series)) {
  if (series$startingPitcher[i] == 'R') series$fanDuelTrend[i] <- as.numeric(series$RHfanDuelTrend[i])
  else series$fanDuelTrend[i] <- as.numeric(series$LHfanDuelTrend[i])
}
series <- series[, !(colnames(series) %in% c("RHfanDuel", "LHfanDuel", "fanDuel", "RHfanDuelTrend", "LHfanDuelTrend"))]

# Outlier truncation
series$pitcherAdj[series$pitcherAdj < -5.0] = -5.0
series$pitcherAdj[series$pitcherAdj > 5.0] = 5.0
series$fanDuelTrend[series$fanDuelTrend < -0.1] = -0.1
series$fanDuelTrend[series$fanDuelTrend > 0.1] = 0.1
series$oddsAdj[series$oddsAdj < -0.5] = -0.5
series$oddsAdj[series$oddsAdj > 0.5] = 0.5

# Summary plots
par(mfrow = c(3, 3))
hist(series$fanduelBase)
hist(series$pitcherAdj)
hist(series$parkAdj)
hist(series$oddsAdj)
hist(series$matchupAdj)
hist(series$productionRate)
hist(series$overUnder)
hist(series$overUnderML)
hist(series$fanDuelTrend)