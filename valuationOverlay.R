conn = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(conn, "
select 
  gameId, date, id, startingPitcher, fanduelActual as actual, fanduelBase, 
  pitcherAdj, parkAdj, oddsAdj, matchupAdj, productionRate, overUnder, overUnderML
from 
  fantasyPrediction a
order by 
  substr(a.gameId, 4, 8);");

series = fetch(rs, n=-1);
dbClearResult(rs)
dbDisconnect(conn)

# Compute production probabilities
series$production = rep(1.0, nrow(series));
#series$production[series$actual < series$fanduelBase * 1.25] = 0.0;
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

#series <- series[series$productionRate > 0.0 & series$productionRate < 1.0,];
#plot(density(series[series$pitcherAdj > 0.0,]$productionRate, na.rm=TRUE));
#lines(density(series[series$pitcherAdj < 0.0,]$productionRate, na.rm=TRUE), col=2);
series$oddsBucket = rep(0.0, nrow(series));
series$oddsBucket[series$oddsAdj < -0.3] = -1.0;
series$oddsBucket[series$oddsAdj > 0.3] = 1.0;
#plot(series$productionRate, series$actual);
plot(series[c(3,4,5,6,7,8,9,10)]);

# Train logistic model
seriesTrain <- series[1:(nrow(series) * 0.7),]
seriesTest <- series[(nrow(series) * 0.7):nrow(series),]
#seriesTrain <- series[1:(nrow(playerSeries) * 0.7),]
#seriesTest <- series[(nrow(playerSeries) * 0.7):nrow(playerSeries),]
producing <- glm(as.factor(production) ~ fanDuelTrend + productionRate + oddsAdj + matchupAdj + pitcherAdj + parkAdj + overUnder, data = seriesTrain, family = binomial);
summary(producing);
producePredict <- predict(producing, newdata=seriesTrain, type="response");
seriesTrain$producePredict = producePredict;
plot(jitter(producePredict, 0.2), jitter(seriesTrain$production, 0.2));
ggplot(data=series, aes(x=jitter(oddsAdj, 0.1),y=jitter(actual, 0.1),color=productionRate)) +
  geom_point(size=3) 
ggplot(data=seriesTest, aes(x=jitter(pitcherAdj, 0.1),y=jitter(actual, 0.1),color=productionRate)) +
  geom_point(size=3) 
ggplot(data=seriesTest, aes(x=jitter(matchupAdj, 0.1),y=jitter(actual, 0.1),color=productionRate)) +
  geom_point(size=3) 
ggplot(data=series, aes(x=jitter(productionRate, 0.2),y=jitter(production, 0.2),color=actual)) +
  geom_point(size=1) 
#ggplot(data=seriesTest, aes(x=jitter(producePredict, 0.1),y=jitter(production, 0.1),color=oddsBucket)) +
#  geom_point(size=3) 

# Required Libraries:
#  library(miscTools); library(ggplot2); library(randomForest)
#
# Train random forest
rf <- randomForest(as.factor(production) ~ fanduelBase + productionRate + oddsAdj + matchupAdj + pitcherAdj + parkAdj + overUnder, data=seriesTrain, ntree=40, na.action=na.omit);

# Test random forest
predictions <- predict(rf, newdata=seriesTest, type="response");
plot(jitter(as.numeric(predictions) - 1, 0.2), jitter(seriesTest$production, 0.2));
#updatePrediction(predictions, seriesTest);

available <- !is.na(seriesTest$production - as.numeric(predictions));
(r2 <- rSquared(seriesTest$production[available], (seriesTest$production - as.double(predictions))[available]));
(mse <- mean((seriesTest$production - as.double(predictions))[available])^2);

# Plot Predicted vs Actual of random forest model
p <- ggplot(aes(x=actual, y=pred),
            data=data.frame(actual=seriesTest$production, pred=as.numeric(predictions) - 1.0))
p + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""));

