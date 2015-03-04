library(randomForest)
library(miscTools)
library(ggplot2)

conn = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(conn, "
select 
  a.gameId, a.id, sum(fanDuel) as actual, fanduelBase, pitcherAdj, parkAdj, baTrendAdj, oddsAdj, matchupAdj, productionRate
from 
  fantasyPrediction a, hitterFantasyStats b
where
  a.gameId = b.gameId and a.id = b.id
group by 
  a.gameId, a.id
order by 
    substr(a.gameId, 4, 8);");

series = fetch(rs, n=-1);
dbClearResult(rs)
dbDisconnect(conn)

# Function to update database with new fantasy score predictions
updatePrediction <- function(predictions, series) {
  conn = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');
  for (i in 1:length(predictions)) {
    if (!is.na(predictions[i])) {
      dbSendQuery(conn, paste("update fantasyPrediction set eFanduel = ", predictions[i], 
                              " where gameId = '", series$gameId[i], "' and id='", series$id[i], "'", sep=""));
    }
  }
  dbDisconnect(conn)  
} 

# Train multiple regressions
fit <- lm(actual ~ fanduelBase + pitcherAdj + parkAdj + baTrendAdj + oddsAdj + matchupAdj + productionRate, data=series);
summary(fit);
fit <- lm(actual ~ fanduelBase + oddsAdj + pitcherAdj + matchupAdj + productionRate, data=series);
summary(fit);

# Train random forest
seriesTrain <- series[1:(nrow(series) * 0.7),]
seriesTest <- series[(nrow(series) * 0.7):nrow(series),]
#rf <- randomForest(actual ~ fanduelBase + pitcherAdj + parkAdj + baTrendAdj + oddsAdj + matchupAdj, data=seriesTrain, ntree=20, na.action=na.omit)
rf <- randomForest(actual ~ fanduelBase + oddsAdj + matchupAdj, data=seriesTrain, ntree=40, na.action=na.omit);

# Test random forest
predictions <- predict(rf, seriesTest);
seriesTest[predictions[seriesTest$actual > 15,],]
#updatePrediction(predictions, seriesTest);

available <- !is.na(seriesTest$actual - predictions);
(r2 <- rSquared(seriesTest$actual[available], (seriesTest$actual - predictions)[available]));
(mse <- mean((seriesTest$actual - predictions)[available])^2);

# Plot Predicted vs Actual of random forest model
p <- ggplot(aes(x=actual, y=pred),
            data=data.frame(actual=seriesTest$actual, pred=predictions))
p + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""));

# Actual FS production vs Streak of under / over performance 
plot(jitter(series$revert), jitter(series$actual));
