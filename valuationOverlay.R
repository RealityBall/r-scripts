conn = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(conn, "
select 
  a.gameId, a.id, sum(fanDuel) as actual, fanduelBase, pitcherAdj, parkAdj, baTrendAdj, oddsAdj, matchupAdj, productionRate
from 
  fantasyPrediction a, hitterFantasyStats b
where
  a.gameId = b.gameId and 
  a.id = b.id 
group by 
  a.gameId, a.id
order by 
    substr(a.gameId, 4, 8);");

series = fetch(rs, n=-1);
dbClearResult(rs)
dbDisconnect(conn)

# Compute production probabilities
series$production = rep(1.0, nrow(series));
#series$production[series$actual < 0.5 * series$fanduelBase] = 0.0;
series$production[series$actual < 2.0] = 0.0;
series <- series[series$productionRate > 0.1 & series$productionRate < 0.9,];
plot(series$productionRate, series$actual);
plot(series[c(3,4,5,6,7,8,9,10)]);

# Train logistic model
seriesTrain <- series[1:(nrow(series) * 0.7),]
seriesTest <- series[(nrow(series) * 0.7):nrow(series),]
#producing <- glm(as.factor(production) ~ productionRate + oddsAdj + matchupAdj, data = seriesTrain, family = binomial);
#producing <- glm(as.factor(production) ~  oddsAdj + matchupAdj + productionRate, data = seriesTrain, family = binomial);
#producing <- glm(as.factor(production) ~  actual, data = seriesTrain, family = binomial);
#producing <- glm(as.factor(production) ~ fanduelBase + pitcherAdj + parkAdj + oddsAdj + matchupAdj + productionRate, data = seriesTrain, family = binomial);
producing <- glm(as.factor(production) ~ oddsAdj + productionRate, data = seriesTrain, family = binomial);
summary(producing);
producePredict <- predict(producing, newdata=seriesTest, type="response");
plot(jitter(producePredict, 0.2), jitter(seriesTest$production, 0.2));

# Required Libraries:
#  library(miscTools)
#  library(ggplot2)
#  library(randomForest)
#
# Train random forest
rf <- randomForest(as.factor(production) ~ fanduelBase + oddsAdj + matchupAdj + productionRate, data=seriesTrain, ntree=40, na.action=na.omit);

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

