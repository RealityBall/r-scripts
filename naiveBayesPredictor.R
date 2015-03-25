bayesPredict <- function(odds, pitcher, matchup, rate, park) {
  start <- 1.0;
  
  if (odds > 0.25) start <- start * 0.5
  else if (odds > 0.1) start <- start * 0.45
  else if (odds < -0.25) start <- start * 0.35
  else if (odds < -0.1) start <- start * 0.375
  else start <- start * 0.42
  
  if (rate > 0.75) start <- start * 0.46
  else if (rate > 0.6) start <- start * 0.45
  else if (rate < 0.4) start <- start * 0.39
  else start <- start * 0.42
  
  if (matchup > 0.2) start <- start * 0.46
  else if (matchup > 0.1) start <- start * 0.45
  else if (matchup < -0.2) start <- start * 0.34
  else if (matchup < -0.1) start <- start * 0.37
  else start <- start * 0.415
  
  if (pitcher > 0.0) start <- start * 0.46
  else if (pitcher > -0.1) start <- start * 0.46
  else start <- start * 0.37

  if (park > 0.0) start <- start * 0.46
  else start <- start * 0.41
  
  start
}

series$bayesPredicted <- as.array(apply(series[,c('oddsAdj', 'pitcherAdj', 'matchupAdj', 'productionRate', 'parkAdj')], 1, 
                                  function(x) bayesPredict(x[1], x[2], x[3], x[4], x[5])));
plot(jitter(series$bayesPredicted, 0.1), series$actual);
summary(series[series$bayesPredicted > summary(series$bayesPredicted)[3],]$actual);
sd(series[series$bayesPredicted > summary(series$bayesPredicted)[3],]$actual);
summary(series[series$bayesPredicted < summary(series$bayesPredicted)[3],]$actual);
sd(series[series$bayesPredicted < summary(series$bayesPredicted)[3],]$actual);

plot(density(series[series$bayesPredicted < summary(series$bayesPredicted)[3],]$actual, na.rm=TRUE));
lines(density(series[series$bayesPredicted > summary(series$bayesPredicted)[3],]$actual, na.rm=TRUE), col=2);

