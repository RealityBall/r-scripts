
# Pr(actual > 0.0 | oddsAdj > 0.0) = 0.3
# Pr(actual > 0.0 | productionRate > 0.6) = 0.3
# Pr(actual > 0.0 | matchupAdj > 0.0) = 0.1

breakpoints <- function(bpSeries, breakPoint) {
  hiRate <- bpSeries[bpSeries$xSeries >= breakPoint,]
  loRate <- bpSeries[bpSeries$xSeries < breakPoint,]
  c(length(hiRate$ySeries[hiRate$ySeries == 1]) / length(hiRate$ySeries), length(loRate$ySeries[loRate$ySeries == 1]) / length(loRate$ySeries), length(hiRate$ySeries))
}

zeroPoints <- function(xSeries, ySeries, name, lower, upper) {
  origSeries <- data.frame(cbind(xSeries, ySeries));
  ratioSeries <- apply(as.array(seq(lower, upper, 0.01)), 1, function(x) breakpoints(origSeries, x));
  factorTs <<- data.frame(index=seq(lower, upper, 0.01), hiRatio=ratioSeries[1,], loRatio=ratioSeries[2,], universeCount=ratioSeries[3,]);
  
  p1 <- 
    ggplot(factorTs, aes(factorTs$index)) +
    geom_line(aes(y=factorTs$hiRatio, color="hiRatio")) +
    geom_line(aes(y=factorTs$loRatio, color="loRatio")) +
    ggtitle(name)
  p2 <- 
    ggplot(factorTs, aes(factorTs$index)) + 
    geom_line(aes(y=factorTs$universeCount, colour="hiUniverse")) +
    geom_line(aes(y=-factorTs$universeCount + factorTs$universeCount[1], colour="loUniverse")) +
    ggtitle("Universe Count")
  multiplot(p1, p2, cols=1)
  c(ratioSeries[1,round(ncol(ratioSeries) / 2)], ratioSeries[2,round(ncol(ratioSeries) / 2)]);
}

oddsData <- zeroPoints(series$oddsAdj, series$production, "Odds Adj", -1.0, 1.0);
pitcherData <- zeroPoints(series$pitcherAdj, series$production, "Pitcher Adj", -1.0, 1.0);
matchupData <- zeroPoints(series$matchupAdj, series$production, "Matchup Adj", -1.0, 1.0);
rateData <- zeroPoints(series$productionRate, series$production, "Rate Adj", 0.0, 1.0);
parkData <- zeroPoints(series$parkAdj, series$production, "Park Adj", -1.0, 1.0);

bayesPredict <- function(odds, pitcher, matchup, rate, park) {
  start <- 1.0;
  
  if (odds > 0.0) start <- start * oddsData[1]
  else start <- start * oddsData[2]
  
  if (pitcher > 0.0) start <- start * pitcherData[1]
  else start <- start * pitcherData[2]
  
  if (matchup > 0.0) start <- start * matchupData[1]
  else start <- start * matchupData[2]
  
  if (rate > 0.5) start <- start * rateData[1]
  else start <- start * rateData[2]
  
  if (park > 0.0) start <- start * park[1]
  else start <- start * park[2]
  
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



