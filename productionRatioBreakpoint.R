
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
rateData <- zeroPoints(series$productionRate, series$production, "Rate Adj", 0.5, 1.0);
parkData <- zeroPoints(series$parkAdj, series$production, "Park Adj", 0.0, 1.0);
ouData <- zeroPoints(series$overUnder, series$production, "OverUnder", 6, 10);

autoVsManu <- FALSE

bayesPredict <- function(odds, pitcher, matchup, rate, park, overUnder) {
  start <- 1.0;
  
  if (autoVsManu) {
    if (odds > 0.0) start <- start * oddsData[1]
    else start <- start * oddsData[2]    
  } else {
    if (odds > 0.1) start <- start * 0.6
    else if (odds < -0.1) start <- start * 0.5
    else if (odds > 0.0) start <- start * oddsData[1]
    else start <- start * oddsData[2]    
  }
  
  if (autoVsManu) {
    if (matchup > 0.0) start <- start * matchupData[1]
    else start <- start * matchupData[2]    
  } else {
    if (matchup > 0.1) start <- start * 0.575
    else if (matchup < -0.1) start <- start * 0.475
    else if (matchup > 0.0) start <- start * matchupData[1]
    else start <- start * matchupData[2]    
  }

  if (autoVsManu) {
    if (rate > 0.75) start <- start * 0.61
    else start <- start * 0.45
  } else {
    if (rate >= 0.85) start <- start * 0.675
    else if (rate <= 0.65) start <- start * 0.43
    else if (rate >= 0.75) start <- start * rateData[1]
    else start <- start * rateData[2]    
  }
    
  if (!is.na(pitcher)) {
    if (autoVsManu) {
      if (pitcher > 0.0) start <- start * pitcherData[1]
      else start <- start * pitcherData[2]
    } else {
      if (pitcher > 0.0) start <- start * pitcherData[1]
      else if (pitcher < -0.5) start <- start * 0.43
      else start <- start * pitcherData[2]
    }     
  } else start <- 0.0
  
  if (autoVsManu) {
    if (park > 0.5) start <- start * parkData[1]
    else start <- start * parkData[2]    
  } else {
    if (park > 0.62) start <- start * 0.575
    else if (park > 0.5) start <- start * parkData[1]
    else start <- start * parkData[2]        
  }
  
  if (autoVsManu) {
    if (overUnder > 7.5) start <- start * ouData[1]
    else start <- start * ouData[2]    
  } else {
    if (overUnder >= 9.0) start <- start * 0.61
    else if (overUnder >= 7.5) start <- start * 0.56
    else start <- start * 0.532
  }
  
  start
}

series$bayesPredicted <- as.array(apply(series[,c('oddsAdj', 'pitcherAdj', 'matchupAdj', 'productionRate', 'parkAdj', 'overUnder')], 1, 
                                        function(x) bayesPredict(x[1], x[2], x[3], x[4], x[5], x[6])));
plot(jitter(series$bayesPredicted, 0.1), series$production);
summary(series[series$bayesPredicted > summary(series$bayesPredicted)[3],]$actual);
sd(series[series$bayesPredicted > summary(series$bayesPredicted)[3],]$actual);
summary(series[series$bayesPredicted < summary(series$bayesPredicted)[3],]$actual);
sd(series[series$bayesPredicted < summary(series$bayesPredicted)[3],]$actual);

plot(density(series[series$bayesPredicted < summary(series$bayesPredicted)[4],]$actual, na.rm=TRUE));
lines(density(series[series$bayesPredicted > summary(series$bayesPredicted)[4],]$actual, na.rm=TRUE), col=2);

plot(density(series[series$bayesPredicted < summary(series$bayesPredicted)[4],]$production, na.rm=TRUE));
lines(density(series[series$bayesPredicted > summary(series$bayesPredicted)[4],]$production, na.rm=TRUE), col=2);

totalActual <- c(0.0, 0.0)
totalActual[1] <- sum(series[series$bayesPredicted > summary(series$bayesPredicted)[4],]$actual)
totalActual[2] <- sum(series[series$bayesPredicted < summary(series$bayesPredicted)[4],]$actual)
