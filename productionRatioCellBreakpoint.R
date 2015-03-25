
# Pr(actual > 0.0 | oddsAdj > 0.0) = 0.3
# Pr(actual > 0.0 | productionRate > 0.6) = 0.3
# Pr(actual > 0.0 | matchupAdj > 0.0) = 0.1

newProdRateCell <- function(origSeries, breakPoint1, breakPoint2) {
  hiRate <- origSeries[origSeries$X1 >= breakPoint1 & origSeries$X2 >= breakPoint2,];
  log(length(hiRate$X3))
  #length(hiRate$X3[hiRate$X3 == 1]) / length(hiRate$X3);
}
newProdRateRow <- function(origSeries, breakPoint) {
  apply(break2seq, 1, function(x) newProdRateCell(origSeries, breakPoint, x));
}

origSeries <- data.frame(cbind(series$parkAdj, series$pitcherAdj, series$production));
break1seq <- as.array(seq(-0.2, 0.2, 0.01))
break2seq <- as.array(seq(-1.0, 3.0, 0.05))

ratioSeries <- data.matrix(apply(break1seq, 1, function(x) newProdRateRow(origSeries, x)));
heatmap <- heatmap(ratioSeries, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))
factor <- data.frame(index=seq(-1, 1, 0.1), hiRatio=ratioSeries[1,], loRatio=ratioSeries[2,], universeCount=ratioSeries[3,]);
plot(factor$index, factor$ratio);

p1 <- 
  ggplot(factor, aes(index)) +
  geom_line(aes(y=hiRatio, color="hiRatio")) +
  geom_line(aes(y=loRatio, color="loRatio")) +
  ggtitle("Production Factor")
p2 <- 
  ggplot(factor, aes(index)) + 
  geom_line(aes(y=universeCount, colour="hiUniverse")) +
  geom_line(aes(y=-universeCount + factor$universeCount[1], colour="loUniverse")) +
  ggtitle("Universe Count")
multiplot(p1, p2, cols=1)