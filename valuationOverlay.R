####
#  Logistic Model
###
## Train logistic model
#
seriesTrain <- series[1:(nrow(series) * 0.7),]
seriesTest <- series[(nrow(series) * 0.7):nrow(series),]

producing <- glm(as.factor(production) ~ fanDuelTrend + productionRate + oddsAdj + matchupAdj + pitcherAdj + overUnder + overUnderML, data = seriesTrain, family = binomial);
summary(producing);

seriesTest$predicted <- predict(producing, newdata=seriesTest, type="response")
par(mfrow = c(1, 1))
plot(jitter(seriesTest$predicted, 0.2), jitter(seriesTest$production, 0.2));

summary(seriesTest$producePredict)

# Results by halves
plot(density(seriesTest[seriesTest$predicted < summary(seriesTest$predicted)[4],]$actual, na.rm=TRUE));
lines(density(seriesTest[seriesTest$predicted > summary(seriesTest$predicted)[4],]$actual, na.rm=TRUE), col=2);
# Results by q1, q23, q4
plot(density(seriesTest[seriesTest$predicted < summary(seriesTest$predicted)[2],]$actual, na.rm=TRUE), col=2, lwd=2.5,
     main="GLM Logistic (2014)", xlim=c(-3,15));
lines(density(seriesTest[seriesTest$predicted > summary(seriesTest$predicted)[2] & 
                           seriesTest$predicted < summary(seriesTest$predicted)[5],]$actual, na.rm=TRUE), col=3, lwd=2.5);
lines(density(seriesTest[seriesTest$predicted > summary(seriesTest$predicted)[5],]$actual, na.rm=TRUE), col=4, lwd=2.5);
legend(10, 0.20, c("Quartile 1", "Quartile 2 & 3", "Quartile 4"),
       lty=c(1,1,1), lwd=c(2.5,2.5,2.5),col=c("red", "green", "blue")) 

unique(seriesTest$mlbPos)
fullGroup1B <- seriesTest[seriesTest$mlbPos == '1B',]
fullGroup1B <- fullGroup1B[with(fullGroup1B, order(-predicted)),]
fullGroup2B <- seriesTest[seriesTest$mlbPos == '2B',]
fullGroup2B <- fullGroup2B[with(fullGroup2B, order(-predicted)),]
fullGroupSS <- seriesTest[seriesTest$mlbPos == 'SS',]
fullGroupSS <- fullGroupSS[with(fullGroupSS, order(-predicted)),]
fullGroup3B <- seriesTest[seriesTest$mlbPos == '3B',]
fullGroup3B <- fullGroup3B[with(fullGroup3B, order(-predicted)),]
fullGroupC <- seriesTest[seriesTest$mlbPos == 'C',]
fullGroupC <- fullGroupC[with(fullGroupC, order(-predicted)),]
fullGroupDH <- seriesTest[seriesTest$mlbPos == 'DH',]
fullGroupDH <- fullGroupDH[with(fullGroupDH, order(-predicted)),]
fullGroupOF <- seriesTest[seriesTest$mlbPos == 'OF' | seriesTest$mlbPos == 'LF' | seriesTest$mlbPos == 'CF' | seriesTest$mlbPos == 'RF',]
fullGroupOF <- fullGroupOF[with(fullGroupOF, order(-predicted)),]
par(mfrow = c(3, 2))
plot(jitter(fullGroup1B$predicted, 1.5), jitter(fullGroup1B$actual, 1.5), main=paste("1st Base (nobs = ", nrow(fullGroup1B), ")"), xlab="Predicted prob. of production", ylab="Actual Fanduel", xlim=c(0.15, 0.85), ylim=c(-5.0, 20.0))
abline(lm(fullGroup1B$actual ~ fullGroup1B$predicted), lwd=2.5, col=2)
plot(jitter(fullGroup2B$predicted, 1.5), jitter(fullGroup2B$actual, 1.5), main=paste("2nd Base (nobs = ", nrow(fullGroup2B), ")"), xlab="Predicted prob. of production", ylab="Actual Fanduel", xlim=c(0.15, 0.85), ylim=c(-5.0, 20.0))
abline(lm(fullGroup2B$actual ~ fullGroup2B$predicted), lwd=2.5, col=2)
plot(jitter(fullGroupSS$predicted, 1.5), jitter(fullGroupSS$actual, 1.5), main=paste("Short Stop (nobs = ", nrow(fullGroupSS), ")"), xlab="Predicted prob. of production", ylab="Actual Fanduel", xlim=c(0.15, 0.85), ylim=c(-5.0, 20.0))
abline(lm(fullGroupSS$actual ~ fullGroupSS$predicted), lwd=2.5, col=2)
plot(jitter(fullGroup3B$predicted, 1.5), jitter(fullGroup3B$actual, 1.5), main=paste("3rd Base (nobs = ", nrow(fullGroup3B), ")"), xlab="Predicted prob. of production", ylab="Actual Fanduel", xlim=c(0.15, 0.85), ylim=c(-5.0, 20.0))
abline(lm(fullGroup3B$actual ~ fullGroup3B$predicted), lwd=2.5, col=2)
plot(jitter(fullGroupC$predicted, 1.5), jitter(fullGroupC$actual, 1.5), main=paste("Catcher (nobs = ", nrow(fullGroupC), ")"), xlab="Predicted prob. of production", ylab="Actual Fanduel", xlim=c(0.15, 0.85), ylim=c(-5.0, 20.0))
abline(lm(fullGroupC$actual ~ fullGroupC$predicted), lwd=2.5, col=2)
plot(jitter(fullGroupOF$predicted, 1.5), jitter(fullGroupOF$actual, 1.5), main=paste("Outfield (nobs = ", nrow(fullGroupOF), ")"), xlab="Predicted prob. of production", ylab="Actual Fanduel", xlim=c(0.15, 0.85), ylim=c(-5.0, 20.0))
abline(lm(fullGroupOF$actual ~ fullGroupOF$predicted), lwd=2.5, col=2)
par(mfrow = c(1, 1))

####
#  Random Forest
####
library(miscTools); library(ggplot2); library(randomForest)
#
# Train random forest
rf <- randomForest(as.factor(production) ~ fanduelBase + productionRate + oddsAdj + matchupAdj + pitcherAdj + overUnder, data=seriesTrain, ntree=40, na.action=na.omit);
summary(rf)

# Test random forest
seriesTest$rfPredictions <- predict(rf, newdata=seriesTest, type="response");
summary(seriesTest$rfPredictions)
# Results by halves
plot(density(seriesTest[seriesTest$rfPredictions == 0,]$actual, na.rm=TRUE), col=2, lwd=2.5,
     main="Random Forest (2014)", xlim=c(-3,15));
lines(density(seriesTest[seriesTest$rfPredictions == 1,]$actual, na.rm=TRUE), col=3, lwd=2.5);
legend(10, 0.20, c("Quartile 1", "Quartile 2 & 3", "Quartile 4"),
       lty=c(1,1), lwd=c(2.5,2.5),col=c("red", "green")) 


