library("caret")

set.seed(1)

inTrain <- createDataPartition(series$production, p = 3/4, list = FALSE)
fullTrainDescr <- series[,8:ncol(series)]
#fullTrainDescr <- fullTrainDescr[, !(colnames(fullTrainDescr) %in% c("production", "fanDuelTrend"))]
fullTrainDescr <- fullTrainDescr[, !(colnames(fullTrainDescr) %in% c("production"))]
fullTrainClass <- series$production
matrix.train <- data.matrix(fullTrainDescr)
for (i in 1:nrow(matrix.train)) {
  if (is.na(matrix.train[i,1])) fullTrainDescr[i,1] <- 0.0
  if (is.na(matrix.train[i,2])) fullTrainDescr[i,2] <- 0.0
}
sum(fullTrainDescr)

trainDescr <- fullTrainDescr[inTrain,]
testDescr <- fullTrainDescr[-inTrain,]
trainClass <- series$production[inTrain]
testClass <- series$production[-inTrain]
testActual <- series$actual[-inTrain]

prop.table(table(series$production))
prop.table(table())

descrCorr <- cor(trainDescr)
highCorr <- findCorrelation(descrCorr, 0.90)

xTrans <- preProcess(trainDescr)
trainDescr <- predict(xTrans, trainDescr)
testDescr <- predict(xTrans, testDescr)

op <- options(digits.secs = 2)
Sys.time()
bootControl <- trainControl(number = 10)
set.seed(2)
svmFit <- train(trainDescr, trainClass,
                method = "svmLinear", tuneLength = 5,
                trControl = bootControl, scaled = FALSE)
#svmFit <- train(trainDescr, trainClass,
#                method = "svmRadial", tuneLength = 5,
#                trControl = bootControl, scaled = FALSE)
#rfFit <- train(trainDescr, trainClass,
#               method = "rf", tuneLength = 5,
#               trControl = bootControl)
op <- options(digits.secs = 2)
Sys.time()

predicted <- predict(svmFit$finalModel, newdata = testDescr)
fullPredicted <- predict(svmFit$finalModel, newdata = fullTrainDescr)
finalPredicted <- cbind(series, fullPredicted)

actualProduction <- testClass
actual <- testActual
caretSeries <- data.frame(predicted, actualProduction, actual)

# By halves
summary(caretSeries$predicted)
plot(density(caretSeries[caretSeries$predicted < summary(caretSeries$predicted)[4],]$actual, na.rm=TRUE));
lines(density(caretSeries[caretSeries$predicted > summary(caretSeries$predicted)[4],]$actual, na.rm=TRUE), col=2);
# Results by q1, q23, q4
plot(density(caretSeries[caretSeries$predicted < summary(caretSeries$predicted)[2],]$actual, na.rm=TRUE), col=2, lwd=2.5,
     main="CARET SVM (2014)", xlim=c(-3,15));
lines(density(caretSeries[caretSeries$predicted > summary(caretSeries$predicted)[2] & 
                          caretSeries$predicted < summary(caretSeries$predicted)[5],]$actual, na.rm=TRUE), col=3, lwd=2.5);
lines(density(caretSeries[caretSeries$predicted > summary(caretSeries$predicted)[5],]$actual, na.rm=TRUE), col=4, lwd=2.5);
legend(10, 0.20, c("Quartile 1", "Quartile 2 & 3", "Quartile 4"),
       lty=c(1,1,1), lwd=c(2.5,2.5,2.5),col=c("red", "green", "blue")) 
