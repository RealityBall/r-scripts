library("caret")

set.seed(1)

inTrain <- createDataPartition(series$production, p = 3/4, list = FALSE)
fullTrainDescr <- series[,6:ncol(series)]
fullTrainDescr <- fullTrainDescr[, !(colnames(fullTrainDescr) %in% c("production", "fanDuelTrend"))]
fullTrainClass <- series$production

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
                method = "svmRadial", tuneLength = 5,
                trControl = bootControl, scaled = FALSE)
#rfFit <- train(trainDescr, trainClass,
#               method = "rf", tuneLength = 5,
#               trControl = bootControl)
op <- options(digits.secs = 2)
Sys.time()

predicted <- predict(svmFit$finalModel, newdata = testDescr)
actualProduction <- testClass
actual <- testActual
caretSeries <- data.frame(predicted, actualProduction, actual)

summary(caretSeries$predicted)
plot(density(caretSeries[caretSeries$predicted < summary(caretSeries$predicted)[4],]$actual, na.rm=TRUE));
lines(density(caretSeries[caretSeries$predicted > summary(caretSeries$predicted)[4],]$actual, na.rm=TRUE), col=2);

plot(density(caretSeries[caretSeries$predicted < summary(caretSeries$predicted)[2],]$actual, na.rm=TRUE), col=2);
lines(density(caretSeries[caretSeries$predicted > summary(caretSeries$predicted)[2] & 
                          caretSeries$predicted < summary(caretSeries$predicted)[5],]$actual, na.rm=TRUE), col=3);
lines(density(caretSeries[caretSeries$predicted > summary(caretSeries$predicted)[5],]$actual, na.rm=TRUE), col=4);

