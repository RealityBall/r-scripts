library("caret")

set.seed(1)

inTrain <- createDataPartition(series$production, p = 3/4, list = FALSE)
trainDescr <- series[inTrain,5:ncol(series) - 1]
testDescr <- series[-inTrain,5:ncol(series) - 1]
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
bootControl <- trainControl(number = 50)
set.seed(2)
#svmFit <- train(trainDescr, trainClass,
#                method = "svmRadial", tuneLength = 5,
#                trControl = bootControl, scaled = FALSE)
rfFit <- train(trainDescr, trainClass,
               method = "rf", tuneLength = 5,
               trControl = bootControl)
op <- options(digits.secs = 2)
Sys.time()

predicted <- predict(svmFit$finalModel, newdata = testDescr)
actualProduction <- testClass
actual <- testActual
caretSeries <- data.frame(predicted, actualProduction, actual)

summary(caretSeries$predicted)
plot(density(caretSeries[caretSeries$predicted < summary(caretSeries$predicted)[4],]$actual, na.rm=TRUE));
lines(density(caretSeries[caretSeries$predicted > summary(caretSeries$predicted)[4],]$actual, na.rm=TRUE), col=2);

