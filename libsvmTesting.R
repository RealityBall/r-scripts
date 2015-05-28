library(e1071)
library(SparseM)

matrix.train <- data.matrix(fullTrainDescr)
for (i in 1:nrow(matrix.train)) {
  if (is.na(matrix.train[i,1])) fullTrainDescr[i,1] <- 0.0
  if (is.na(matrix.train[i,2])) fullTrainDescr[i,2] <- 0.0
}
sum(fullTrainDescr)
write.matrix.csr(as.matrix.csr(data.matrix(fullTrainDescr), nrow(fullTrainDescr), ncol(fullTrainDescr)), file="/Users/mauricio/Downloads/svmtest.dat", fullTrainClass)
