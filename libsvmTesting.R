library(e1071)
library(SparseM)

write.matrix.csr(as.matrix.csr(data.matrix(fullTrainDescr), nrow(fullTrainDescr), ncol(fullTrainDescr)), file="/Users/mauricio/Downloads/svmtest.dat", fullTrainClass)
