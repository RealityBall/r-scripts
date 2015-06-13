install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)

par(mfrow = c(1, 1))
fit = glmnet(data.matrix(trainDescr), trainClass, family = "binomial")
plot(fit, xvar = "dev", label = TRUE)
summary(fit)
print(fit)
coef(fit)

cvfit = cv.glmnet(data.matrix(trainDescr), trainClass)
plot(cvfit)

coef(cvfit, s = "lambda.min")

series$glmNetPredicted <- predict(cvfit, newx = data.matrix(fullTrainDescr), type = "response", s = "lambda.min")[,1]
hist(series$glmNetPredicted)

par(mfrow = c(1, 1))
plot(jitter(series$glmNetPredicted, 0.2), jitter(series$production, 0.2));

summary(series$glmNetPredicted)

# Results by halves
plot(density(series[series$glmNetPredicted < summary(series$glmNetPredicted)[4],]$actual, na.rm=TRUE));
lines(density(series[series$glmNetPredicted > summary(series$glmNetPredicted)[4],]$actual, na.rm=TRUE), col=2);
# Results by q1, q23, q4
plot(density(series[series$glmNetPredicted < summary(series$glmNetPredicted)[2],]$actual, na.rm=TRUE), col=2, lwd=2.5,
          main="GLMNET Regularized Logistic (2014)", xlim=c(-3,15))
lines(density(series[series$glmNetPredicted > summary(series$glmNetPredicted)[2] & 
                     series$glmNetPredicted < summary(series$glmNetPredicted)[5],]$actual, na.rm=TRUE), col=3, lwd=2.5);
lines(density(series[series$glmNetPredicted > summary(series$glmNetPredicted)[5],]$actual, na.rm=TRUE), col=4, lwd=2.5);
legend(10, 0.20, c("Quartile 1", "Quartile 2 & 3", "Quartile 4"),
       lty=c(1,1,1), lwd=c(2.5,2.5,2.5),col=c("red", "green", "blue")) 
