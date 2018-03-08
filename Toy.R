toy <- read.csv("simpletoy.csv",
               header = TRUE)
plot(toy,pch=16)
library(e1071)
svm.model <- svm(y ~ x, data = toy, kernel='linear')
#print(svm.model$coefs)
W <- crossprod(svm.model$coefs,svm.model$SV)
b <- svm.model$rho
svm.predict <- predict(svm.model,toy)
points(toy$x,svm.predict,col='red',pch=16)
library(Metrics)
RMSEsvm=rmse(svm.predict,toy$y)
OptModelsvm=tune(svm, y~x, data=toy,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModelsvm)