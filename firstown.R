library(rpart)
library(caret)
library(lattice)
split=0.80
summary(mushroom)
mushroom$veil.type <- NULL
trainIndex <- createDataPartition(mushroom$class, p=split, list=FALSE)
data_train <- data[ trainIndex,]
data_test <- data[-trainIndex,]

dtree_model_mean <- rpart( ~ season + weekday + workingday + atemp + hum + windspeed,data=data_train,method="class")