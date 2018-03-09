library(caTools)
library(data.tree)
library(caret)
library(ggplot2)
library(lattice)
library(rpart.plot)
library(rpart)
mushroom <- read.csv("cs358/mushrooms.csv", header = TRUE)
# print(summary(mushroom))

sample <- sample.split(mushroom$class, SplitRatio = .80)
# train data 80%
x_train <- subset(mushroom, sample == TRUE)
# test data 20%
x_test <- subset(mushroom, sample == FALSE)

y_train<-x_train$class
y_test <- x_test$class

x_train$class<-NULL
x_test$class<-NULL
cv.10.folds<-createMultiFolds(y_train,k=10,times=2)

# create a control object for repeated cv in caret
ctrl.1<-trainControl(method="repeatedcv",number=10,repeats=2,index=cv.10.folds)

mod3 <-train(x=x_train,y=y_train,method="rpart",trControl=ctrl.1,tuneLength=7)
# plot(varImp(mod3),main="RPART - Variable Importance Plot")
y_predicted<-predict(mod3,x_test)
df1<-data.frame(Orig=y_test,Pred=y_predicted)

# conprint <- confusionMatrix(table(df1$Orig,df1$Pred))
# print(conprint)
# rpart.plot(mod3$finalModel)
library(rpart)
trainingData <- subset(mushroom, sample == TRUE)
testingData <- subset(mushroom, sample == FALSE)

dt.Model      <- rpart( class ~ ., data = trainingData )
dt.Prediction <- predict( dt.Model, newdata = testingData,type ="class" )
print(confusionMatrix(table(dt.Prediction,testingData$class) ))
