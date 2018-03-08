library(caTools)
library(data.tree)
library(caret)
library(ggplot2)
library(lattice)
library(rpart.plot)
library(rpart)
mushroom <- read.csv("cs358/mushrooms.csv", header = TRUE)
# print(mushroom)
print(summary(mushroom))
z<-cbind.data.frame(Var=names(mushroom), Total_Class=sapply(mushroom,function(x){as.numeric(length(levels(x)))}))
# print(z)
# set.seed(101)
sample <- sample.split(mushroom$class, SplitRatio = .80)
# train data 80%
x_train <- subset(mushroom, sample == TRUE)
# test data 20%
x_test <- subset(mushroom, sample == FALSE)
# only data's class
y_train<-x_train$class
y_test <- x_test$class

x_train$class<-NULL
x_test$class<-NULL
cv.10.folds<-createMultiFolds(y_train,k=10,times=2)
ctrl.1<-trainControl(method="repeatedcv",number=10,repeats=2,index=cv.10.folds)
mod3 <-train(x=x_train,y=y_train,method="rpart",trControl=ctrl.1,tuneLength=5)
plot(varImp(mod3),main="RPART - Variable Importance Plot")

rpart.plot(mod3$finalModel)
y_predicted<-predict(mod3,x_test)
df1<-data.frame(Orig=y_test,Pred=y_predicted)
cm <- confusionMatrix(table(df1$Orig,df1$Pred))
print(cm)
