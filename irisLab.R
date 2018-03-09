library(caTools)
library(data.tree)
library(caret)
library(ggplot2)
library(e1071)
data(iris)
str(iris)
print(summary(iris))

library(ggvis)
iris %>% ggvis(~Sepal.Length, 
               ~Sepal.Width, 
               fill = ~Species) %>% layer_points()
split = sample.split(iris$Species, SplitRatio = .8)
training_set = subset(iris, split == TRUE)
test_set = subset(iris, split == FALSE)

# nrow(training_set)
# training_set[,1:4] = scale(training_set[,1:4])
# test_set[,1:4] = subset()

sample <- sample.split(iris$Species, SplitRatio = .70)
# train data 80%
x_train <- subset(iris, sample == TRUE)
# test data 20%
x_test <- subset(iris, sample == FALSE)

classifier1 = svm(formula = Species~., data = x_train, kernel = 'linear')
classifier2 = svm(formula = Species~ Petal.Width + Petal.Length, 
                  data = training_set, 
                  kernel = 'linear')
# ctrl.1<-trainControl(method="repeatedcv",number=10,repeats=2,index=cv.10.folds)
# 
# mod3 <-train(x=x_train,y=X_train$Speices,method="rpart",trControl=ctrl.1,tuneLength=5)
# plot(varImp(mod3),main="RPART - Variable Importance Plot")
# y_predicted<-predict(mod3,x_test)
# print(y_predicted)
predictedY <- predict(classifier1, x_test)

print(confusionMatrix(table(x_test$Species,predictedY)))