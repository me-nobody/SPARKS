irisdata<-datasets::iris
table(iris$Species)
library(tidyr)
library(caret)
library(rpart.plot)
summary(irisdata)
str(irisdata)
scaledIrisData<-as.data.frame(scale(irisdata[1:4]))
labels<-irisdata[,5]
scaledIrisData<-cbind(scaledIrisData,labels)
set.seed(3033)
intrain <- createDataPartition(y = irisdata$Species, p= 0.7, list = FALSE)
training <- irisdata[intrain,]
testing <- irisdata[-intrain,]
dim(training);dim(testing)
anyNA(training)
#Classification_by_Information_Gain#############################################
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(Species ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
prp(dtree_fit$finalModel, box.palette="Reds", tweak=1.2)
################################################################################
test_pred_info<-predict(dtree_fit,newdata = testing)
confusionMatrix(test_pred_info,testing$Species)
#Classification_by_Gini_index###################################################
set.seed(3333)
dtree_fit_gini <- train(Species ~., data = training, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 10)
prp(dtree_fit_gini$finalModel,box.palette = "Blues", tweak = 1.2)
################################################################################
test_pred_gini<-predict(dtree_fit_gini,newdata = testing)
confusionMatrix(test_pred_gini,testing$Species)
#Reference:
#1)https://dataaspirant.com/decision-tree-classifier-implementation-in-r/#:~:text=The%20decision%20tree%20classifier%20is,algorithm%20in%20our%20earlier%20articles.
#  Rahul Saxena