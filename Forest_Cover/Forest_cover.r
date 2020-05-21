#Initialize library for svm

setwd("~/Forest_Cover")

library(e1071)
library(ggplot2)
library(lattice)
library(caret)

Forest_train <- read.csv("C:/users/mitesh~1/desktop/Forest_Cover/train.csv", header=T, sep = ',')
attach(Forest_train)
Forest_test <- read.csv("C:/users/mitesh~1/desktop/Forest_Cover/test.csv", header = T, sep = ',')
attach(Forest_test)

summary(Forest_train)
plot(Forest_train$Soil_Type1)

#factorizing hillshade
#Forest_train$Hillshade_9am <- as.factor(Forest_train$Hillshade_9am/255)
#Forest_test$Hillshade_9am <- as.factor(Forest_test$Hillshade_9am/255)


#Forest_train$Hillshade_Noon <- as.factor(Forest_train$Hillshade_Noon/255)
#Forest_test$Hillshade_Noon <- as.factor(Forest_test$Hillshade_Noon/255)

#Forest_train$Hillshade_3pm <- as.factor(Forest_train$Hillshade_3pm/255)
#Forest_test$Hillshade_3pm <- as.factor(Forest_test$Hillshade_3pm/255)

#factorizing Wildness_area
Forest_train$Wilderness_Area1 <- as.factor(Forest_train$Wilderness_Area1)
Forest_test$Wilderness_Area1 <- as.factor(Forest_test$Wilderness_Area1)

Forest_train$Wilderness_Area2 <- as.factor(Forest_train$Wilderness_Area2)
Forest_test$Wilderness_Area2 <- as.factor(Forest_test$Wilderness_Area2)

Forest_train$Wilderness_Area3 <- as.factor(Forest_train$Wilderness_Area3)
Forest_test$Wilderness_Area3 <- as.factor(Forest_test$Wilderness_Area3)

Forest_train$Wilderness_Area4 <- as.factor(Forest_train$Wilderness_Area4)
Forest_test$Wilderness_Area4 <- as.factor(Forest_test$Wilderness_Area4)

#Factorization of Aspect


#Forest_train$Aspect <- as.factor(Forest_train$Aspect/360)
#Forest_test$Aspect <- as.factor(Forest_test$Aspect/360)

#Factorization of Cover Type

Forest_train$Cover_Type <- as.factor(Forest_train$Cover_Type)

#creating Formula

formula <- Cover_Type ~ Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Wilderness_Area1 + Wilderness_Area2 + Wilderness_Area3 + Wilderness_Area4 + Soil_Type1 + Soil_Type2 + Soil_Type3 + Soil_Type4 + Soil_Type5 + Soil_Type6 + Soil_Type7 + Soil_Type8 + Soil_Type9 + Soil_Type10 + Soil_Type11 + Soil_Type12 + Soil_Type13 + Soil_Type14 + Soil_Type15 + Soil_Type16 + Soil_Type17 + Soil_Type18 + Soil_Type19 + Soil_Type20+ Soil_Type21 + Soil_Type22 + Soil_Type23 + Soil_Type24 + Soil_Type25 + Soil_Type26 + Soil_Type27 + Soil_Type28 + Soil_Type29 + Soil_Type30 + Soil_Type31 + Soil_Type32 + Soil_Type33 + Soil_Type34 + Soil_Type35 + Soil_Type36 + Soil_Type37 + Soil_Type38 + Soil_Type39 + Soil_Type40 

formula1 <- Cover_Type ~ Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Wilderness_Area1 + Wilderness_Area2 + Wilderness_Area3 + Wilderness_Area4 + Soil_Type1 + Soil_Type2 + Soil_Type3 + Soil_Type4 + Soil_Type5 + Soil_Type6 + Soil_Type8 + Soil_Type9 + Soil_Type10 + Soil_Type11 + Soil_Type12 + Soil_Type13 + Soil_Type14 + Soil_Type16 + Soil_Type17 + Soil_Type18 + Soil_Type19 + Soil_Type20+ Soil_Type21 + Soil_Type22 + Soil_Type23 + Soil_Type24 + Soil_Type25 + Soil_Type26 + Soil_Type27 + Soil_Type28 + Soil_Type29 + Soil_Type30 + Soil_Type31 + Soil_Type32 + Soil_Type33 + Soil_Type34 + Soil_Type35 + Soil_Type36 + Soil_Type37 + Soil_Type38 + Soil_Type39 + Soil_Type40 


str(Forest_train$Wilderness_Area1 )
str(Forest_test$Wilderness_Area1)

#View(Forest_train$Hillshade_9am)
#str(Forest_Data$Soil_Type7)
#View(Forest_Data2)
#View(Forest_Data$Cover_Type)


#Index_2 = 1
#train_Set <- Forest_Data[-Index_1] 
#test_Set <- Forest_2[-Index_2]
#View(train_Set)
#dim(train_Set)

# apply random forest
library(randomForest)

set.seed(415)

fit <- randomForest(formula1, data=Forest_train, importance=TRUE, ntree=200)
varImpPlot(fit)
Prediction <- predict(fit, Forest_test)
head(Prediction)
summary(Prediction)
#newDataSet <- Prediction
#newDataSet
submit.randomForest <- data.frame(Id = Forest_test$Id, Cover_Type = Prediction)
View(submit.randomForest)
#submit.randomForest$Cover_Type <- cbind(submit.randomForest, newDataSet )
write.csv(submit.randomForest, file = "forest_second.csv", row.names = FALSE)
#Formula: svm(<column name of class attribute., data= <training dataset>)





#SVM Prediction 
library(e1071)
set.seed(500)
mod <- svm(formula1, data= Forest_train, kernel = "radial", degree = 3, gamma = 1, cost = 2, type="C-classification")

prediction <- predict(mod, newdata = Forest_test)

submit.svm <- data.frame(Id = Forest_test$Id, Cover_Type = prediction)
write.csv(submit.svm, file = "svm_first.csv", row.names = FALSE)
head(prediction)
#ctree prediction
library(grid)
library(mvtnorm)
library(modeltools)
library(stats4)
library(party)

#build our model
fit.ctree <- ctree(formula1, data=Forest_train)

#examine model for variable importance
fit.ctree
plot(fit.ctree )

#run model against test data set
predict.ctree <- predict(fit.ctree, Forest_test)

#build a dataframe with our results
submit.ctree <- data.frame(Id = Forest_test$Id, Cover_Type=predict.ctree)

#write results to .csv for submission
write.csv(submit.ctree, file="submit_ctree_v1.csv",row.names=FALSE)


svmmodel <- svm(Cover_Type~., data = train_Set)
summary(svmmodel)
head(svmmodel)
#Predict the test set using the created svm model
svmpredict <- predict(svmmodel, test_Set)

#build a dataframe with our results
submit.svm <- data.frame(datetime = test$datetime, count=predict.ctree)

#write results to .csv for submission
write.csv(submit.ctree, file="submit_ctree_v1.csv",row.names=FALSE)

View(svmpredict)
#Cross-tabulation of prediction against the TRUE classes
svmconfmat <- table(true = testrecords[,7], pred = svmpredict)
#Display the tabulation
svmconfmat

 # confusion matrix
library(lattice)
library(ggplot2)
library(caret)
confusionMatrix(prediction, submit.svm[, 2])

