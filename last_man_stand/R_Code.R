#Last man stand problem hackthon by analytics vidya

library(rpart)

train <- read.csv("Train_Fyxd0t8.csv", header = TRUE, stringsAsFactors = FALSE)
head(train)

test <- read.csv("Test_C1XBIYq.csv", header = TRUE, stringsAsFactors = FALSE)
submission <- data.frame(test$ID)
names(submission) <- "ID"

## decision tree for classification
train$Crop_Damage <- as.factor(train$Crop_Damage)

formula <- formula(Crop_Damage ~ Estimated_Insects_Count + Crop_Type+
                       Soil_Type + Pesticide_Use_Category + Number_Doses_Week+
                       Number_Weeks_Used + Number_Weeks_Quit + Season)

model1 <- rpart(formula1, method = "class",data = train)

printcp(model1)
plot(model1)
summary(model1)

# plot tree
plot(model1, uniform=TRUE,
     main="Classification Tree for Kyphosis")
text(model1, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree
pfit<- prune(model1, cp= model1$cptable[which.min(model1$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for Kyphosis")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


pred <- predict(model1, test[, c("Estimated_Insects_Count", "Pesticide_Use_Category", "Number_Doses_Week","Number_Weeks_Used", "Number_Weeks_Quit")],type = "class")

submission["Crop_Damage"] <- pred
write.csv(submission, "rtree_1.csv", row.names = FALSE)


#### random forest

# Random Forest prediction of Kyphosis data
library(randomForest)
fit <- randomForest(formula1, data=train, ntree=500, importance=TRUE, na.action=na.roughfix)
print(fit) # view results
importance(fit) # importance of each predictor
varImpPlot(fit)

formula1 <- formula(Crop_Damage ~ Estimated_Insects_Count + Pesticide_Use_Category + Number_Doses_Week+Number_Weeks_Used + Number_Weeks_Quit)

rand_pred <- predict(fit, test[,c("Estimated_Insects_Count", "Pesticide_Use_Category", "Number_Doses_Week","Number_Weeks_Used", "Number_Weeks_Quit")])
pred_sub <- data.frame(test$ID, rand_pred)
names(pred_sub) <- c("ID", "Crop_Damage")
write.csv(pred_sub, "randon_forest_1.csv", row.names = FALSE)

##SVM
library("e1071")

model2 <- svm(formula, data = train, type="C", kernal= "polynomial")

model2 <- svm(formula , data = train)

predictedY <- predict(model2, test[-1])
predictedY <- predictedY[1:59310]
submission2 <- data.frame(test$ID, predictedY)
names(submission2) <- c("ID", "Crop_Damage")
write.csv(submission2, "svm.csv", row.names= FALSE)
