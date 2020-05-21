library(readr)
library(xgboost)
library(neuralnet)
memory.limit(size=6024)
set.seed(721) #seed bag1:8, then eta=0.06not0.04&nround125not250: bag2:64, bag3:6, bag4:88, bag5: 0.03-300-seed666



library(readr)
library(xgboost)

# Set a random seed for reproducibility
#set.seed(1)

cat("reading the train and test data\n")
train <- read_csv("Train_Fyxd0t8.csv")
test  <- read_csv("Test_C1XBIYq.csv")
train[is.na(train)] <- -9999
test[is.na(test)] <- -9999

submission <- data.frame(ID=test$ID)
feature.names <- names(train)[2:ncol(train)-1]

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
    if (class(train[[f]])=="character") {
        levels <- unique(c(train[[f]], test[[f]]))
        train[[f]] <- as.integer(factor(train[[f]], levels=levels))
        test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    }
}

cat("training a XGBoost classifier\n")
clf <- xgboost(data        = data.matrix(train[,feature.names]),
               label       = train$Crop_Damage,
               eta         = 0.01,
               depth       = 20,
               max_depth   = 15,
               nrounds     = 1000, # best rank 12 with this setup
               subsample           = 0.60,
               colsample_bytree    = 0.60,
               verbose             = 2,
               objective   = "reg:linear",
               eval_metric = "rmse",
               maximize = TRUE)
names(clf)
cat("making predictions\n")

submission$Crop_Damage <- as.integer(round(predict(clf, data.matrix(test[,feature.names]))))

# I pretended this was a regression problem and some predictions may be outside the range
submission[submission$Crop_Damage <0, "Response"] <- 0
submission[submission$Crop_Damage>2, "Response"] <- 2
submission$Response <- NULL
cat("saving the submission file\n")
write.csv(submission, "xgboost.csv", row.names= FALSE)
