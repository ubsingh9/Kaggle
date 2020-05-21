# kaggle ooto comptetion
#import the packages

require(xgboost)
require(methods)
require(reshape2)

#read data to the R console

featureclass = rep('numeric',93)
colclasstrain = c('integer',featureclass,'character')
colclasstest = c('integer',featureclass)

train = read.csv('train.csv', header=T,colClasses=colclasstrain)
test = read.csv('test.csv',header=T,colClasses=colclasstest)

#keep record of the test id for final output

id = test[,1]

#remove the id column

train = train[,-1]
test = test[,-1]

#convert the target from character into integer starting from 0

target = train$target
classnames = unique(target)
target = as.integer(colsplit(target,'_',names=c('x1','x2'))[,2])-1

#remove the target the from train

train = train[,-ncol(train)]

#convert dataset into numeric Matrix format

trainMatrix <- data.matrix(train)
testMatrix <- data.matrix(test)

#cross-validation to choose the parameters

numberOfClasses <- max(target) + 1

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)

cv.nround <- 500
cv.nfold <- 5
bst.cv = xgb.cv(param=param, data = trainMatrix, label = target,
                nfold = cv.nfold, nrounds = cv.nround)

plot(bst.cv$test.mlogloss.mean,lty = 'l')
nround <- which(bst.cv$test.mlogloss.mean==min(bst.cv$test.mlogloss.mean))

#train the model

bst = xgboost(data = trainMatrix, label = target, param=param, nrounds = nround)

#predict the model

ypred = predict(bst, testMatrix)


#prepare for output

predMatrix <- data.frame(matrix(ypred, ncol=9, byrow=TRUE))
colnames(predMatrix) = classnames
res<-data.frame(id, predMatrix)
write.csv(res, 'submission.csv', quote = F, row.names = F)


