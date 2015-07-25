library(caret)
library(rpart)
setwd("~/GitHub/datasciencecoursera/PracticalML/Project")

pmltrain <- read.csv("pml-training.csv")
dim(pmltrain)

# eliminate NAs to clean the data
NAcount <- colSums(is.na(pmltrain))
hist(NAcount)

smtrain <- pmltrain[,!NAcount] # eliminate columns with mostly NAs
summary(smtrain$classe)
str(smtrain)

inTrain <- createDataPartition(y=smtrain$classe, p=.05, list=FALSE)
trainset <- smtrain[inTrain,]
testset <- smtrain[-inTrain,]
dim(trainset)

m1 <-train(classe ~ ., method="rpart", data=trainset)
confusionMatrix( trainset$classe, predict(m1, trainset))

# preProc <- preProcess(trainset[,-93], method="pca", thres = .9)

m2 <- train(classe ~ ., method="treebag", data=trainset)

1
# models to try: rpart, lda, gbm, nb, rf