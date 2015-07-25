# Quiz 1


# Quiz 2
# install.packages("AppliedPredictiveModeling")
# install.packages("caret")
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
#1 answer - choice #3

#2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(concrete$Superplasticizer)
hist(log10(concrete$Superplasticizer))
#2 answer - ??

#3
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
training_IL = training[,c(1, grep("^IL", names(predictors)))]
ptIL <- preProcess(training_IL[,-1], method="pca", thres = .9)
#3 answer - 9 components needed

#4
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
training_IL = training[,c(1, grep("^IL", names(predictors)))]
testing_IL = testing[,c(1, grep("^IL", names(predictors)))]

trainAll <- train(diagnosis ~ ., method="glm", data=training_IL)
confusionMatrix(training$diagnosis, predict(trainAll, testing_IL))

ptIL <- preProcess(training_IL[,-1], method="pca", thres = .8)
trainptIL <- predict(ptIL, training_IL[,-1])
modelptIL <- train(trainptIL$diagnosis ~ ., method="glm", data = trainptIL)
testptIL <-predict(ptIL, testing_IL[,-1])
confusionMatrix(testing_IL, predict(modelptIL, testptIL))


# Quiz 3
# Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)
?segmentationOriginal
summary(segmentationOriginal)
str(segmentationOriginal)
dummies<-predict(dummyVars(Class ~ Case, data=segmentationOriginal), newdata=segmentationOriginal)
head(dummies)
trainset <- segmentationOriginal[as.logical(dummies[,2]),]
testset <- segmentationOriginal[as.logical(dummies[,1]),]
set.seed(125)
?rpart
mod <- train(Class ~ TotalIntenCh2 + FiberWidthCh1 + PerimStatusCh1, method="rpart", data=trainset)
plot(mod$finalModel, uniform=TRUE, main="Classification Tree")
library(rattle)
fancyRpartPlot(mod$finalModel)
caseA = c(TotalIntenCh2 = 23000, FiberWidthCh1 = 10, PerimStatusCh1=2)
predict(mod, newdata=caseA)
mod$finalModel
# a. PS   b. WS   c. PS

# Q2 - The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.

# Q3
library(pgmm)
data(olive)
olive = olive[,-1]
str(olive)
summary(olive)
newdata = as.data.frame(t(colMeans(olive)))
fit <- train(Area ~ ., method="rpart", data=olive)
fit$finalModel
predict(fit, newdata = newdata)

# Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
str(trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
set.seed(13234)
fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method = "glm", family="binomial", data = trainSA)
missClass(testSA$chd, predict(fit, testSA))
missClass(trainSA$chd, predict(fit, trainSA))

# Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
str(vowel.test)
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
set.seed(33833)
fit <- train(factor(y) ~ ., method="rf", data=vowel.train)
varImp(fit)

# Quiz 4
# Q1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
str(vowel.test)
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
set.seed(33833)
rf_fit <- train(y ~ ., method="rf", data=vowel.train)
gbm_fit <- train(y ~ ., method="gbm", data=vowel.train)
confusionMatrix(vowel.test$y, predict(rf_fit, vowel.test))
confusionMatrix(vowel.test$y, predict(gbm_fit, vowel.test))

# Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
rf_fit <- train(diagnosis ~ ., method="rf", data=training)
gbm_fit <- train(diagnosis ~ ., method="gbm", data=training)
lda_fit <- train(diagnosis ~ ., method="lda", data=training)
confusionMatrix(testing$diagnosis, predict(rf_fit, testing))
confusionMatrix(testing$diagnosis, predict(gbm_fit, testing))
confusionMatrix(testing$diagnosis, predict(lda_fit, testing))
predDF <- data.frame(predict(rf_fit, training), predict(gbm_fit, training), predict(lda_fit, training), diagnosis=training$diagnosis)
combModFit <- train(diagnosis ~ ., method="gam", data=training)
confusionMatrix(testing$diagnosis, predict(combModFit, testing))

# Q3 
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)

# Q4
library(lubridate)  # For year() function below
dat = read.csv("~/Desktop/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)