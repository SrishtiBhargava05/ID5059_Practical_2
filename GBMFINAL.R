#Loading the data
DF <- read.csv("FullData", header = TRUE)
library("tidyverse")
DF <- DF%>%select(-X)

#Converting to factor
DF$Month <- as.factor(DF$Month)
DF$DayOfWeek <- as.factor(DF$DayOfWeek)
DF$DayofMonth<- as.factor(DF$DayofMonth)
DF$DepHour <- as.factor(DF$DepHour)
DF$DelayTime <- as.factor(DF$DelayTime)

#Dividing the data into train and test
set.seed(1714155)
samp <- sample(nrow(DF), 0.6 * nrow(DF))
train <- DF[samp, ]
test <- DF[-samp, ]
write.csv(train, "Train.csv")
write.csv(test, "Test.csv")
summary(train$DelayTime)/nrow(train)

#Down Sampling to keep equal number of observations for both delays and non-delays
set.seed(1714155)
down_train <- downSample(x = train[, -ncol(train)],
                         y = train$DelayTime)
fitControl <- trainControl(method = "repeatedcv", number = 2)

#Model: Altering the predictors 
#depth - 9
#ntrees - 500
#learning rate and observations per node held constant 
#Not the best. 
gbmGrid <-  expand.grid(interaction.depth = 9, 
                        n.trees = 500, 
                        shrinkage = 0.1, n.minobsinnode = 10)
gbmFit<- train(Class ~ Month +Origin + Dest+ Distance+
                 DepHour, data = down_train, method = "gbm", 
               trControl = fitControl, tuneGrid =  gbmGrid, verbose = FALSE)
preds1<- predict.train(gbmFit1, newdata = test)
truth <- test$DelayTime
x <- table(preds1, truth)
caret::confusionMatrix(x, reference = "1")

#Boosting without altering the parameters using the best tree
#2-fold CV
#n.trees = 50, 100, 150 
#Depth of trees = 1,2,3
#learning rate at a constant 0.1
#number of observations for a node to be considered is at a constant 10 
#Selected boost has 150 trees, depth 3 
gbmFit2 <- train(Class ~ Month + Origin + Dest+ UniqueCarrier +
                   DepHour, data = down_train, method = "gbm", 
                 trControl = fitControl, verbose = FALSE)
gbmFit2
#Predicting the test data
preds1<- predict.train(gbmFit2, newdata = test)
truth <- test$DelayTime

#Confusion Matrix
x <- table(preds1, truth)
caret::confusionMatrix(x, positive = "1")

#Model with params adjusted didnt do much to the model thus the final model 
#is the one above this
#depth - 8 ntrees - 500 
gbmGrid <-  expand.grid(interaction.depth = 8, 
                        n.trees = 500, 
                        shrinkage = 0.1, n.minobsinnode = 10)
gbmFits<- train(Class ~ Month +Origin + Dest+ UniqueCarrier+
                  DepHour, data = down_train, method = "gbm", 
                trControl = fitControl, tuneGrid =  gbmGrid, verbose = FALSE)
preds2<- predict.train(gbmFits, newdata = test)
truth <- test$DelayTime
x <- table(preds2, truth)
caret::confusionMatrix(x, reference = "1")

