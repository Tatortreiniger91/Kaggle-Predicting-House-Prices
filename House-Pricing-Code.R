install.packages("tidyr")
install.packages("hydroGOF")
install.packages("dummies")
install.packages("car")
install.packages("caret")
install.packages("randomForest")
install.packages("dplyr")
install.packages("lattice")
install.packages("ggplot2")
library(tidyr)
library(hydroGOF)
library(dummies)
library(car)
library(caret)
library(randomForest)
library(dplyr)

# clear environment
rm(list=ls())

# take the time
ptm_Skript_beginn <- proc.time()

# set path and working directory
mainDir <- "path/Kaggle"
setwd(file.path(mainDir))

# load train and test dataset
train <- read.csv("train.csv", header = T)
test <- read.csv("test.csv", header = T)

# load testsubmission
testsubmission <- read.csv("sample_submission.csv", header = T)

# safe Ids
Id <- test$Id

# log SalePrice
train$SalePrice <- log10(train$SalePrice)

# create the SalePrice variable in test dataset
test$SalePrice <- as.numeric(0)

# combine the datasets without the Id row
whole <- rbind(train[,-1], test[,-1])

# get an overview
summary(whole)
str(whole)

# correct the typo
whole$GarageYrBlt[which(whole$GarageYrBlt==2207)] <- 2007

# create functions to remove the NAs
cleaning_num <- function(x){
  for(i in 1:ncol(x)){
    if(is.numeric(x[,i])){
      x[,i][which(is.na(x[,i]))] <- 0
    }
  }
  return(x)
}
cleaning_fac <- function(x){
  for(i in 1:ncol(x)){
    if(is.factor(x[,i])){
      level <- levels(x[,i])[x[,i]]
      level[is.na(level)] <- "not available"
      x[,i] <- as.factor(level)
    }
  }
  return(x)
}

# exercise the function to the dataset and check again
whole <- cleaning_num(whole)
whole <- cleaning_fac(whole)
summary(whole)

#recode factor variables
whole$BldgType <- recode(whole$BldgType, "1Fam"=5, "2fmCon"=4, "Duplex"=3, "Twnhs"=2, "TwnhsE"=1, "not available"=0)
whole$BsmtQual <- recode(whole$BsmtQual, "Ex"=5, "Gd"=4, "TA"=3, "Fa"=2, "Po"=1, "not available"=0)
whole$BsmtCond <- recode(whole$BsmtCond, "Ex"=5, "Gd"=4, "TA"=3, "Fa"=2, "Po"=1, "not available"=0)
whole$ExterQual <- recode(whole$ExterQual, "Ex"=5, "Gd"=4, "TA"=3, "Fa"=2, "Po"=1, "not available"=0)
whole$ExterCond <- recode(whole$ExterCond, "Ex"=5, "Gd"=4, "TA"=3, "Fa"=2, "Po"=1, "not available"=0)
whole$GarageFinish <- recode(whole$GarageFinish,"Fin"=3, "RFn"=2, "Unf"=1, "not available"=0)
whole$GarageQual <- recode(whole$GarageQual, "Ex"=5, "Gd"=4, "TA"=3, "Fa"=2, "Po"=1, "not available"=0)
whole$GarageCond <- recode(whole$GarageCond, "Ex"=5, "Gd"=4, "TA"=3, "Fa"=2, "Po"=1, "not available"=0)
whole$BsmtExposure<- recode(whole$BsmtExposure,"Gd"=4, "Av"=3,"Mn"=2,"No"=1,"not available"=0)
whole$BsmtFinType1<- recode(whole$BsmtFinType1, "GLQ"=6, "ALQ"=5,"BLQ"=4, "Rec"=3,"LwQ"=2,"Unf"=1,"not available"=0)
whole$BsmtFinType2<- recode(whole$BsmtFinType2, "GLQ"=6, "ALQ"=5,"BLQ"=4, "Rec"=3,"LwQ"=2,"Unf"=1,"not available"=0)
whole$HeatingQC <- recode(whole$HeatingQC, "Ex"=5, "Gd"=4, "TA"=3, "Fa"=2, "Po"=1, "not available"=0)
whole$KitchenQual <- recode(whole$KitchenQual, "Ex"=5, "Gd"=4, "TA"=3, "Fa"=2, "Po"=1, "not available"=0)
whole$FireplaceQu <- recode(whole$FireplaceQu, "Ex"=5, "Gd"=4, "TA"=3, "Fa"=2, "Po"=1, "not available"=0)
whole$PoolQC <- recode(whole$PoolQC, "Ex"=5, "Gd"=4, "TA"=3, "Fa"=2, "Po"=1, "not available"=0)
whole$Fence <- recode(whole$Fence, "GdPrv"=4, "MnPrv"=3, "GdWo"=2, "MnWw"=1, "not available"=0)
whole$Functional<- recode(whole$Functional, "Typ"=7, "Min1"=6, "Min2"=5, "Mod"=4, "Maj1"=3, "Maj2"=2, "Sev"=1, "not available"=0)

# create the TotalArea and SellingAge variables
whole$TotalArea <- whole$TotalBsmtSF + whole$X1stFlrSF + whole$X2ndFlrSF
whole$SellingAge <- whole$YrSold-whole$YearBuilt

# create dummy variables
whole_dummy <-dummy.data.frame(whole, dummy.classes = "factor")

# split in train and test dataset
train2 <- whole_dummy[1:1460,]
test2 <- whole_dummy[1461:2919,]

modelcompare <- numeric(5)

####################
set.seed(1234)
model1 <- train(
  SalePrice~., data=train2,
  method="lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)
pred1 <- predict(model1, test2)
modelcompare[1] <- rmse(log10(testsubmission$SalePrice), pred1)
SalePrice <- 10**pred1
submission <- data.frame(Id, SalePrice)
write.csv(submission, "submission1.csv", row.names = F)

set.seed(1234)
model2 <- train(
  SalePrice~.,
  tuneLength = 5,
  data = train2, method = "lm",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
pred2 <- predict(model2, test2)
modelcompare[2] <- rmse(log10(testsubmission$SalePrice), pred2)
SalePrice <- 10**pred2
submission <- data.frame(Id, SalePrice)
write.csv(submission, "submission2.csv", row.names = F)
# kaggle score 0.13982

set.seed(1234)
model3 <- train(SalePrice~.,
                data=train2,
                method = "ranger",
                trControl = trainControl(
                  method = "cv", number = 10,
                  verboseIter = TRUE
                ))
pred3 <- predict(model3, test2)
modelcompare[3] <- rmse(log10(testsubmission$SalePrice), pred3)
SalePrice <- 10**pred3
submission <- data.frame(Id, SalePrice)
write.csv(submission, "submission3.csv", row.names = F)
# kaggle score 0.13972

set.seed(1234)
model4 <- train(
  SalePrice~.,
  tuneLength = 5,
  data = train2, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
pred4 <- predict(model4, test2)
modelcompare[4] <- rmse(log10(testsubmission$SalePrice), pred4)
SalePrice <- 10**pred4
submission <- data.frame(Id, SalePrice)
write.csv(submission, "submission4.csv", row.names = F)
# kaggle score 0.13804

set.seed(1234)
model5 <- train(
  SalePrice~.,
  tuneLength = 5,
  data = train2, method = "rf",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
pred5 <- predict(model5, test2)
modelcompare[5] <- rmse(log10(testsubmission$SalePrice), pred5)
SalePrice <- 10**pred5
submission <- data.frame(Id, SalePrice)
write.csv(submission, "submission5.csv", row.names = F)
# kaggle score 0.13852

# check the skript running time
modelcompare
ptm_Skript_end <- proc.time()-ptm_Skript_beginn
ptm_Skript_end[[3]]
