####LIBRARIES

library(readr)
library(magrittr)
library(dplyr)
library(tidyverse)
library(xgboost)
library(gbm)
library(caret)

#install.packages("pROC")
library(pROC)
#install.packages("MLmetrics")
library(MLmetrics)

#install.packages('e1071')
library(e1071)

data <- read_csv("train.csv")
+
#NAs

#apply(train,2,function(x) sum(is.na(x)))

#XGBoost

# Specify Cross-Validation and number of folds, enable parallel computation 
#xgb_trcontorl = trainControl( method= "cv", number = 5, allowParallel = FALSE, verboseIter = TRUE, returnData = FALSE )




# grid space to search for best hyperparameters  
xgbGrid <- expand.grid(nrounds=c(100,200), 
                       max_depth=c(20),
                       colsample_bytree = c(0.9) , 
                       eta= c(0.01),
                       gamma= c(5),
                       min_child_weight = c(1),
                       subsample = c(0.8))


log_loss_result<- vector(mode="numeric", length=5)
auc_result <- vector(mode="numeric", length=5)
tune_parameter<- list()
accuracy_result <- vector(mode="numeric", length=5) 




# train model 
set.seed(123)
train <- data[sample(seq(1:nrow(data)), round(0.8*nrow(data))),]
test <- data[-c(sample(seq(1:nrow(data)), round(0.8*nrow(data)))),]

x_train <- train %>% select(-c(target,ID_code))
y_train <- train %>% select(c(target))
y_train <- as.factor(y_train$target)

x_test <- test %>% select(-c(target,ID_code))
y_test <- test %>% select(c(target))
y_test <- as.factor(y_test$target)


xgb_model = train(x_train, y_train, 
                    tuneGrid = xgbGrid,
                    method="xgbTree", verbose=2)
  # best value for hyperparaeters  
  tune_parameter<- xgb_model$bestTune
  
  # Model evaluation -- AUC 
  predicted <- predict(xgb_model, x_test)
  roc_obj <- roc(as.numeric(levels(y_test)[y_test]), as.numeric(levels(predicted)[predicted]))   
  auc_result <- auc(roc_obj)
  

print(tune_parameter)
print(auc_result)
print(mean(auc_result))

