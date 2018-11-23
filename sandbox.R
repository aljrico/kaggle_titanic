library(tidyverse)
library(magrittr)
library(data.table)
library(caret)
library(scales)
library(keras)
library(wru)

library(xgboost)
library(progress)


source("numerise_data.R")

# Retrieving Data & Preprocessing ---------------------------------------------------------

train <- fread("data/train.csv")
test  <- fread("data/test.csv")

y <- train$Survived
train$Survived <- NULL
tr_id <- 1:nrow(train)
val_id <- sample(tr_id, 0.2*length(tr_id))
te_id <- test$PassengerId
tr_te <- bind_rows(train,test)

source("na_replace.R")
source("feature_engineering.R")
features_tree <- tr_te %>%  feature_engineering(type = "tree")
features_deep <- tr_te %>%  feature_engineering(type = "deep")


# Modelling ---------------------------------------------------------------

source("xgb.R")
source("neural_network.R")

plot(y_xgb,y_nn)

hist(y_nn)
hist(y_xgb)

y_pred <- floor(y_nn + y_xgb)

predictions <- data.frame(PassengerId = te_id,
													Survived = as.integer(y_pred))

write_csv(predictions, "predictions_dense.csv")
