y <- train$Survived
train$Survived <- NULL
tr_id <- 1:nrow(train)
te_id <- test$PassengerId
tr_te <- bind_rows(train,test)
