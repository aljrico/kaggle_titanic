
# Preprocessing -----------------------------------------------------------


train <- features_tree[tr_id,]
test  <- features_tree[-tr_id,]

val <- train[val_id,]
train <- train[-val_id,]

train_label <- y[-val_id] %>% data.matrix()
val_label <- y[val_id]

train_features <- train %>% data.matrix()
val_features <- val %>% data.matrix()
test_features <- test %>% data.matrix()


# Model -------------------------------------------------------------------


source("tune_xgb.R")
source("get_folds.R")

dtrain <- xgb.DMatrix(train_features, label = y[-val_id])
dtest  <- xgb.DMatrix(test_features)
dval   <- xgb.DMatrix(val_features, label = y[val_id])

train_tune <- train_features %>% as_tibble() %>%  dplyr::mutate(target = y[-val_id])

tuning_scores <- tune_xgb(train_data = train_tune,
												 target_label = "target",
												 ntrees = 100,
												 objective = "binary:logistic",
												 eval_metric = "error",
												 fast = FALSE)

m <- which.max(tuning_scores$scores)
currentSubsampleRate <- tuning_scores[["subsample"]][[m]]
currentColsampleRate <- tuning_scores[["colsample_bytree"]][[m]]
lr <- tuning_scores[["lr"]][[m]]
mtd <- tuning_scores[["mtd"]][[m]]
mcw <- tuning_scores[["mcw"]][[m]]

ntrees <- 1e4

p <- list(objective = "binary:logistic",
					booster = "gbtree",
					eval_metric = "error",
					eta = lr/ntrees,
					max_depth = mtd,
					min_child_weight = 30,
					gamma = 0,
					subsample = currentSubsampleRate,
					colsample_bytree = currentColsampleRate,
					colsample_bylevel = 0.632,
					alpha = 0,
					lambda = 0,
					nrounds = ntrees)

xgb_model <- xgb.train(p, dtrain, p$nrounds, list(val=dval), print_every_n = 50, early_stopping_rounds = 300)

y_xgb <- predict(xgb_model, dtest, type = "prob")
rm(p,xgb_model,ntrees,dtrain,dtest,dval,train_tune);gc()
