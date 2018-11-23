

# Preprocessing -----------------------------------------------------------

train <- features_deep[tr_id,]
test  <- features_deep[-tr_id,]

val <- train[val_id,]
train <- train[-val_id,]

train_label <- y[-val_id] %>% data.matrix()
val_label <- y[val_id]

train_features <- train %>% data.matrix() %>% normalize()
val_features <- val %>% data.matrix() %>% normalize()
test_features <- test %>% data.matrix() %>% normalize()


# Model -------------------------------------------------------------------

model <- keras_model_sequential()

model %>%
	layer_dense(input_shape = ncol(train),
							units = 2^10, activation = 'relu') %>%
	layer_dense(units = 2^10, activation = 'relu') %>%
	layer_dropout(rate = 0.6) %>%
	layer_dense(units = 2^10, activation = 'relu') %>%
	layer_dropout(rate = 0.6) %>%
	layer_dense(units = 1,   activation = 'sigmoid')

model %>% compile(
	optimizer = 'adam',
	loss = 'binary_crossentropy',
	metrics = c('accuracy')
)

history <- model %>%
	keras::fit(
		train_features,
		train_label,
		bach_size = 64,
		epochs = 25,
		validation_data = list(val_features, y[val_id])
	)

y_nn <- model %>% predict_proba(test_features) %>% .[,1]
