library(tidyverse)
library(keras)

K <- keras::backend()

train_x <- readRDS('train_x.rds')
train_y <- readRDS('train_y.rds')
test    <- readRDS('test.rds')

# Model ----
model <- keras_model_sequential()

model %>%
  layer_dense(
    units = 100, 
    activation = 'relu', 
    input_shape = c(ncol(train_x))
  ) %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 1, activation = 'sigmoid')

model %>% compile(
  optimizer = "adam", 
  loss = "binary_crossentropy",
  metrics = list('acc')
)

# Train ----
history <- model %>% 
  fit(
    as.matrix(train_x), 
    as.matrix(train_y), 
    batch_size = 32,
    epochs = 1
  )

# Predict + Submit ----
preds <- 
  model %>% 
  predict(as.matrix(test))

read_csv("data-raw/sample_submission.csv") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = preds) %>%
  write_csv('deep_learning.csv')
