# temp file for simply fitting and debugging
library(dplyr)
library(ggplot2)
library(mgcv)
library(reshape2)
library(lubridate)
library(nnet)
library(deepregression)
library(tidyr)
library(lattice)
library(caret)
library(stats)
library(plyr)
set.seed(123456)
source("preprocessing.R")
# ------------------------------- F1
f1 <- function(y_true, y_pred) {
  true_positives <- k_sum(k_round(k_clip(y_true * y_pred, 0, 1)))
  possible_positives <- k_sum(k_round(k_clip(y_true, 0, 1)))
  predicted_positives <- k_sum(k_round(k_clip(y_pred, 0, 1)))
  precision <- true_positives / (predicted_positives + k_epsilon())
  recall <- true_positives / (possible_positives + k_epsilon())
  f1_val <- 2 * (precision * recall) / (precision + recall + k_epsilon())
  return(f1_val)
}

# ------------------------------- Function for defining NN
cnn_block <-
  function(filters,
           kernel_size,
           pool_size,
           rate,
           input_shape = NULL) {
    function(x) {
      x %>%
        layer_conv_2d(filters,
                      kernel_size,
                      padding = "same",
                      input_shape = input_shape) %>%
        layer_activation(activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_max_pooling_2d(pool_size = pool_size) %>%
        layer_dropout(rate = rate)
    }
  }
# ------------------------------- Define CNN
num_classes <- 7
channels <- 2   # 2 if both image types are used simulteaneously
dropout_rate <- 0.5  # can be varied/tuned

cnn1_pic <-
  cnn_block(
    filters = 8,
    kernel_size = c(5, 5),
    pool_size = c(2, 2),
    rate = dropout_rate,
    input_shape = shape(39, 16, channels)
  )
cnn2_pic <-
  cnn_block(
    filters = 16,
    kernel_size = c(5, 5),
    pool_size = c(2, 2),
    rate = dropout_rate
  )

cnn_pic <- keras_model_sequential() %>%
  cnn1_pic() %>%
  cnn1_pic() %>%
  layer_flatten() %>%
  layer_activation(activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(num_classes)

# ------------------------------------------ train/test split
indcs_train <- c(1:28370)
indcs_val <- c(28371:32422)
indcs_test <- c(32423:40527)
# ------------------------------------------ model with images
test_mod_imgs <- deepregression(
  y = y[1:28370,],
  list_of_formulas = list(~ 1 + d(image)),
  family = "multinoulli",
  data = lapply(data, function(x) deepregression:::subset_array(x, 1:28370)),
  list_of_deep_models = list(d = cnn_pic),
  optimizer = optimizer_adam(learning_rate = 0.0001),
  monitor_metrics = list(
    f1,
    "categorical_accuracy",
    "categorical_crossentropy",
    tf$keras$metrics$AUC(multi_label = T)
  )
)

print(test_mod_imgs)

history_imgs <- test_mod_imgs %>% fit(epochs = 30, patience = 5L,
                      early_stopping = F, batch_size = 300,
                      validation_data = list(lapply(data, function(x) deepregression:::subset_array(x, 28371:32422)),
                                             y[28371:32422,]), view_metrics = TRUE)

plot(history_imgs)

# ------------------------------------------ model with lags
test_mod_lags <- deepregression(
  y = y[1:28370,],
  list_of_formulas = list( ~ 1 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6),
  family = "multinoulli",
  data = lapply(data, function(x) deepregression:::subset_array(x, 1:28370)),
  list_of_deep_models = list(d = cnn_pic),
  optimizer = optimizer_adam(learning_rate = 0.0001),
  monitor_metrics = list(
    f1,
    "categorical_accuracy",
    "categorical_crossentropy",
    tf$keras$metrics$AUC(multi_label = T)
  )
)



hist_lags <- test_mod_lags %>% fit(epochs = 30, patience = 5L,
                      early_stopping = TRUE, batch_size = 300,
                      validation_data = list(lapply(data, function(x) deepregression:::subset_array(x, 28371:32422)),
                                             y[28371:32422,]), view_metrics = TRUE)

# ------------------------------------------ model with images + lagstest_mod_lags <- deepregression(
test_mod_lags_imgs <- deepregression(
  y = y[1:28370,],
  list_of_formulas = list( ~ 1 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + d(image)),
  family = "multinoulli",
  data = lapply(data, function(x) deepregression:::subset_array(x, 1:28370)),
  list_of_deep_models = list(d = cnn_pic),
  optimizer = optimizer_adam(learning_rate = 0.0001),
  monitor_metrics = list(
    f1,
    "categorical_accuracy",
    "categorical_crossentropy",
    tf$keras$metrics$AUC(multi_label = T)
  )
)


history_mods_imgs_test <- test_mod_lags_imgs %>% fit(epochs = 30, patience = 5L,
                      early_stopping = F, batch_size = 700,
                      validation_data = list(lapply(data, function(x) deepregression:::subset_array(x, 28371:32422)),
                                             y[28371:32422,]), view_metrics = TRUE)
plot(history_mods_imgs_test)


# ------------------------------------------ model with images + lagstest_mod_lags <- deepregression(
test_mod_lags_imgs_month <- deepregression(
  y = y[1:35000,],
  list_of_formulas = list( ~ 1 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + s(month) + d(image)),
  family = "multinoulli",
  data = lapply(data, function(x) deepregression:::subset_array(x, 1:35000)),
  list_of_deep_models = list(d = cnn_pic),
  optimizer = optimizer_adam(learning_rate = 0.0001),
  monitor_metrics = list(
    f1,
    "categorical_accuracy",
    "categorical_crossentropy",
    tf$keras$metrics$AUC(multi_label = T)
  )
)

# patience: How long no improvement in model
hist_mod_lags_imgs_month <- test_mod_lags_imgs_month %>% fit(epochs = 50, patience = 15L,
                                                     early_stopping = T, batch_size = 32,
                                                     validation_data = list(lapply(data, function(x) deepregression:::subset_array(x, 35001:40527)),
                                                                            y[35001:40527,]), view_metrics = TRUE)
plot(hist_mod_lags_imgs_month)
