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

source("preprocessing.R")
# ------------------------------- F1
f1 <- function(y_true, y_pred) {
  true_positives <- k_sum(k_round(k_clip(y_true * y_pred, 0, 1)))
  possible_positives <- k_sum(k_round(k_clip(y_true, 0, 1)))
  predicted_positives <- k_sum(k_round(k_clip(y_pred, 0, 1)))
  precision <- true_positives / (predicted_positives + k_epsilon())
  recall <- true_positives / (possible_positives + k_epsilon())
  f1_val <- 2*(precision*recall)/(precision+recall+k_epsilon())
  return(f1_val)
}

# ------------------------------- Function for defining NN
cnn_block <- function(filters, kernel_size, pool_size, rate, input_shape = NULL){
  function(x){
    x %>%
      layer_conv_2d(filters, kernel_size, padding="same", input_shape = input_shape) %>%
      layer_activation(activation = "relu") %>%
      layer_batch_normalization() %>%
      layer_max_pooling_2d(pool_size = pool_size) %>%
      layer_dropout(rate = rate)
  }
}
# ------------------------------- Define CNN
num_classes <- 7
channels <- 2   # 2 if both image types are used simulteaneously
dropout_rate <- 0.25  # can be varied/tuned

cnn1_pic <- cnn_block(filters = 8, kernel_size = c(5,5), pool_size = c(2,2),
                      rate = dropout_rate, input_shape = shape(39, 16, channels))
cnn2_pic <- cnn_block(filters = 16, kernel_size = c(5,5), pool_size = c(2,2),
                      rate = dropout_rate)

cnn_pic <- keras_model_sequential() %>%
  cnn1_pic() %>%
  cnn1_pic() %>%
  layer_flatten() %>%
  layer_activation(activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout_rate) %>%
  layer_dense(num_classes)

# ------------------------------- Function for model cv
define_mod <- function(list_formula_structured, list_formula_cnn, list_data, target, fam) {
  tmp <- deepregression(y = target,
                      list_of_formulas = list_formula_structured,
                      family = "multinoulli",
                      data = list_data,
                      list_of_deep_models = list_formula_cnn,
                      optimizer = optimizer_adam(),
                      monitor_metrics = list(
                        f1,
                        "categorical_accuracy",
                        "categorical_crossentropy",
                        tf$keras$metrics$AUC())
  )
return(tmp)
}

# ------------------------------- define formulas for struct. part
mod_formulas <- list(
  "mod_date" = list(~ 1 + s(date_numeric) + d(image)),
  "mod_month" = list(~ 1 + s(month) + d(image)),
  "mod_lags" = list(~ 1 + lag_1 + lag_2 + lag_3 + d(image)),
  "mod_season" = list(~ 1 + season + d(image)),
  "mod_year" = list(~ 1 + s(year) + d(image)),
  "mod_lags_date" = list(~ 1 + lag_1 + lag_2 + lag_3 + s(date_numeric) + d(image)),
  "mod_lags_month" = list(~ 1 + lag_1 + lag_2 + lag_3 + s(month) + d(image)),
  "mod_lags_season" = list(~ 1 + lag_1 + lag_2 + lag_3 + season + d(image)),
  "mod_lags_year" = list(~ 1 + lag_1 + lag_2 + lag_3 + s(year) + d(image)),
  "mod_lags_date_ext" = list(
    ~ 1 + lag_1 + lag_2 + lag_3 +
      lag_4 + lag_5 + lag_6 +
      lag_7 + lag_8 + lag_9 + lag_10 +
      s(date_numeric) + d(image)
  ),
  "mod_lags_month_ext" = list(
    ~ 1 + lag_1 + lag_2 + lag_3 +
      lag_4 + lag_5 + lag_6 +
      lag_7 + lag_8 + lag_9 + lag_10 +s(month) + d(image)
  ),
  "mod_lags_season_ext" = list(
    ~ 1 + lag_1 + lag_2 + lag_3 +
      lag_4 + lag_5 + lag_6 +
      lag_7 + lag_8 + lag_9 + lag_10  + season + d(image)
  ),
  "mod_lags_year_ext" = list(
    ~ 1 + lag_1 + lag_2 + lag_3 +
      lag_4 + lag_5 + lag_6 +
      lag_7 + lag_8 + lag_9 + lag_10 + s(year) + d(image)
  ),
  "mod_lags_season_num_ext" = list(
    ~ 1 + lag_1 + lag_2 + lag_3 +
      lag_4 + lag_5 + lag_6 +
      lag_7 + lag_8 + lag_9 + lag_10 + s(season_num, k = 2) + d(image)
  ),
  "mod_lags_season_year_ext" = list(
    ~ 1 + lag_1 + lag_2 + lag_3 +
      lag_4 + lag_5 + lag_6 +
      lag_7 + lag_8 + lag_9 + lag_10 + s(season_num, k = 2) + s(year) + d(image)
  ),
  "mod_lags_season_date_ext" = list(
    ~ 1 + lag_1 + lag_2 + lag_3 +
      lag_4 + lag_5 + lag_6 +
      lag_7 + lag_8 + lag_9 + lag_10 + s(season_num, k = 2) + s(date_numeric) + d(image)
  )
)

str(data)
# ------------------------------- initialize models
mods <- list(c(1:9))

mods_2nd <- list(c(1:7))
for (i in c(1:7)) {
  print(i+9)
  print(names(mod_formulas)[[i+9]])
  print(mod_formulas[[i+9]])
  assign(names(mod_formulas)[[i+9]], define_mod(mod_formulas[[i+9]], list(d = cnn_pic), data, y, "multinoulli"))
  mods_2nd[[i]] <- get(names(mod_formulas)[[i+9]])
}

# ------------------------------- do cv
for(i in c(1:7)) {
  mod_tmp_name <- names(mod_formulas)[[i+9]]
  mod_tmp <- get(mod_tmp_name)
  print(mod_tmp_name)
  assign(paste0(mod_tmp_name, "_cv"), mod_tmp %>% cv(epochs = 50, cv_folds = indcs_final,
                                                shuffle = FALSE,
                                                class_weight = list(class_weigths), batch_size = 100))
}

# ------------------------------- evaluate results
for(i in c(1:9)) {
  mod_tmp_name <- names(mod_formulas)[[i]]
  cv_result_tmp <- get(paste0(mod_tmp_name, "_cv"))
  v_auc <- c(1:10)
  for (j in c(1:10)){
    #print("v_auc")
    #print(v_auc)
    #print("cv_res")
    #print(cv_result_tmp[[j]]$metrics$val_auc)
    v_auc[j] <- mean(cv_result_tmp[[j]]$metrics$val_auc)
  }
  print(paste(mod_tmp_name,mean(v_auc)))
}

