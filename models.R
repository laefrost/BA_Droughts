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
mods <- list(c(1:length(mod_formulas)))

for (i in c(1:length(mod_formulas))) {
  assign(names(mod_formulas)[[i]], define_mod(mod_formulas[[i]], list(d = cnn_pic), data, y, "multinoulli"))
  mods[[i]] <- get(names(mod_formulas)[[i]])
}

# ------------------------------- fit models - do hp?
mod_lags_season_ext %>% fit(epochs = 10, early_stopping = TRUE, view_metrics = T)
plot(mod_lags_season_date_ext)

# ------------------------------- do cv
for(i in c(1:(length(mod_formulas)-1))) {
  mod_tmp_name <- names(mod_formulas)[[i]]
  mod_tmp <- get(mod_tmp_name)
  print(mod_tmp_name)
  assign(paste0(mod_tmp_name, "_cv"), mod_tmp %>% cv(epochs = 50, cv_folds = indcs_final,
                                                shuffle = FALSE,
                                                class_weight = list(class_weigths), batch_size = 100))
}

# ------------------------------- evaluate results
df_results <- data.frame("model" = c(1:16), "Auc" = c(1:16),"val_auc" = c(1:16), "loss" = c(1:16), "val_loss" = c(1:16),
                         "categorical_accuracy" = c(1:16), "val_categorical_accuracy" = c(1:16),
                         "categorical_crossentropy" = c(1:16), "val_categorical_crossentropy" = c(1:16),
                         "python_function" = c(1:16), "val_python_function" = c(1:16))
for(i in c(1:(length(mod_formulas)-1))) {
  mod_tmp_name <- names(mod_formulas)[[i]]
  cv_result_tmp <- get(paste0(mod_tmp_name, "_cv"))
  v_auc <- c(1:10)
  v_loss <- c(1:10)
  auc <- c(1:10)
  loss <- c(1:10)
  categorical_accuracy <- c(1:10)
  categorical_crossentropy <- c(1:10)
  v_categorical_accuracy <- c(1:10)
  v_categorical_crossentropy <- c(1:10)
  v_pf <- c(1:10)
  pf <- c(1:10)
  #print("------------------------------------------------------------------")
  for (j in c(1:10)){
    #print("v_auc")
    #print(v_auc)
    #print("cv_res")
    #print(cv_result_tmp[[j]]$metrics$val_auc)
    #print(cv_result_tmp[[j]]$metrics)
    v_auc[j] <- mean(cv_result_tmp[[j]]$metrics$val_auc)
    auc[j] <- mean(cv_result_tmp[[j]]$metrics$auc)
    loss[j] <- mean(cv_result_tmp[[j]]$metrics$loss)
    v_loss[j] <- mean(cv_result_tmp[[j]]$metrics$val_loss)
    v_categorical_crossentropy[j] <- mean(cv_result_tmp[[j]]$metrics$val_categorical_crossentropy)
    categorical_crossentropy[j] <- mean(cv_result_tmp[[j]]$metrics$categorical_crossentropy)
    v_categorical_accuracy[j] <- mean(cv_result_tmp[[j]]$metrics$val_categorical_accuracy)
    categorical_accuracy[j] <- mean(cv_result_tmp[[j]]$metrics$categorical_accuracy)
    v_pf[j] <- mean(cv_result_tmp[[j]]$metrics$val_python_function)
    pf[j] <- mean(cv_result_tmp[[j]]$metrics$python_function)

  }
  #print(paste(mod_tmp_name,mean(v_auc)))
  df_results[i, ] <- c(mod_tmp_name, mean(auc), mean(v_auc),mean(loss),mean(v_loss),
                       mean(categorical_accuracy), mean(v_categorical_accuracy),
                       mean( categorical_crossentropy),mean(v_categorical_crossentropy),
                       mean(pf), mean(v_pf))
  saveRDS(cv_result_tmp, file = paste0("06_06_23-cv_", mod_tmp_name))
}
df_results
saveRDS(df_results, file = paste0("06_06_23-cv_overview"))

tmp_cv <- get("mod_lags_season_ext_cv")
plot(tmp_cv[[4]])

