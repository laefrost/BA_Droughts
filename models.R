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

# ------------------------------- Function for model cv
define_mod <-
  function(list_formula_structured,
           list_formula_cnn,
           list_data,
           target,
           fam) {
    tmp <- deepregression(
      y = target,
      list_of_formulas = list_formula_structured,
      family = "multinoulli",
      data = list_data,
      list_of_deep_models = list_formula_cnn,
      optimizer = optimizer_adam(learning_rate = 0.0003),
      monitor_metrics = list(
        f1,
        "categorical_accuracy",
        "categorical_crossentropy",
        tf$keras$metrics$AUC(multi_label = T)
      )
    )
    return(tmp)
  }

# ------------------------------- define formulas for struct. part
mod_formulas <- list(
  "mod_img" = list( ~ 1 + d(image)),
  #"mod_year" = list(~ 1 + s(year)),
  "mod_lags" = list( ~ 1 + lag_1 + lag_2 + lag_3
                     + lag_4 + lag_5 + lag_6),
  #"mod_season" = list( ~ 1 + season),
  #"mod_img_date" = list( ~ 1 + s(date_numeric) + d(image)),
  #"mod_img_month" = list( ~ 1 + s(month) + d(image)),
  #"mod_img_lags" = list( ~ 1 + lag_1 + lag_2 + lag_3
  #                   + lag_4 + lag_5 + lag_6
  #                   + d(image)),
  #"mod_img_season" = list( ~ 1 + season + d(image)),
  #"mod_img_year" = list(~ 1 + s(year) + d(image)),
  "mod_img_lags_month" = list( ~ 1 + lag_1 + lag_2 + lag_3
                           + lag_4 + lag_5 + lag_6 + s(month) + d(image)),
  "mod_img_lags_season" = list( ~ 1 + lag_1 + lag_2 + lag_3
                            + lag_4 + lag_5 + lag_6  + season + d(image))
  #"mod_img_lags_year" = list( ~ 1 +  lag_1 + lag_2 + lag_3
  #                        + lag_4 + lag_5 + lag_6  + s(year) + d(image)),
  #"mod_img_lags_year_ext" = list( ~ 1 + lag_1 + lag_2 + lag_3
  #                        + lag_4 + lag_5 + lag_6
  #                        + lag_7 + lag_8 + lag_9
  #                        + s(year) + d(image)),
  #"mod_lags_season_ext" = list( ~ 1 + lag_1 + lag_2 + lag_3
  #                            + lag_4 + lag_5 + lag_6
  #                            + lag_7 + lag_8 + lag_9
  #                            + season + d(image))
)

# ------------------------------- initialize models
mods <- list(c(1:length(mod_formulas)))

for (i in c(1:length(mod_formulas))) {
  assign(names(mod_formulas)[[i]],
         define_mod(mod_formulas[[i]], list(d = cnn_pic), data, y, "multinoulli"))
  mods[[i]] <- get(names(mod_formulas)[[i]])
}

# ------------------------------- do cv
for (i in c(1:(length(mod_formulas)))) {
  mod_tmp_name <- names(mod_formulas)[[i]]
  mod_tmp <- get(mod_tmp_name)
  print(mod_tmp_name)
  assign(
    paste0(mod_tmp_name, "_cv"),
    mod_tmp %>% cv(
      epochs = 100,
      cv_folds = indcs_final,
      shuffle = T,
      class_weight = list(class_weigths),
      batch_size = 32
    )
  )
}


# -------------------------------- do cv for one model
mod_img_lags_month_cv_single <- mod_img_lags_month %>% cv(
  epochs = 100,
  cv_folds = indcs_final,
  shuffle = T,
  class_weight = list(class_weigths),
  batch_size = 32
)

# --------------------------------  extract performance
mod_tmp_name <- "mod_img_lags_month_cv_single"
cv_result_tmp <- get(mod_tmp_name)
df_tmp_overview <- data.frame(matrix(NA, nrow = 50, ncol = 10))
names(df_tmp_overview) <- names(cv_result_tmp[[1]]$metrics)
plot_cv(cv_result_tmp)
# pro Metrik
for (metric in c(1:length(cv_result_tmp[[1]]$metrics))) {
  df_tmp_metric <- data.frame(matrix(NA, nrow = 10, ncol = 50))
  for (cv in c(1:10)){
    df_tmp_metric[cv, ] <- cv_result_tmp[[cv]]$metrics[[metric]]
  }
  df_tmp_overview[, metric] <- colMeans(df_tmp_metric)
}
saveRDS(cv_result_tmp, file = paste0("28_06_23-cv_shuffled", mod_tmp_name))
df_tmp_overview[which.min(df_tmp_overview$val_loss),]

 # ------------------------------- evaluate results
df_results <- data.frame(matrix(NA, nrow = length(mod_formulas), ncol = 10))
names(df_results) <- names(mod_lags_cv[[1]]$metrics)
row.names(df_results) <- names(mod_formulas)

for (i in c(1:(length(mod_formulas)))) {
  mod_tmp_name <- names(mod_formulas)[[i]]
  cv_result_tmp <- get(paste0(mod_tmp_name, "_cv"))
  #print(mod_tmp_name)
  df_tmp_overview <- data.frame(matrix(NA, nrow = 100, ncol = 10))
  names(df_tmp_overview) <- names(cv_result_tmp[[1]]$metrics)
  plot_cv(cv_result_tmp)
  # pro Metrik
  for (metric in c(1:length(cv_result_tmp[[1]]$metrics))) {
    df_tmp_metric <- data.frame(matrix(NA, nrow = 4, ncol = 100))
    for (cv in c(1:4)){
      df_tmp_metric[cv, ] <- cv_result_tmp[[cv]]$metrics[[metric]]
    }
    df_tmp_overview[, metric] <- colMeans(df_tmp_metric)
  }
  #print(df_tmp_overview)
  #print(df_tmp_overview[which.min(df_tmp_overview$val_loss),])
  saveRDS(cv_result_tmp, file = paste0("25_06_23-cv_shuffled", mod_tmp_name))
  df_results[i, ] <- df_tmp_overview[which.min(df_tmp_overview$val_loss),]
}

df_results

saveRDS(df_results, file = paste0("27_06_23-shuffled-cv-epoch-30_overview"))

# ------------------------------- read results
results <- readRDS("26_06_23-shuffled-cv-batch100_overview")
results
