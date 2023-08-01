### Info: Initializes models, does nested resampling and saves results into RDS files
### Called from: -

source("preprocessing.R")
source("splits.R")
source("nested_resampling.R")
set.seed(12345678)

# -------------------------------- Modelling functions
f1 <- function(y_true, y_pred) {
  true_positives <- k_sum(k_round(k_clip(y_true * y_pred, 0, 1)))

  possible_positives <- k_sum(k_round(k_clip(y_true, 0, 1)))
  predicted_positives <- k_sum(k_round(k_clip(y_pred, 0, 1)))

  precision <- true_positives / (predicted_positives + k_epsilon())
  recall <- true_positives / (possible_positives + k_epsilon())
  f1_val <-
    2 * (precision * recall) / (precision + recall + k_epsilon())
  return(f1_val)
}

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

define_cnn <- function(dropout_rate, channels, num_classes) {
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
    cnn2_pic() %>%
    layer_flatten() %>%
    layer_activation(activation = "relu") %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(num_classes)
  cnn_pic
}

# -------------------------------- Modelling prerequesits
## Define splits
splits <- create_splits_nested(df)
str(splits)

## Define callbacks
callbacks <- list()
callbacks <-
  append(callbacks, list(
    callback_early_stopping(
      patience = 6,
      restore_best_weights = TRUE,
      monitor = "val_loss"
    )
  ))

## Hypergrid for Gridsearch
hyper_grid <-
  expand.grid(learning_rate = c(0.0001, 0.001),
              batch_size  = c(64, 128))

tuning_archive <- data.frame(
  "tuning_iteration" = 1:(nrow(hyper_grid)),
  "learning_rate" = hyper_grid$learning_rate,
  "batch_size" = hyper_grid$batch_size,
  "avg_mf1" = 0,
  "avg_bal_acc" = 0
)

# -------------------------------- Model 1: Only images
mod_imgs <- deepregression(
    y = y,
    list_of_formulas = list(~ 1 + d(image)) ,
    family = "multinoulli",
    data = data,
    list_of_deep_models = list(d = define_cnn(0.2, 2, 7)),
    optimizer = optimizer_adam(learning_rate = 0.0001),
    monitor_metrics = list(
      f1,
      "categorical_accuracy",
      "categorical_crossentropy",
      tf$keras$metrics$AUC(multi_label = T)
    )
  )

  mod_imgs_outer_res <-
    nested_rsmp_final(mod_imgs,
                      splits,
                      tuning_archive,
                      df,
                      callbacks,
                      class_weights,
                      F,
                      F, 0)
saveRDS(mod_imgs_outer_res, "mod_imgs_outer_res")

# -------------------------------- Model 2: Images and month
mod_imgs_month <- deepregression(
     y = y,
     list_of_formulas = list( ~ 1 + month + d(image)) ,
     family = "multinoulli",
     data = data,
     list_of_deep_models = list(d = define_cnn(0.2, 2, 7)),
     optimizer = optimizer_adam(learning_rate = 0.0001),
     monitor_metrics = list(
       f1,
       "categorical_accuracy",
       "categorical_crossentropy",
       tf$keras$metrics$AUC(multi_label = T)
     )
   )

   mod_imgs_month_outer_res <-
     nested_rsmp_final(mod_imgs_month,
                       splits,
                       tuning_archive,
                       df,
                       callbacks,
                       class_weights,
                       F,
                       F, 0)
saveRDS(mod_imgs_month_outer_res, "mod_imgs_month_outer_res")

# -------------------------------- Model 3: Images and season
mod_imgs_season <- deepregression(
    y = y,
    list_of_formulas = list( ~ 1 + season + d(image)) ,
    family = "multinoulli",
    data = data,
    list_of_deep_models = list(d = define_cnn(0.2, 2, 7)),
    optimizer = optimizer_adam(learning_rate = 0.0001),
    monitor_metrics = list(
      f1,
      "categorical_accuracy",
      "categorical_crossentropy",
      tf$keras$metrics$AUC(multi_label = T)
    )
  )

  mod_imgs_season_outer_res <-
    nested_rsmp_final(mod_imgs_season,
                      splits,
                      tuning_archive,
                      df,
                      callbacks,
                      class_weights,
                      F,
                      F, 0)
saveRDS(mod_imgs_season_outer_res, "mod_imgs_season_outer_res")

# -------------------------------- Model 4: Images and year
mod_imgs_year <- deepregression(
  y = y,
  list_of_formulas = list( ~ 1 + s(year) + d(image)) ,
  family = "multinoulli",
  data = data,
  list_of_deep_models = list(d = define_cnn(0.2, 2, 7)),
  optimizer = optimizer_adam(learning_rate = 0.0001),
  monitor_metrics = list(
    f1,
    "categorical_accuracy",
    "categorical_crossentropy",
    tf$keras$metrics$AUC(multi_label = T)
  )
)

mod_imgs_year_outer_res <-
  nested_rsmp_final(mod_imgs_year,
                    splits,
                    tuning_archive,
                    df,
                    callbacks,
                    class_weights,
                    F,
                    F, 0)
saveRDS(mod_imgs_year_outer_res, "mod_imgs_year_outer_res")

# -------------------------------- Model 5: Images, season and year
 mod_imgs_season_year <- deepregression(
   y = y,
   list_of_formulas = list(~ 1 + s(year) + season + d(image)) ,
   family = "multinoulli",
   data = data,
   list_of_deep_models = list(d = define_cnn(0.2, 2, 7)),
   optimizer = optimizer_adam(learning_rate = 0.0001),
   monitor_metrics = list(
     f1,
     "categorical_accuracy",
     "categorical_crossentropy",
     tf$keras$metrics$AUC(multi_label = T)
   )
 )

 mod_imgs_season_year_outer_res <-
   nested_rsmp_final(
     mod_imgs_season_year,
     splits,
     tuning_archive,
     df,
     callbacks,
     class_weights,
     F,
     F,
     0
   )
saveRDS(mod_imgs_season_year_outer_res,
         "mod_imgs_season_year_outer_res")


# -------------------------------- Model 6: Images and lags
mod_imgs_lags <- deepregression(
    y = y,
     list_of_formulas = list( ~ 1 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + d(image)) ,
     family = "multinoulli",
     data = data,
     list_of_deep_models = list(d = define_cnn(0.2, 2, 7)),
     optimizer = optimizer_adam(learning_rate = 0.0001),
     monitor_metrics = list(
       f1,
       "categorical_accuracy",
       "categorical_crossentropy",
       tf$keras$metrics$AUC(multi_label = T)
          )
   )

   mod_imgs_lags_outer_res <-
     nested_rsmp_final(mod_imgs_lags,
                       splits,
                       tuning_archive,
                       df,
                       callbacks,
                       class_weights,
                       F,
                       T, 6)
saveRDS(mod_imgs_lags_outer_res, "mod_imgs_lags_outer_res")


# # -------------------------------- Model 7: images + lags + length
mod_imgs_lags_length <- deepregression(
   y = y,
   list_of_formulas = list(
     ~ 1 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + curr_length:lag_1 + d(image)
   ) ,
   family = "multinoulli",
   data = data,
   list_of_deep_models = list(d = define_cnn(0.2, 2, 7)),
   optimizer = optimizer_adam(learning_rate = 0.0001),
   monitor_metrics = list(
     f1,
     "categorical_accuracy",
     "categorical_crossentropy",
     tf$keras$metrics$AUC(multi_label = T)
   )
 )

mod_imgs_lags_length_outer_res <-
   nested_rsmp_final(mod_imgs_lags_length,
                     splits,
                     tuning_archive,
                     df,
                     callbacks,
                     class_weights,
                     T,
                     T, 6)
saveRDS(mod_imgs_lags_length_outer_res,
         "mod_imgs_lags_length_outer_res")

# -------------------------------- Model 7: Images, lags and year
mod_imgs_lags_year <- deepregression(
   y = y,
   list_of_formulas = list( ~ 1 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + s(year) + d(image)) ,
  family = "multinoulli",
   data = data,
   list_of_deep_models = list(d = define_cnn(0.2, 2, 7)),
   optimizer = optimizer_adam(learning_rate = 0.0001),
   monitor_metrics = list(
     f1,
     "categorical_accuracy",
     "categorical_crossentropy",
     tf$keras$metrics$AUC(multi_label = T)
   )
 )

mod_imgs_lags_year_outer_res <-
   nested_rsmp_final(mod_imgs_lags_year,
                     splits,
                     tuning_archive,
                     df,
                     callbacks,
                     class_weights,
                     F,
                     T, 6)
 saveRDS(mod_imgs_lags_year_outer_res, "mod_imgs_lags_year_outer_res")


# -------------------------------- Model: Images, Images-1
mod_imgs_imgs_lagged <- deepregression(
  y = y,
  list_of_formulas = list(~ 1 + d(image) + d1(image_lagged)) ,
  family = "multinoulli",
  data = data,
  list_of_deep_models = list(d = define_cnn(0.2, 2, 7), d1 = define_cnn(0.2, 2, 7)),
  optimizer = optimizer_adam(learning_rate = 0.0001),
  monitor_metrics = list(
    f1,
    "categorical_accuracy",
    "categorical_crossentropy",
    tf$keras$metrics$AUC(multi_label = T)
  )
)

mod_imgs_imgs_lagged_outer_res <-
  nested_rsmp_final(mod_imgs_imgs_lagged,
                        splits,
                        tuning_archive,
                        df,
                        callbacks,
                        class_weights,
                        F,
                        F,
                        0)
saveRDS(mod_imgs_imgs_lagged_outer_res,
        "mod_imgs_imgs_lagged_outer_res")

# -------------------------------- Model 8: Images, Images-1, Images + 1
mod_imgs_imgs_lagged_lead <- deepregression(
     y = y,
      list_of_formulas = list( ~ 1 + d(image) + d2(image_lagged) + d3(image_lead)) ,
      family = "multinoulli",
      data = data,
     list_of_deep_models = list(d = define_cnn(0.2, 2, 7), d2 = define_cnn(0.2, 2, 7), d3 = define_cnn(0.2, 2, 7)),
      optimizer = optimizer_adam(learning_rate = 0.0001),
      monitor_metrics = list(
        f1,
        "categorical_accuracy",
        "categorical_crossentropy",
         tf$keras$metrics$AUC(multi_label = T)
      )
    )

    mod_imgs_imgs_lagged_lead_outer_res <-
      nested_rsmp_final(mod_imgs_imgs_lagged_lead,
                            splits,
                            tuning_archive,
                            df,
                            callbacks,
                           class_weights,
                           F,
                           F, 0)

saveRDS(mod_imgs_imgs_lagged_lead_outer_res, "mod_imgs_imgs_lagged_lead_outer_res")

# -------------------------------- Model 8: Images, Images + 1
mod_imgs_imgs_lead <- deepregression(
      y = y,
      list_of_formulas = list( ~ 1 + d(image) + d1(image_lead)) ,
      family = "multinoulli",
      data = data,
      list_of_deep_models = list(d = define_cnn(0.2, 2, 7), d1 = define_cnn(0.2, 2, 7)),
      optimizer = optimizer_adam(learning_rate = 0.0001),
      monitor_metrics = list(
        f1,
        "categorical_accuracy",
        "categorical_crossentropy",
        tf$keras$metrics$AUC(multi_label = T)
      )
    )

    mod_imgs_imgs_lead_outer_res <-
      nested_rsmp_final(mod_imgs_imgs_lead,
                            splits,
                            tuning_archive,
                            df,
                            callbacks,
                            class_weights,
                            F,
                            F, 0)
saveRDS(mod_imgs_imgs_lead_outer_res, "mod_imgs_imgs_lead_outer_res")
