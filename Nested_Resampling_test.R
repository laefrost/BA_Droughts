library(mgcv)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(nnet)
library(deepregression)
library(tidyr)
library(longCatEDA)
library(lattice)
library(caret)
library(deepregression)
library(purrr)
set.seed(123456)
source("splits.R")
source("preprocessing.R")

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


define_mod <- function(list_formula_structured,
                       learning_rate = 0.00123,
                       dropout_rate = 0.2,
                       features = data,
                       target = y,
                       channels = 2) {
  tmp_dr <- deepregression(
    y = target,
    list_of_formulas = list_formula_structured,
    family = "multinoulli",
    data = features,
    list_of_deep_models = list(d = define_cnn(dropout_rate, channels, 7)),
    optimizer = optimizer_adam(learning_rate = learning_rate),
    monitor_metrics = list(
      f1,
      "categorical_accuracy",
      "categorical_crossentropy",
      tf$keras$metrics$AUC(multi_label = T)
    )
  )
}


create_splits_v2 <- function(indcs, window, horizon, skip) {
  indcs_tmp <-
    createTimeSlices(
      indcs,
      initialWindow = window,
      horizon = horizon,
      skip = skip,
      fixedWindow = T
    )
  # create list of
  indcs <- vector(mode = "list", length(indcs_tmp))
  for (i in c(1:length(indcs_tmp[[1]]))) {
    # indices for outer resampling procedure
    indcs_train <- indcs_tmp$train[[i]][1:floor(0.8 * window)]
    indcs_val <- setdiff(indcs_tmp$train[[i]], indcs_train)
    indcs[[i]] <- list(indcs_train, indcs_val, indcs_tmp$test[[i]])
  }
  indcs
}

replace_na <- function(x) {
  x[is.na(x)] <- 0
  x
}


nested_rspm <-
  function(x,
           data,
           lags_bool,
           tuning_archive,
           df,
           mod_name) {
    # create indices for train/test splits
    splits <- create_splits_nested(df)

    # only tmp
    indcs_outer <- list(splits[[1]][[5]])
    indcs_inner <- splits[[2]][[5]]

    str((indcs_outer))
    str(indcs_inner)

    og_weights <- x$model$get_weights()

    list_cms_wo <- list(1:length(indcs_outer))
    list_cms_wi <- list(1:length(indcs_outer))

    x_train <-
      deepregression:::prepare_data(
        x$init_params$parsed_formulas_content,
        gamdata = x$init_params$gamdata$data_trafos
      )
    callbacks <- list()
    callbacks <-
      append(callbacks, list(
        callback_early_stopping(
          patience = 6,
          restore_best_weights = TRUE,
          monitor = "val_loss"
        )
      ))
    # do outer resampling loop
    for (outer in c(1:length(indcs_outer))) {
      print(paste(
        "Outer split ------------------------------------------ ",
        outer
      ))
      train_indcs_outer <- indcs_outer[[outer]][[1]]
      val_indcs_outer <- indcs_outer[[outer]][[2]]
      test_indcs_outer <- indcs_outer[[outer]][[3]]

      train_data_outer <-
        lapply(x_train, function(x)
          deepregression:::subset_array(x, train_indcs_outer))
      test_data_outer <-
        lapply(data, function(x)
          deepregression:::subset_array(x, test_indcs_outer))
      val_data_outer <-
        lapply(x_train, function(x)
          deepregression:::subset_array(x, val_indcs_outer))

      fold_mod <- x$model
      fold_mod$set_weights(og_weights)

      for (hps in 1:nrow(tuning_archive)) {
        print(paste(
          "Tuning --------------------------------------------",
          hps
        ))
        tune_mod <- x$model
        tune_mod$set_weights(og_weights)
        tune_learning_rate <- tuning_archive[hps, "learning_rate"]
        losses <- c(1:length(indcs_inner))
        for (inner in c(1:length(indcs_inner))) {
          print(paste(
            "Inner split ------------------------------------------ ",
            inner
          ))
          # train_indcs_inner <- indcs_inner[[outer]][[inner]][[1]]
          # val_indcs_inner <- indcs_inner[[outer]][[inner]][[2]]
          # test_indcs_inner <- indcs_inner[[outer]][[inner]][[3]]
          train_indcs_inner <- indcs_inner[[inner]][[1]]
          val_indcs_inner <- indcs_inner[[inner]][[2]]
          test_indcs_inner <- indcs_inner[[inner]][[3]]

          # create train/val/test data
          train_data_inner <- lapply(x_train, function(x)
            deepregression:::subset_array(x, train_indcs_inner))
          val_data_inner <- lapply(x_train, function(x)
            deepregression:::subset_array(x, val_indcs_inner))
          test_data_inner <- lapply(data, function(x)
            deepregression:::subset_array(x, test_indcs_inner))

          args_tune <- list()
          args_tune <- append(
            args_tune,
            list(
              object = tune_mod,
              x = train_data_inner,
              y = deepregression:::subset_array(x$init_params$y, train_indcs_inner),
              validation_split = NULL,
              validation_data = list(
                val_data_inner,
                deepregression:::subset_array(x$init_params$y, val_indcs_inner)
              ),
              callbacks = callbacks,
              verbose = F,
              view_metrics = FALSE
            )
          )

          ellipsis <- x$init_params$ellipsis
          ellipsis$optimizer$learning_rate$assign(tune_learning_rate)
          args_tune <- append(args_tune, ellipsis)

          ret_tune <- do.call(x$fit_fun, args_tune)
          tune_mod$set_weights(og_weights)
          losses[inner] <-
            ret_tune$metrics$val_loss[length(ret_tune$metrics$val_loss)]
        }
        print(losses)
        tuning_archive[hps,] <-
          c(
            hps,
            tuning_archive$learning_rate[hps],
            tuning_archive$dropout_rate[hps],
            mean(losses)
          )
      }

      learning_rate <-
        tuning_archive$learning_rate[which.min(tuning_archive$val_loss)]
      drop_out <-
        tuning_archive$dropout_rate[which.min(tuning_archive$val_loss)]

      args <- list()
      args <- append(
        args,
        list(
          object = fold_mod,
          x = train_data_outer,
          y = deepregression:::subset_array(x$init_params$y, train_indcs_outer),
          validation_split = NULL,
          validation_data = list(
            val_data_outer,
            deepregression:::subset_array(x$init_params$y, val_indcs_outer)
          ),
          callbacks = callbacks,
          verbose = F,
          view_metrics = FALSE,
          #early_stopping = T,
          epochs = 35,
          #shuffle = T,
          class_weight = list(class_weigths),
          batch_size = 128
        )
      )

      ellipsis <- x$init_params$ellipsis
      ellipsis$optimizer$learning_rate$assign(learning_rate)
      args <- append(args, ellipsis)

      ret_outer <- do.call(x$fit_fun, args)
      png(filename = paste0(mod_name, outer, ".png"))
      plot(ret_outer)
      dev.off()
      predictions_outer_wo_rolling <-
        x %>% predict(newdata = test_data_outer)

      colnames(predictions_outer_wo_rolling) <-
        c("other", "BM" ,   "HFA"   , "HNA"  , "HNFA",  "NEA"  , "SEA")

      predicted_classes_wo <-
        colnames(predictions_outer_wo_rolling)[max.col(predictions_outer_wo_rolling)]
      predicted_classes_wo <-
        factor(predicted_classes_wo, levels = levels(df$value))
      list_cms_wo[[outer]] <-
        confusionMatrix(predicted_classes_wo, df$value[test_indcs_outer])
      print(list_cms_wo[[outer]])
      saveRDS(list_cms_wo[[outer]], file = paste0(mod_name, "_", outer, "_cm_wo"))

      if (lags_bool) {
        predictions_wi_outer_rolling <- predictions_outer_wo_rolling
        colnames(predictions_wi_outer_rolling) <-
          c("other", "BM" ,   "HFA"   , "HNA"  , "HNFA",  "NEA"  , "SEA")
        for (row in 1:nrow(predictions_wi_outer_rolling)) {
          predictions_wi_outer_rolling[row, ] <-
            x %>% predict(newdata = lapply(test_data_outer, function(x)
              deepregression:::subset_array(x, row:row)))
          predicted_class <-
            names(which.max(predictions_wi_outer_rolling[row, ]))
          for (lag in 1:6) {
            if (lag + row <= nrow(predictions_wi_outer_rolling)) {
              test_data_outer[paste0("lag_", lag)][[1]][row + lag] <-
                as.factor(predicted_class)
            }
          }
        }
        predicted_classes_wi <-
          colnames(predictions_wi_outer_rolling)[max.col(predictions_wi_outer_rolling)]
        predicted_classes_wi <-
          factor(predicted_classes_wi, levels = levels(df$value))
        list_cms_wi[[outer]] <-
          confusionMatrix(predicted_classes_wi, df$value[test_indcs_outer])
        print(list_cms_wi[[outer]])
        saveRDS(list_cms_wi[[outer]], file = paste0(mod_name, "_", outer, "_cm_wi"))
      }

      fold_mod$set_weights(og_weights)
    }
    return(list(list_cms_wo, list_cms_wi))
  }

hyper_grid <-
  expand.grid(learning_rate = c(0.0001, 0.0005, 0.001),
              dropout_rate  = c(0.25))

tuning_archive <- data.frame(
  "tuning_iteration" = 1:(nrow(hyper_grid)),
  "learning_rate" = hyper_grid$learning_rate,
  "dropout_rate" = hyper_grid$dropout_rate,
  "val_loss" = 0
)


# ------------------------------- define mods and do PE
splits_helper <- create_splits_nested(df)
str(indcs_helper)
indcs_helper <- splits_helper[[1]][[3]]
indcs_helper_train <- indcs_helper[[1]]
indcs_helper_val <- indcs_helper[[2]]
indcs_helper_test <- indcs_helper[[3]]
test_data <- lapply(data, function(x) deepregression:::subset_array(x, indcs_helper_test))

### only images
# mod_img <-
#   define_mod(
#     list_formula_structured = list(~ 1 + d(image)),
#     features = data,
#     target = y,
#     channels = 2
#   )
# nr_mod_img <-
#   nested_rspm(
#     x = mod_img,
#     data = data,
#     lags_bool = F,
#     tuning_archive = tuning_archive,
#     df = df,
#     "mod_imgs"
#   )

# mod_img <- deepregression(
#   y = y[indcs_helper_train, ],
#   list_of_formulas = list(~ 1 + d(image)),
#   family = "multinoulli",
#   data = lapply(data, function(x)
#     deepregression:::subset_array(x, indcs_helper_train)),
#   list_of_deep_models = list(d = define_cnn(0.25, 2, 7)),
#   optimizer = optimizer_adam(learning_rate = 0.0001),
#   monitor_metrics = list(
#     f1,
#     "categorical_accuracy",
#     "categorical_crossentropy",
#     tf$keras$metrics$AUC(multi_label = T)
#   )
# )

mod_img <- deepregression(
  y = y,
  list_of_formulas = list(~ 1 + d(image)),
  family = "multinoulli",
  data = data,
  list_of_deep_models = list(d = define_cnn(0.25, 2, 7)),
  optimizer = optimizer_adam(learning_rate = 0.0001),
  monitor_metrics = list(
    f1,
    "categorical_accuracy",
    "categorical_crossentropy",
    tf$keras$metrics$AUC(multi_label = T)
  )
)

#weigths = list("0" = 1, "1" = 10000000, "2" = 1, "3" = 1, "4" = 1, "5" = 1, "6" = 1)
#levels(data$gwl)
class_weights
ow <- mod_img$model$get_weights()
modi <- mod_img$model

x_train <- deepregression:::prepare_data(
  mod_img$init_params$parsed_formulas_content,
  gamdata = mod_img$init_params$gamdata$data_trafos
)

train_data <- lapply(x_train, function(x)
  deepregression:::subset_array(x, indcs_helper_train))
val_data <- lapply(x_train, function(x)
  deepregression:::subset_array(x, indcs_helper_val))
test_data <- lapply(data, function(x)
  deepregression:::subset_array(x, indcs_helper_test))


args <- list()
args <- append(
  args,
  list(
    object = modi,
    x = train_data,
    y = deepregression:::subset_array(mod_img$init_params$y, indcs_helper_train),
    validation_split = NULL,
    validation_data = list(
      val_data,
      deepregression:::subset_array(mod_img$init_params$y, indcs_helper_val)
    ),
    callbacks = callbacks,
    verbose = F,
    view_metrics = FALSE,
    class_weight = class_weights,
    epochs = 50,
    batch_size = 32
  )
)

ellipsis <- mod_img$init_params$ellipsis
ellipsis$optimizer$learning_rate$assign(learning_rate)
args <- append(args, ellipsis)

mod_img$model$optimizer$

hist_img <- mod_img %>% fit(epochs = 50, batch_size = 64, early_stopping = T, patience = 10, class_weight = class_weights,
                            validation_data = list(
                              lapply(data, function(x)
                                deepregression:::subset_array(x, indcs_helper_val)),
                              y[indcs_helper_val, ]))
plot(hist_img)
test_data <- lapply(data, function(x)
  deepregression:::subset_array(x, indcs_helper_test))
str(test_data)
predictions_img <- mod_img %>%  predict(test_data)
colnames(predictions_img) <- c("other", "BM" ,   "HFA"   , "HNA"  , "HNFA",  "NEA"  , "SEA")

pc_imgs <- colnames(predictions_img)[max.col(predictions_img)]
pc_imgs <- factor(pc_imgs, levels = levels(df$value))
cm_img <- confusionMatrix(pc_imgs, df$value[indcs_helper_test])
















### -------------- lags and images
mod_img_lags <- deepregression(
  y = y[indcs_helper_train, ],
  list_of_formulas = list( ~ 1 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + curr_length + d(image)),
  family = "multinoulli",
  data = lapply(data, function(x)
    deepregression:::subset_array(x, indcs_helper_train)),
  list_of_deep_models = list(d = define_cnn(0.25, 2, 7)),
  optimizer = optimizer_adam(learning_rate = 0.0001),
  monitor_metrics = list(
    f1,
    "categorical_accuracy",
    "categorical_crossentropy",
    tf$keras$metrics$AUC(multi_label = T)
  )
)

hist_img_lags <-
  mod_img_lags %>% fit(
    epochs = 50,
    batch_size = 32,
    early_stopping = T,
    patience = 6,
    validation_data = list(
      lapply(data, function(x)
        deepregression:::subset_array(x, indcs_helper_val)),
      y[indcs_helper_val, ],
      view_metrics = T
    ),
    class_weight = class_weights
  )
plot(hist_img_lags)

levels(test_data$gwl)

predictions_img_lags_new <- mod_img_lags %>%  predict(test_data)
colnames(predictions_img_lags_new) <- c("other", "BM" , "HFA" , "HNA"  , "HNFA",  "NEA", "SEA")
pc_imgs_lags_new <- colnames(predictions_img_lags_new)[max.col(predictions_img_lags_new)]
pc_imgs_lags_new <- factor(pc_imgs_lags_new  ,levels = levels(df$value))
cm_img_lags_new <- confusionMatrix(pc_imgs_lags_new, df$value[indcs_helper_test])




# predictions_img_lags <- mod_img_lags %>%  predict(test_data)
# colnames(predictions_img_lags) <- c("other", "BM" , "HFA" , "HNA"  , "HNFA",  "NEA", "SEA")
# pc_imgs_lags <- colnames(predictions_img_lags)[max.col(predictions_img_lags)]
# pc_imgs_lags <- factor(pc_imgs_lags  ,levels = levels(df$value))
# cm_img_lags <- confusionMatrix(pc_imgs_lags, df$value[indcs_helper_test])

test_data_wi_new <- test_data
levels(test_data_wi$gwl)

predictions_img_lags_wi_new <- predictions_img_lags_new
colnames(predictions_img_lags_wi_new) <- c("other", "BM" ,   "HFA"   , "HNA"  , "HNFA",  "NEA"  , "SEA")
pc_imgs_lags_wi_new <- pc_imgs_lags_new
for (row in 1:nrow(predictions_img_lags_wi_new)) {
  #print(row)
  dp <- lapply(test_data_wi_new, function(x) deepregression:::subset_array(x, row))
  # print(dp$lag_1)
  # print(dp$lag_2)
  # print(dp$lag_3)
  # print(dp$lag_4)
  # print(dp$curr_length)
  predictions_img_lags_wi_new[row, ] <- mod_img_lags %>% predict(dp)
  predicted_class_new <- names(which.max(predictions_img_lags_wi_new[row, ]))
  pc_imgs_lags_wi_new[row] <- predicted_class_new
  #print(paste("predicted_class", predicted_class_new))
  if (row > 1 ){
  if(predicted_class_new == pc_imgs_lags_wi_new[row-1]) {
    #print(paste("found nooooooooooooooooo difference: ",  pc_imgs_lags_wi[row], pc_imgs_lags_wi[row-1]))
    test_data_wi_new$curr_length[row] <- test_data_wi_new$curr_length[[row - 1]] + 1
    #print(paste("currrrent length = ", test_data_wi$curr_length[row], "former length:", test_data_wi$curr_length[[row - 1]]))
  }
  else {
    #print(paste("found a difference: ",  pc_imgs_lags_wi[row], pc_imgs_lags_wi[row-1]))
    test_data_wi_new$curr_length[row] <- 1
    #print(paste("currrrent length = ", test_data_wi$curr_length[row], "former length:", test_data_wi$curr_length[[row - 1]]))
  }
  }
  for (lag in 1:6) {
    if (lag + row <= nrow(predictions_img_lags_wi_new)) {
      test_data_wi_new[paste0("lag_", lag)][[1]][row + lag] <-
        as.factor(predicted_class_new)
    }
  }
}
predicted_classes_wi_img_lags_new <- colnames(predictions_img_lags_wi_new)[max.col(predictions_img_lags_wi_new)]
predicted_classes_wi_img_lags_new <- factor(predicted_classes_wi_img_lags_new, levels = levels(df$value))
cm_img_lags_wi_new <- confusionMatrix(pc_imgs_lags_wi_new, df$value[indcs_helper_test])



cm_img_lags_old <- confusionMatrix(pc_imgs_lags, df$value[indcs_helper_test])
cm_img_lags_wi_old <- confusionMatrix(pc_imgs_lags_wi, df$value[indcs_helper_test])



predict_rolling <- function() {

}



log(unlist(class_weights))







### -------------- only lags
mog_lags <- deepregression(
  y = y[indcs_helper_train,],
  list_of_formulas = list(~ 1 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6),
  family = "multinoulli",
  data = lapply(data, function(x) deepregression:::subset_array(x, indcs_helper_train)),
  list_of_deep_models = list(d = define_cnn(0.25, 2, 7)),
  optimizer = optimizer_adam(learning_rate = 0.0001),
  monitor_metrics = list(
    f1,
    "categorical_accuracy",
    "categorical_crossentropy",
    tf$keras$metrics$AUC(multi_label = T)
  )
)

hist_lags <- mog_lags %>% fit(epochs = 50, batch_size = 128, early_stopping = T, patience = 6,
                                      validation_data = list(
                                        lapply(data, function(x) deepregression:::subset_array(x, indcs_helper_val)),
                                        y[indcs_helper_val,]
                                      ))
plot(hist_lags)
predictions_lags <- mog_lags %>%  predict(new_data = lapply(data, function(x)
  deepregression:::subset_array(x, c(indcs_helper_test))))
colnames(predictions_lags) <- c("other", "BM" ,   "HFA"   , "HNA"  , "HNFA",  "NEA"  , "SEA")
str(indcs_helper_val)
nrow(predictions_lags)
pc_lags <- colnames(predictions_lags)[max.col(predictions_lags)]
pc_lags <- factor(pc_lags  ,levels = levels(df$value))
cm_lags <- confusionMatrix(pc_lags, df$value[indcs_helper_test])
length(df$value[indcs_helper_test])


### -------------- lagged images
mod_img_lagged <-
  deepregression(
    y = y_combined[indcs_helper_train,],
    list_of_formulas = list(~ 1 + d(image_extended)),
    family = "multinoulli",
    data = lapply(data_combined, function(x) deepregression:::subset_array(x, indcs_helper_train)),
    list_of_deep_models = list(d = define_cnn(0.25, 8, 7)),
    optimizer = optimizer_adam(learning_rate = 0.0001),
    monitor_metrics = list(
      f1,
      "categorical_accuracy",
      "categorical_crossentropy",
      tf$keras$metrics$AUC(multi_label = T)
    )
  )

hist_img_lagged <- mod_img_lagged %>% fit(epochs = 50, batch_size = 128, early_stopping = T, patience = 6,
                              validation_data = list(
                                lapply(data_combined, function(x) deepregression:::subset_array(x, indcs_helper_val)),
                                y_combined[indcs_helper_val,]
                              ))

predictions_img_lagged <- mod_lags %>%  predict(new_data = lapply(data_combined, function(x)
  deepregression:::subset_array(x, indcs_helper_test)))
colnames(predictions_img_lagged) <- c("other", "BM" ,   "HFA"   , "HNA"  , "HNFA",  "NEA"  , "SEA")
pc_img_lagged <- colnames(predictions_img_lagged)[max.col(predictions_img_lagged)]
cm_img_lagged <- confusionMatrix(pc_img_lagged, df_combined$value[indcs_helper_test])





































# ------------------------------- define formulas for struct. part
mod_formulas <- list(
  "mod_img" = list(~ 1 + d(image)),
  #"mod_year" = list(~ 1 + s(year)),
  "mod_lags" = list(~ 1 + lag_1 + lag_2 + lag_3
                    + lag_4 + lag_5 + lag_6),
  #"mod_season" = list( ~ 1 + season),
  #"mod_img_date" = list( ~ 1 + s(date_numeric) + d(image)),
  #"mod_img_month" = list( ~ 1 + s(month) + d(image)),
  #"mod_img_lags" = list( ~ 1 + lag_1 + lag_2 + lag_3
  #                   + lag_4 + lag_5 + lag_6
  #                   + d(image)),
  #"mod_img_season" = list( ~ 1 + season + d(image)),
  #"mod_img_year" = list(~ 1 + s(year) + d(image)),
  "mod_img_lags_month" = list(~ 1 + lag_1 + lag_2 + lag_3
                              + lag_4 + lag_5 + lag_6 + s(month) + d(image)),
  "mod_img_lags_season" = list(~ 1 + lag_1 + lag_2 + lag_3
                               + lag_4 + lag_5 + lag_6  + season + d(image)),
  "mod_img_lags_year" = list(~ 1 +  lag_1 + lag_2 + lag_3
                             + lag_4 + lag_5 + lag_6  + s(year) + d(image)),
  "mod_img_lags_year_season" = list(
    ~ 1 +  lag_1 + lag_2 + lag_3
    + lag_4 + lag_5 + lag_6  + s(year) + season + d(image)
  )
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
x <- define_mod(mod_formulas[[2]], channels = 2)


mods <- list(c(1:length(mod_formulas)))

for (i in c(1:length(mod_formulas))) {
  assign(names(mod_formulas)[[i]],
         define_mod(mod_formulas[[i]], channels = 2))
  mods[[i]] <- get(names(mod_formulas)[[i]])
}

str(data_combined)
str(y_combined)
# ------------------------------- do nested resampling
form_lagged_images <-
  list(~ 1 + s(year) + season + d(image_extended))
test_lagged_images <-
  define_mod(
    form_lagged_images,
    features = data_combined,
    target = y_combined ,
    channels = 8
  )
str(data_combined)
test_nested <-
  nested_rspm(test_lagged_images, data = data_combined, F, tuning_archive)

t1 <-
  Reduce('+', lapply(test_nested, function(x)
    replace_na(x$table))) / length(indcs_outer)
t2 <-
  Reduce('+', lapply(test_nested, function(x)
    replace_na(x$byClass))) / length(indcs_outer)

hist <- test_lagged_images %>% fit()
plot(hist)

nested_rsmp_imgs <- nested_rspm(mods[[1]], data, F, tuning_archive)
nested_rsmp_lags <- nested_rspm(mods[[2]], data, T, tuning_archive)
nested_rsmp_imgs_lags_month <-
  nested_rspm(mods[[3]], data, T, tuning_archive)
nested_rsmp_imgs_lags_season <-
  nested_rspm(mods[[4]], data, T, tuning_archive)
nested_rsmp_imgs_lags_year <-
  nested_rspm(mods[[5]], data, T, tuning_archive)
nested_rsmp_imgs_lags_year_season <-
  nested_rspm(mods[[6]], data, T, tuning_archive)

nested_rsmp_lags_season[[1]]
str(mods)

cm_avg_table_wo_imgs <-
  Reduce('+', lapply(nested_rsmp_imgs[[1]], function(x)
    replace_na(x$table))) / length(indcs_outer)
saveRDS(cm_avg_table_wo_imgs, file = paste0("cm_avg_table_wo_imgs"))
cm_avg_byClass_wo_imgs <-
  Reduce('+', lapply(nested_rsmp_imgs[[1]], function(x)
    replace_na(x$byClass))) / length(indcs_outer)
saveRDS(cm_avg_byClass_wo_imgs, file = paste0("cm_avg_byClass_wo_imgs"))

readRDS("cm_avg_table_wo_imgs")


cm_avg_table_wo_lags <-
  Reduce('+', lapply(nested_rsmp_lags[[1]], function(x)
    replace_na(x$table))) / length(indcs_outer)
saveRDS(cm_avg_table_wo_lags, file = paste0("cm_avg_table_wo_lags"))

cm_avg_table_wi_lags <-
  Reduce('+', lapply(nested_rsmp_lags[[2]], function(x)
    replace_na(x$table))) / length(indcs_outer)
saveRDS(cm_avg_table_wi_lags, file = paste0("cm_avg_table_wi_lags"))

cm_avg_byClass_wo_lags <-
  Reduce('+', lapply(nested_rsmp_lags[[1]], function(x)
    replace_na(x$byClass))) / length(indcs_outer)
saveRDS(cm_avg_byClass_wo_lags, file = paste0("cm_avg_byClass_wo_lags"))

cm_avg_byClass_wi_lags <-
  Reduce('+', lapply(nested_rsmp_lags[[2]], function(x)
    replace_na(x$byClass))) / length(indcs_outer)
saveRDS(cm_avg_byClass_wi_lags, file = paste0("cm_avg_byClass_wi_lags"))



cm_avg_table_wo_lags_month <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_month[[1]], function(x)
           replace_na(x$table))) / length(indcs_outer)
saveRDS(cm_avg_table_wo_lags_month,
        file = paste0("cm_avg_table_wo_lags_month"))
cm_avg_table_wi_lags_month <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_month[[2]], function(x)
           replace_na(x$table))) / length(indcs_outer)
saveRDS(cm_avg_table_wi_lags_month,
        file = paste0("cm_avg_table_wi_lags_month"))
cm_avg_byClass_wo_lags_month <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_month[[1]], function(x)
           replace_na(x$byClass))) / length(indcs_outer)
saveRDS(cm_avg_byClass_wo_lags_month,
        file = paste0("cm_avg_byClass_wo_lags_month"))
cm_avg_byClass_wi_lags_month <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_month[[2]], function(x)
           replace_na(x$byClass))) / length(indcs_outer)
saveRDS(cm_avg_byClass_wi_lags_month,
        file = paste0("cm_avg_byClass_wi_lags_month"))

cm_avg_table_wo_lags_season <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_season[[1]], function(x)
           replace_na(x$table))) / length(indcs_outer)
saveRDS(cm_avg_table_wo_lags_season,
        file = paste0("cm_avg_table_wo_lags_season"))
cm_avg_table_wi_lags_season <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_season[[2]], function(x)
           replace_na(x$table))) / length(indcs_outer)
saveRDS(cm_avg_table_wi_lags_season,
        file = paste0("cm_avg_table_wi_lags_season"))
cm_avg_byClass_wo_lags_season <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_season[[1]], function(x)
           replace_na(x$byClass))) / length(indcs_outer)
saveRDS(cm_avg_byClass_wo_lags_season,
        file = paste0("cm_avg_byClass_wo_lags_season"))
cm_avg_byClass_wi_lags_season <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_season[[2]], function(x)
           replace_na(x$byClass))) / length(indcs_outer)
saveRDS(cm_avg_byClass_wi_lags_season,
        file = paste0("cm_avg_byClass_wi_lags_season"))

cm_avg_table_wo_lags_year <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_year[[1]], function(x)
           replace_na(x$table))) / length(indcs_outer)
saveRDS(cm_avg_table_wo_lags_year,
        file = paste0("cm_avg_table_wo_lags_year"))
cm_avg_table_wi_lags_year <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_year[[2]], function(x)
           replace_na(x$table))) / length(indcs_outer)
saveRDS(cm_avg_table_wi_lags_year,
        file = paste0("cm_avg_table_wi_lags_year"))
cm_avg_byClass_wo_lags_year <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_year[[1]], function(x)
           replace_na(x$byClass))) / length(indcs_outer)
saveRDS(cm_avg_byClass_wo_lags_year,
        file = paste0("cm_avg_byClass_wo_lags_year"))
cm_avg_byClass_wi_lags_year <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_year[[2]], function(x)
           replace_na(x$byClass))) / length(indcs_outer)
saveRDS(cm_avg_byClass_wi_lags_year,
        file = paste0("cm_avg_byClass_wi_lags_year"))

cm_avg_table_wo_lags_year_s <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_year_season[[1]], function(x)
           replace_na(x$table))) / length(indcs_outer)
saveRDS(cm_avg_table_wo_lags_year_s,
        file = paste0("cm_avg_table_wo_lags_year_s"))
cm_avg_table_wi_lags_year_s <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_year_season[[2]], function(x)
           replace_na(x$table))) / length(indcs_outer)
saveRDS(cm_avg_table_wi_lags_year_s,
        file = paste0("cm_avg_table_wi_lags_year_s"))
cm_avg_byClass_wo_lags_year_s <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_year_season[[1]], function(x)
           replace_na(x$byClass))) / length(indcs_outer)
saveRDS(cm_avg_byClass_wo_lags_year_s,
        file = paste0("cm_avg_byClass_wo_lags_year_s"))
cm_avg_byClass_wi_lags_year_s <-
  Reduce('+',
         lapply(nested_rsmp_imgs_lags_year_season[[2]], function(x)
           replace_na(x$byClass))) / length(indcs_outer)
saveRDS(cm_avg_byClass_wi_lags_year_s,
        file = paste0("cm_avg_byClass_wi_lags_year_s"))

result_before <- readRDS("cm_avg_byClass_wi_lags_year_s")
result_before2 <- readRDS("cm_avg_byClass_wo_lags_year_s")

#  nested_rsmp <- function(list_structured) {
#   # create indices for train/test splits
#   indcs_outer <- create_splits(c(1:nrow(df)), 20 * 365, 4 * 365, 24 * 365)
#   indcs_inner <- vector(mode = "list", 3)
#
#   # do outer resampling loop
#   for (outer in c(1:length(indcs_outer))) {
#     train_data_outer <-
#       lapply(data, function(x)
#         deepregression:::subset_array(x, indcs_outer[[outer]][[1]]))
#     test_data_outer <-
#       lapply(data, function(x)
#         deepregression:::subset_array(x, indcs_outer[[outer]][[2]]))
#     indcs_inner <-
#       create_splits(indcs_outer[[outer]][[1]], 5 * 365, 365, 6 * 365)
#     # do inner resampling loop per hp
#     for (hps in 1:nrow(tuning_archive)) {
#        inner_mod <-
#          define_mod(
#            list_structured,
#            tuning_archive$learning_rate[1],
#            tuning_archive$dropout_rate[1],
#            data,
#            y
#          )
#        # do CV for each HP comb.
#       cv_inner_mod <- inner_mod %>% cv(
#          epochs = 400,
#          cv_folds = indcs_inner,
#          shuffle = T,
#          class_weight = list(class_weigths),
#          batch_size = 32,
#          early_stopping = T
#        )
#       stop_iter_cv_result(cv_inner_mod)
#       # extract best Parameters over epoch
#       avg_inner_cv_res <- rowMeans(as.data.frame(lapply(cv_inner_mod, function(cv_fold) cv_fold$metrics$val_loss)))
#       # get the row == epoch where val_loss is minimal
#       min_val_loss <- min(avg_inner_cv_res)
#       tuning_archive[hps, ] <- c(hps, tuning_archive$learning_rate[hps], tuning_archive$dropout_rate[hps], min_val_loss)
#     }
#     learning_rate <- tuning_archive$learning_rate[which.min(tuning_archive$val_loss)]
#     drop_out <- tuning_archive$dropout_rate[which.min(tuning_archive$val_loss)]
#
#     outer_mod <- define_mod(
#       list_structured,
#       learning_rate,
#       dropout_rate,
#       train_data,
#       y[indcs_outer[[outer]][[1]], ]
#     )
#
#     outer_mod %>% fit(
#       epochs = 50,
#       patience = 5L,
#       early_stopping = T,
#       batch_size = 32,
#       # Hier andere Validierungsdaten übergeben um early stopping zu finden
#       validation_data = list(
#         lapply(data, function(x)
#           deepregression:::subset_array(x, 28371:28372)),
#         y[28371:28372, ]
#       ),
#       view_metrics = TRUE
#     )
#
#     predictions_wo_rolling <-
#       outer_mod %>% predict(newdata = test_data)
#     colnames(predictions_wo_rolling) <-
#       c("other", "BM" ,   "HFA"   , "HNA"  , "HNFA",  "NEA"  , "SEA")
#
#     predicted_classes_wo <- colnames(predictions_wo_rolling)[max.col(predictions_wo_rolling)]
#
#     predictions_wi_rolling <- predictions_wo_rolling
#     colnames(predictions_wi_rolling) <-
#       c("other", "BM" ,   "HFA"   , "HNA"  , "HNFA",  "NEA"  , "SEA")
#
#     for (row in 1:nrow(predictions_wi_rolling)) {
#       predictions_wi_rolling[row,] <-
#         curr_mod %>% predict(newdata = lapply(test_data, function(x)
#           deepregression:::subset_array(x, row:row)))
#       predicted_class <- names(which.max(predictions_wi_rolling[row,]))
#       for (lag in 1:3) {
#         if (lag+row <= nrow(predictions_wi_rolling)) {
#           test_data[paste0("lag_", lag)][[1]][row + lag] <- as.factor(predicted_class)
#         }
#       }
#     }
#     predicted_classes_wi <- colnames(predictions_wi_rolling)[max.col(predictions_wi_rolling)]
#     table(predicted_classes_wi)
#     table(predicted_classes_wo)
#   }
# }
#
# create_splits <- function(indcs, window, horizon, skip) {
#   indcs_tmp <-
#     createTimeSlices(
#       indcs,
#       initialWindow = window,
#       horizon = horizon,
#       skip = skip,
#       fixedWindow = T
#     )
#   # create list of
#   indcs <- vector(mode = "list", length(indcs_tmp))
#   for (i in c(1:length(indcs_tmp[[1]]))) {
#     # indices for outer resampling procedure
#     indcs[[i]] <- list(indcs_tmp$train[[i]], indcs_tmp$test[[i]])
#   }
#   indcs
# }
