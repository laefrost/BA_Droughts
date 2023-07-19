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
#source("preprocessing.R")
#source("splits.R")

# ####################################################### Helpers
# f1 <- function(y_true, y_pred) {
#   true_positives <- k_sum(k_round(k_clip(y_true * y_pred, 0, 1)))
#   possible_positives <- k_sum(k_round(k_clip(y_true, 0, 1)))
#   predicted_positives <- k_sum(k_round(k_clip(y_pred, 0, 1)))
#   precision <- true_positives / (predicted_positives + k_epsilon())
#   recall <- true_positives / (possible_positives + k_epsilon())
#   f1_val <-
#     2 * (precision * recall) / (precision + recall + k_epsilon())
#   return(f1_val)
# }
#
# cnn_block <-
#   function(filters,
#            kernel_size,
#            pool_size,
#            rate,
#            input_shape = NULL) {
#     function(x) {
#       x %>%
#         layer_conv_2d(filters,
#                       kernel_size,
#                       padding = "same",
#                       input_shape = input_shape) %>%
#         layer_activation(activation = "relu") %>%
#         layer_batch_normalization() %>%
#         layer_max_pooling_2d(pool_size = pool_size) %>%
#         layer_dropout(rate = rate)
#     }
#   }
#
# define_cnn <- function(dropout_rate, channels, num_classes) {
#   cnn1_pic <-
#     cnn_block(
#       filters = 8,
#       kernel_size = c(5, 5),
#       pool_size = c(2, 2),
#       rate = dropout_rate,
#       input_shape = shape(39, 16, channels)
#     )
#   cnn2_pic <-
#     cnn_block(
#       filters = 16,
#       kernel_size = c(5, 5),
#       pool_size = c(2, 2),
#       rate = dropout_rate
#     )
#
#   cnn_pic <- keras_model_sequential() %>%
#     cnn1_pic() %>%
#     cnn2_pic() %>%
#     layer_flatten() %>%
#     layer_activation(activation = "relu") %>%
#     layer_batch_normalization() %>%
#     layer_dropout(rate = dropout_rate) %>%
#     layer_dense(num_classes)
#   cnn_pic
# }
#
#
replace_na <- function(x) {
  x[is.na(x)] <- 0
  x
}

####################################################### Status: Tested and done
# TITLE: fits model using train - val - test splits
# x: deepregression object
# og_weights: original weights of x
# callbacks: list of callbacks used for fitting
# splits: list of train - val - test indices
# x_train: prepared data
# learning_rate: hyp. for optimizer
# df: dataframe containing test values
# RETURN: list of lists (fit_history and confusion matrix per split)
rep_ho <-
  function(x,
           og_weights,
           callbacks,
           splits,
           x_train,
           learning_rate,
           dropout_rate,
           df,
           class_weights) {
    fitted <- lapply(splits, function(split) {
      print("rep_ho")
      this_mod <- x$model
      this_mod$set_weights(og_weights)

      train_indcs <- split[[1]]
      val_indcs <- split[[2]]
      test_indcs <- split[[3]]

      train_data <- lapply(x_train, function(x)
        deepregression:::subset_array(x, train_indcs))
      val_data <- lapply(x_train, function(x)
        deepregression:::subset_array(x, val_indcs))
      test_data <- lapply(data, function(x)
        deepregression:::subset_array(x, test_indcs))

      args <- list()
      args <- append(
        args,
        list(
          object = this_mod,
          x = train_data,
          y = deepregression:::subset_array(x$init_params$y, train_indcs),
          validation_split = NULL,
          validation_data = list(
            val_data,
            deepregression:::subset_array(x$init_params$y, val_indcs)
          ),
          callbacks = callbacks,
          verbose = F,
          view_metrics = FALSE,
          class_weight = class_weights,
          epochs = 50,
          batch_size = 32
        )
      )

      # assign hyperparameters
      ellipsis <- x$init_params$ellipsis
      ellipsis$optimizer$learning_rate$assign(learning_rate)
      args <- append(args, ellipsis)

      # fit model with args
      ret <- do.call(x$fit_fun, args)
      predictions <- x %>% predict(test_data)
      colnames(predictions) <-
        c("other", "BM" ,   "HFA"   , "HNA"  , "HNFA",  "NEA"  , "SEA")
      predicted_classes <-
        factor(colnames(predictions)[max.col(predictions)], levels(df$value))
      cm <- confusionMatrix(predicted_classes, df$value[test_indcs])
      this_mod$set_weights(og_weights)
      list(ret, predictions, cm)

    })
    # list of fit-history and predictions
    return(fitted)
  }

################################################ Status: Tested and done
evaluate_splits_rep_ho <- function(results) {
  fit_hists <- lapply(results, function(result)
    result[[1]])
  predictions <- lapply(results, function(result)
    result[[2]])
  cms <- lapply(results, function(result)
    result[[3]])

  avg_cm_table <-
    Reduce('+', lapply(cms, function(cm)
      replace_na(cm$table))) / length(cms)
  avg_cm_by_class <-
    Reduce('+', lapply(cms, function(cm)
      replace_na(cm$byClass))) / length(cms)
  avg_mf1 <-
    mean(Reduce('+', lapply(cms, function(cm)
      replace_na(cm$byClass)[, "F1"])) / length(cms))
  avg_bal_acc <-
    mean(Reduce('+', lapply(cms, function(cm)
      replace_na(cm$byClass)[, "Recall"])) / length(cms))

  #avg_map <- sum(avg_cm_by_class[,"Precision"]) / 7
  #avg_mf1 <- 2*((avg_map * avg_mar)/(avg_map + avg_mar))
  #avg_mf1 <- mean(avg_cm_by_class[,"F1"])
  #print(c(avg_mf1, avg_bal_acc))

  return(c(avg_mf1, avg_bal_acc))
}

# splits_helper <- create_splits_nested(df)
# x_test_2 <- deepregression(
#   y = y,
#   list_of_formulas = list( ~ 1 + lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + curr_length) ,
#   family = "multinoulli",
#   data = data,
#   list_of_deep_models = list(d = define_cnn(0.25, 2, 7)),
#   optimizer = optimizer_adam(learning_rate = 0.0001),
#   monitor_metrics = list(
#     f1,
#     "categorical_accuracy",
#     "categorical_crossentropy",
#     tf$keras$metrics$AUC(multi_label = T)
#   )
# )
#
# callbacks <- list()
# callbacks <-
#   append(callbacks, list(
#     callback_early_stopping(
#       patience = 6,
#       restore_best_weights = TRUE,
#       monitor = "val_loss"
#     )
#   ))
#
# hyper_grid <-
#   expand.grid(learning_rate = c(0.0001, 0.0005, 0.001),
#               dropout_rate  = c(0.25))
#
# tuning_archive <- data.frame(
#   "tuning_iteration" = 1:(nrow(hyper_grid)),
#   "learning_rate" = hyper_grid$learning_rate,
#   "dropout_rate" = hyper_grid$dropout_rate,
#   "avg_mf1" = 0,
#   "avg_bal_acc" = 0
# )
#
# nested_rsmp_final(x_test_2, splits_helper, tuning_archive, df, callbacks, class_weights)

# x: deepregression object
# splits: nested list of train - val - test indices for both inner and outer rsmp
# tuning_archive: data frame with hps
# callbacks: list of callbacks used for fitting
# splits: list of train - val - test indices
# df: dataframe containing test values
# RETURN: list of outer rsmp. results
nested_rsmp_final <-
  function(x,
           splits,
           tuning_archive,
           df,
           callbacks,
           class_weights) {
    og_weights <- x$model$get_weights()
    x_train <- deepregression:::prepare_data(
      x$init_params$parsed_formulas_content,
      gamdata = x$init_params$gamdata$data_trafos
    )

    splits_outer <- splits[[1]]
    results <- vector(mode = "list", length = length(splits_outer))
    str(results)

    for (outer in seq_along(splits_outer)) {
      print(paste(outer, "outer"))
      splits_inner <- splits[[2]][[outer]]

      # do HP-tuning using inner splits
      for (hps in 1:nrow(tuning_archive)) {
        print(paste(hps, " tuning"))
        # do rep hop for inner resmp. procedure
        results_hps <-
          rep_ho(
            x,
            og_weights,
            callbacks,
            splits_inner,
            x_train,
            tuning_archive[hps, "learning_rate"],
            tuning_archive[hps, "dropout_rate"],
            df,
            class_weights
          )
        #str(results_hps)
        eval_res <- evaluate_splits_rep_ho(results_hps)
        #print(eval_res)
        tuning_archive[hps, 4:5] <- eval_res
      }
    }
    print(tuning_archive)
    # select best hp combination
    learning_rate <-
      tuning_archive$learning_rate[which.max(tuning_archive$avg_mf1)]
    drop_out <-
      tuning_archive$dropout_rate[which.max(tuning_archive$avg_mf1)]
    print(learning_rate)
    print(drop_out)

    # #tmp drop_out
    # drop_out = 0.2
    # learning_rate = 0.001
    # print("Splits")
    # print(splits_outer[[outer]])
    # do rep ho for outer split and append result

    results_outer <-
      rep_ho(
        x,
        og_weights,
        callbacks,
        splits_outer,
        x_train,
        learning_rate,
        drop_out,
        df,
        class_weights
      )
    #print(results[[outer]])
    results_outer
  }



# TITLE: does nested resampling
# x: deepregression object
# og_weights: original weights of x
# callbacks: list of callbacks used for fitting
# splits: list of train - val - test indices
# x_train: prepared data
# df: dataframe containing test values
#
# RETURN:
# nested_rspm <- function(x, og_weights, callbacks, splits, x_train, tuning_archive, df) {
#   nested_result <- lapply(splits, function(split) {
#
#   inner_splits <- create_splits(1930, df[split[[1]][length(split[[1]])], "year"], 5, 5, df, 10)
#
#   print(df[split[[1]][length(split[[1]])], "year"])
#
#   print(str(inner_splits))
#
#   # tuning_result <- lapply(tuning_archive, function(hps) rep_ho(x,
#   #                                              og_weights,
#   #                                              this_mod,
#   #                                              splits,
#   #                                              hps[[1]],
#   #                                              hps[[2]],
#   #                                              callbacks,
#   #                                              x_train, df))
#   # }
#   }
#   )
# }
#
# nested_rspm(x, og_weights, callbacks, splits, x_train, tuning_archive, df)
