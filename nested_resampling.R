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

# TITLE: fits model using train - val - test splits
# x: deepregression object
# og_weights: original weights of x
# callbacks: list of callbacks used for fitting
# splits: list of train - val - test indices
# x_train: prepared training data
# learning_rate: hyp. for optimizer
# batch_size: hyp. for optimizer
# df: dataframe of observations
# class_weights: Weights used for cost-sensitive learning
# curr_length_existing: Boolean, if curr_length is included in model
# lags_existing: Boolean, if lags is included in model
# nmb_of_lags: int, degree of lags included
# RETURN if predecessor-dependent covariates are present: (fit_history, list of "false" oos evaluation, list of "true" oos evaluation)
# RETURN if no predecessor-dependent covariates are present: (fit_history, list of oos evaluation)
rep_ho <-
  function(x,
           og_weights,
           callbacks,
           splits,
           x_train,
           learning_rate,
           batch_size,
           df,
           class_weights,
           curr_length_existing,
           lags_existing,
           nmb_of_lags) {
    # fit one model per split
    fitted <- lapply(splits, function(split) {
      print("rep_ho")
      this_mod <- x$model
      # assign original weights to model to ensure comparability of splits
      this_mod$set_weights(og_weights)

      # select indices for train, validation and testset per split
      train_indcs <- split[[1]]
      val_indcs <- split[[2]]
      test_indcs <- split[[3]]

      # create corresponding data format to be used in deepregression
      train_data <- lapply(x_train, function(x)
        deepregression:::subset_array(x, train_indcs))
      val_data <- lapply(x_train, function(x)
        deepregression:::subset_array(x, val_indcs))
      test_data <- lapply(data, function(x)
        deepregression:::subset_array(x, test_indcs))

      # Define additonal parameters for fitting the model
      # Set batch size as HP
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
          batch_size = batch_size
        )
      )

      # set learning rate as HP
      ellipsis <- x$init_params$ellipsis
      ellipsis$optimizer$learning_rate$assign(learning_rate)
      args <- append(args, ellipsis)

      # fit model with args
      ret <- do.call(x$fit_fun, args)

      # calculate predictions and predicted classes
      predictions <- x %>% predict(test_data)
      colnames(predictions) <-
        c("other", "BM" ,   "HFA"   , "HNA"  , "HNFA",  "NEA"  , "SEA")
      predicted_classes <-
        factor(colnames(predictions)[max.col(predictions)], levels(df$value))
      # evaluate performance using a confusion matrix
      cm <- confusionMatrix(predicted_classes, df$value[test_indcs])

      # list of ("false") evaluation: predictions, the corresponding confusion matrix and the predicted classes
      list_pred_is <- list(predictions, cm, predicted_classes)

      # do "true" oos evaluation if predecessor dependent covariates are present
      if (curr_length_existing | lags_existing) {
        # list of "true" oos evaluation
        list_pred_oos <-
          create_oos_forecast(
            predictions,
            predicted_classes,
            x,
            test_indcs,
            test_data,
            curr_length_existing,
            lags_existing,
            df,
            nmb_of_lags
          )
        # result list: fit history of split, list of "false" oos evaluation, list of "true" oos evaluation
        results <- list(ret, list_pred_is, list_pred_oos)
      }
      else {
        # otherwise: result list: fit history of split, list of oos evaluation
        results <- list(ret, list_pred_is)
      }
      this_mod$set_weights(og_weights)
      results
    })

    # list of fit-history and predictions-list(s) per split
    return(fitted)
  }


# TITLE: Does iterative testing procedure for "true" oos evaluation
# predictions: data frame of predictions from "false" oos evaluation
# predicted_classes: vector of predicted classes from "false" oos evaluation
# x: deepregression object
# test_indcs: indces of test set
# test_data: dataset of test indices
# df: dataframe of observations
# class_weights: Weights used for cost-sensitive learning
# curr_length_existing: Boolean, if curr_length is included in model
# lags_existing: Boolean, if lags is included in model
# nmb_of_lags: int, degree of lags included
# RETURN if predecessor-dependent covariates are present: (fit_history, list of "false" oos evaluation, list of "true" oos evaluation)
# RETURN if no predecessor-dependent covariates are present: (fit_history, list of oos evaluation)
create_oos_forecast <-
  function(predictions,
           predicted_classes,
           x,
           test_indcs,
           test_data,
           curr_length_existing,
           lags_existing,
           df,
           nmb_of_lags) {

    predictions_oos <- predictions
    predicted_classes_oos <- predicted_classes

    for (row in 1:nrow(predictions_oos)) {
      # get data of current row
      dp <-
        lapply(test_data, function(x)
          deepregression:::subset_array(x, row))
      # set predictions at current row to prediction of dp
      predictions_oos[row,] <- x %>% predict(dp)
      colnames(predictions_oos) <-
        c("other", "BM" ,   "HFA"   , "HNA"  , "HNFA",  "NEA"  , "SEA")
      # get predicted class for dp
      predicted_class_oos <- names(which.max(predictions_oos[row,]))
      # set predicted classes at current row to predicted class of dp
      predicted_classes_oos[row] <- predicted_class_oos
      if (row > 1 &
          curr_length_existing & ((1 + row) <= nrow(predictions_oos))) {
        if (predicted_class_oos == predicted_classes_oos[row - 1]) {
          test_data$curr_length[row + 1] <- test_data$curr_length[[row]] + 1
        }
        else {
          test_data$curr_length[row + 1] <- 1
        }
      }
      if (lags_existing) {
        for (lag in 1:nmb_of_lags) {
          if (lag + row <= nrow(predictions_oos)) {
            test_data[paste0("lag_", lag)][[1]][row + lag] <-
              as.factor(predicted_class_oos)
          }
        }
      }
    }
    # create confusion matrix of "true" oos evaluation
    cm <- confusionMatrix(predicted_classes_oos, df$value[test_indcs])
    list(predictions_oos, cm, predicted_classes_oos)
  }


# TITLE: Helper function to remove NAs form confusion matrix
# x: confusion matrix
# RETRUN: confusion marix w.o. NAs
replace_na <- function(x) {
  x[is.na(x)] <- 0
  x
}

# TITLE: Aggregates the perforamnces of splits of repeated hold out procedure
# results: list with result from rep_ho()
# pos: int (2,3) indicating whether the (false) oos evaluation is to be evaluated
# RETRUN: vector of avg. f1-score and avg. macro average recall
evaluate_splits_rep_ho <- function(results, pos) {
  cms <- lapply(results, function(result)
    result[[pos]][[2]])

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
  return(c(avg_mf1, avg_bal_acc))
}


# x: deepregression object
# splits: nested list of train - val - test indices for both inner and outer rsmp
# tuning_archive: data frame with hps
# callbacks: list of callbacks used for fitting
# splits: list of train - val - test indices
# df: dataframe containing test values
# RETURN: list of outer rsmp. results (fit_history, list of ("false") oos evaluation, (list of "true" oos evaluation))
nested_rsmp_final <-
  function(x,
           splits,
           tuning_archive,
           df,
           callbacks,
           class_weights,
           curr_length_existing,
           lags_existing,
           nmb_of_lags) {
    # get original weights of model
    og_weights <- x$model$get_weights()

    # prepare data for training
    x_train <- deepregression:::prepare_data(
      x$init_params$parsed_formulas_content,
      gamdata = x$init_params$gamdata$data_trafos
    )

    # select outer splits
    splits_outer <- splits[[1]]
    # empty list for results of the outer resampling splits
    results <- vector(mode = "list", length = length(splits_outer))

    # iterate over outer iteration
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
            tuning_archive[hps, "batch_size"],
            df,
            class_weights,
            F,
            F,
            nmb_of_lags
          )
        eval_res <- evaluate_splits_rep_ho(results_hps, 2)
        tuning_archive[hps, 4:5] <- eval_res
      }

      print(tuning_archive)
      # select best hp combination for split
      learning_rate <-
        tuning_archive$learning_rate[which.max(tuning_archive$avg_mf1)]
      batch_size <-
        tuning_archive$batch_size[which.max(tuning_archive$avg_mf1)]

      print(learning_rate)
      print(batch_size)

      # do train-val-test procedure for current split (= one time holdout)
      results[[outer]] <-
        rep_ho(
          x,
          og_weights,
          callbacks,
          list(splits_outer[[outer]]),
          x_train,
          learning_rate,
          batch_size,
          df,
          class_weights,
          curr_length_existing,
          lags_existing,
          nmb_of_lags
        )
    }
    # list of ho-result per split (from rep_ho())
    results_unlisted <-
      lapply(results, function(result)
        result[[1]])
    results_unlisted
  }
