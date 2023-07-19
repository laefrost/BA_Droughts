# tests for evaluation
source("nested_resampling.R")
source("preprocessing.R")
source("splits.R")
test_results <- readRDS("mod_imgs_outer_res")
test_results2 <- readRDS("mod_lags_outer_res")
evaluate_splits_rep_ho(test_results)
evaluate_splits_rep_ho(test_results2)
get_avg_cms(test_results, F)
get_avg_cms(test_results2, F)

################################################ Status: Tested and done
get_avg_cms <- function(cms, cm_extracted) {
  # fit_hists <- lapply(results, function(result)
  #   result[[1]])
  # predictions <- lapply(results, function(result)
  #   result[[2]])
  #### das hier vlt noch je nach input anpassen
  if(!cm_extracted)
    cms <- lapply(cms, function(result)
    result[[3]])

  avg_cm_table <-
    Reduce('+', lapply(cms, function(cm)
      replace_na(cm$table))) / length(cms)
  avg_cm_by_class <-
    Reduce('+', lapply(cms, function(cm)
      replace_na(cm$byClass))) / length(cms)
  return(list(avg_cm_table, avg_cm_by_class))
}


inspect_transitions <- function(predictions, splits, predictions_extracted){
  if (!predictions_extracted)
    predictions <- lapply(predictions, function(result) result[[2]])

  df$index <- 1:nrow(df)
  splits_outer <- splits[[1]]
  cms <- list()
  for (outer in 1:length(splits_outer)) {
    test_indcs <- splits_outer[[outer]][[3]]
    transition_days_df <-  df[(test_indcs),] %>% filter(transition == T)
    predictions_outer_df <- as.data.frame(predictions[[outer]]) %>% mutate(index = test_indcs)
    transition_predictions_df <- merge(x=transition_days,y=predictions_outer,
                                    by="index")

    predictions_pure <- transition_predictions_df %>% select(other, BM, HFA, HNA, HNFA, NEA, SEA)
    predicted_classes_tmp <- factor(colnames(predictions_pure)[max.col(predictions_pure)], levels(df$value))
    cms[[outer]] <- confusionMatrix(predicted_classes_tmp, transition_predictions$value)
  }
  avg_cms <- get_avg_cms(cms, T)
  cmb_cms <- c(avg_cms, cms)
  cmb_cms
}

################## For Testing
# predictions, x, curr_length_existing, lags_existing, test_indcs, data
# splits <- create_splits_nested(df)
# splits[[1]][[1]][[3]]
# oos <- create_oos_forecast(test_results2[[1]][[2]], test_results2[[1]], F, T, splits[[1]][[1]][[3]], data)

create_oos_forecast <- function(predictions, x, curr_length_existing, lags_existing, test_indcs, data){
  predictions_oos <- predictions
  test_data <- lapply(data, function(x) deepregression:::subset_array(x, test_indcs))
  for (row in 1:nrow(predictions_oos)) {
    dp <- lapply(test_data, function(x) deepregression:::subset_array(x, row))
    predictions_oos[row, ] <- x %>% predict(dp)
    predicted_class_oos <- names(which.max(predictions_oos[row, ]))
    predicted_classes_oos[row] <- predicted_class_oos
    if (row > 1 & curr_length_existing){
      if(predicted_class_oos == predicted_classes_oos[row-1])
        test_data$curr_length[row] <- test_data$curr_length[[row - 1]] + 1
      else
        test_data$curr_length[row] <- 1
    }
    if (lags_existing) {
      for (lag in 1:6) {
        if (lag + row <= nrow(predictions_oos))
          test_data[paste0("lag_", lag)][[1]][row + lag] <-as.factor(predicted_class_oos)
      }
    }
  }
  list(predictions_oos, predicted_classes_oos)
}

