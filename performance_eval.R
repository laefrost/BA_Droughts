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

