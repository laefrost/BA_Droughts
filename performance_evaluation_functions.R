### Info: Contains functions for performance evaluation
### Called from: models_evaluation.R

# ------------------------------- Load packages
library(ggplot2)
library(pROC)
library(PRROC)
library(plotly)
library(knitr)
library(kableExtra)
library(multiROC)
library(yardstick)

# ------------------------------- Setting colors etc.
palette_gwls <- c("#6e2474", "#eee009","#ad1313","#60adce","#e925e0","#e3a51d","#95cb77")
viz_ordered_classes <- c("BM", "HFA", "HNA", "HNFA", "NEA", "SEA", "other")

# ------------------------------- Helper functions for evaluation
# TITLE: returns elements of result-list from nested_resamling_final()
# element: integer (1,2,3) indicateing which element to retrieve
# pos: integer (2,3) indicating whether to get the is-cms or the oos-cms
# RETURN: list of elements from splits
get_result_element <- function(results, element, pos) {
  elements <- lapply(results, function(result)
    result[[pos]][[element]])
  elements
}

# TITLE: returns avg. confusion matrix of splits
# cms: either list of confusion matrix objects or list of result object from nested_resampling()
# cm_extracted: boolean, T if cms == list of cm objects; F if cms == result object from nested_resampling()
# pos: integer (2,3) indicating whether to get the is-cms or the oos-cms
# RETURN: list of avg. cms (table, byClass)
get_avg_cms <- function(cms, cm_extracted, pos) {
  if (!cm_extracted)
    cms <- get_result_element(cms, 2, pos)

  # averages the cm$table over splits
  avg_cm_table <-
    Reduce('+', lapply(cms, function(cm)
      replace_na(cm$table))) / length(cms)

  # averages the cm$byClass over splits
  avg_cm_by_class <-
    Reduce('+', lapply(cms, function(cm)
      replace_na(cm$byClass))) / length(cms)
  return(list(avg_cm_table, avg_cm_by_class))
}


# TITLE: computes cms for trainsition days
# predictions: Either list of predictions per split or result list from rep_ho()
# splits: Used splits during nested resampling
# predictions_extracted: boolean, T if predictions == list of predictions; F if predictions == result from rep_ho()
# pos: integer (2,3) indicating whether to get the ("true")-oos-predictions
# RETURN: list of avg. cms (table, byClass)
inspect_transitions <-
  function(predictions,
           splits,
           predictions_extracted,
           pos) {
    if (!predictions_extracted)
      predictions <- get_result_element(predictions, 1, pos)

    df$index <- 1:nrow(df)
    splits_outer <- splits[[1]]
    cms <- list()
    # select transistion days per split and create confusion matrix per split
    for (outer in 1:length(splits_outer)) {
      test_indcs <- splits_outer[[outer]][[3]]
      transition_days_df <-
        df[(test_indcs), ] %>% filter(transition == T)
      predictions_outer_df <-
        as.data.frame(predictions[[outer]]) %>% mutate(index = test_indcs)
      transition_predictions_df <-
        merge(x = transition_days_df, y = predictions_outer_df,
              by = "index")

      predictions_pure <-
        transition_predictions_df %>% select(other, BM, HFA, HNA, HNFA, NEA, SEA)
      predicted_classes_tmp <-
        factor(colnames(predictions_pure)[max.col(predictions_pure)], levels(df$value))
      cms[[outer]] <-
        confusionMatrix(predicted_classes_tmp, transition_predictions_df$value)
    }
    avg_cms <- get_avg_cms(cms, T)
    cmb_cms <- c(avg_cms, cms)
    cmb_cms
  }


# ------------------------------- Functions creating the evaluation output per model
# TITLE: Creates dataframe combining different metrics from the confusion matrix, saves it into .tex file
# cms: list of avg. confusion matrices from get_avg_cms()
# RETURN: dataframe with combined combinations
build_cm_output <- function(avg_cms, model_name, plot_name) {
  base_cm <- as.data.frame.matrix(avg_cms[[1]])
  base_cm <- base_cm[c(2, 3, 4, 5, 6, 7, 1), c(2, 3, 4, 5, 6, 7, 1)]
  # create additional columns
  col_1 <- round(rowSums(base_cm), 2)
  row_1 <- c(round(colSums(base_cm), 2), NA, NA)
  base_cm <- round(base_cm, 2)
  precision_tmp <- unname(round(avg_cms[[2]][, "Precision"], 2))
  precision_tmp <- precision_tmp[c(2, 3, 4, 5, 6, 7, 1)]
  combined_cm <-
    base_cm %>% mutate("colSums" = col_1, Precision = (precision_tmp))
  # create additional rows
  recall_tmp <- unname(round(avg_cms[[2]][, "Recall"], 2))
  recall_tmp <- recall_tmp[c(2, 3, 4, 5, 6, 7, 1)]
  row_2 <- c(recall_tmp, NA, NA)
  combined_cm <- rbind(combined_cm, row_1, row_2)
  cm_tex <-
    kable(combined_cm, booktabs = TRUE, "latex") %>% add_header_above(c("Labels" = 10)) %>%
    column_spec(8, bold = F, border_right = T) %>%
    kable_styling(font_size = 11)
  writeLines(cm_tex, paste0(model_name, "/", plot_name, ".tex"))

  combined_cm
}

# TITLE: Aggregates the perforamnces of splits of repeated hold out procedure
# results: list with result from rep_ho()
# pos: int (2,3) indicating whether the (known predecessor) oos evaluation is to be evaluated
# RETRUN: vector of performance metrics
evaluate_final_splits <- function(results, pos, test_indcs, y) {
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
  avg_mar <-
    mean(Reduce('+', lapply(cms, function(cm)
      replace_na(cm$byClass)[, "Recall"])) / length(cms))
  avg_map <-
    mean(Reduce('+', lapply(cms, function(cm)
      replace_na(cm$byClass)[, "Precision"])) / length(cms))
  avg_accuracy <- mean(unlist(lapply(cms, function(cm)  replace_na(cm$overall)["Accuracy"])))

  cw_f1 <- Reduce('+', lapply(cms, function(cm)
    replace_na(cm$byClass)[, "F1"])) / length(cms)


  y_test <- deepregression:::subset_array(y, c(test_indcs[[1]],test_indcs[[2]],test_indcs[[3]]))
  y_pred <- rbind(results[[1]][[pos]][[1]], results[[2]][[pos]][[1]],results[[3]][[pos]][[1]])
  cat_cross <- k_mean(tf$keras$metrics$categorical_crossentropy(y_test, y_pred))$numpy()

  return(c(round(unname(cw_f1), 2),
            format(round(avg_mf1,2), nsmall = 2),
           format(round(avg_mar,2), nsmall = 2),
           format(round(avg_map,2), nsmall = 2),
           format(round(avg_accuracy,2), nsmall = 2),
           format(round(cat_cross,2), nsmall = 2)))

}

# TITLE: Creates dataframe combining overview metrics of models
# list_models_results: list of model results
# model_names: list of model names specifications to identify models
# RETURN: dataframe containing overview metrics per specification
build_overview_transposed <- function(model_names, list_models_results, test_indices, y) {
  rows <- c("other","BM","HFA","HNA","HNFA","NEA","SEA","Avg. macro F1-Score",
            "Avg. macro recall", "Avg. macro precision", "Avg. accuracy", "Avg. crossentropy")
  overview_df <- data.frame(matrix(NA,    # Create empty data frame
                                    nrow = length(rows),
                                    ncol = length(model_names)))
  colnames(overview_df) <- model_names
  rownames(overview_df) <- rows
  for (model in 1:length(list_models_results)) {
    if (length(list_models_results[[model]][[1]]) == 3) {
      perf_wo <- evaluate_final_splits(list_models_results[[model]], 2, test_indices, y)
      perf_wi <-
        evaluate_final_splits(list_models_results[[model]], 3, test_indices, y)
      perf <- paste0(perf_wi, " (", perf_wo, ")")
    }
    else {
      perf <- evaluate_final_splits(list_models_results[[model]], 2, test_indices, y)
    }
    overview_df[, model] <- c(perf)
  }
  overview_tex <- kable(overview_df, booktabs = TRUE, "latex")
  print(overview_tex)
  overview_df
}


# TITLE: Creates line plot with predicted classes vs. true classes
# predictions: list of predicted classes
# test_indices: indices of test sets
# df: dataframe containing all observations
# model_name: model name necessary for saving
# appendix: further arguments for saving
# RETURN: dataframe containing overview metrics per specification
generate_lineplot_predictions <- function(predictions, test_indices, df, model_name, appendix =""){
  predictions_1 <- predictions[[1]]
  predictions_2 <- predictions[[2]]
  predictions_3 <- predictions[[3]]

  tv_1 <- df$value[test_indices[[1]]]
  tv_2 <- df$value[test_indices[[2]]]
  tv_3 <- df$value[test_indices[[3]]]

  df_1 <- data.frame(cp = c(tv_1, predictions_1), split = "Iteration 1", index = 1:length(predictions_1),
                     indicator = c(rep("True class", length(predictions_1)), rep("Predicted class", length(predictions_1))))
  df_2 <- data.frame(cp = c( tv_2, predictions_2), split = "Iteration 2", index = 1:length(predictions_2),
                     indicator = c(rep("True class", length(predictions_2)), rep("Predicted class", length(predictions_2))))
  df_3 <- data.frame(cp = c( tv_3, predictions_3), split = "Iteration 3", index = 1:length(predictions_3),
                     indicator = c(rep("True class", length(predictions_3)), rep("Predicted class", length(predictions_3))))

  df_combined <- rbind(df_1, df_2, df_3)
  lineplot <- ggplot(df_combined, aes(x = index, y = cp,
                                      colour = factor(indicator, levels = c("True class", "Predicted class")),
                                      group = factor(indicator, levels = c("True class", "Predicted class")))) +
    geom_line() + facet_wrap(~ split, nrow = 3) +
    scale_x_continuous(name = "Index t in test set") +
    scale_y_discrete(name = "Circulation pattern") +
    scale_colour_manual(values = c("#0dacec", "#ecb40d"), name = "Class") +
    theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1))

  ggsave(
    filename = paste0(model_name, appendix, "_lineplot.png"),
    plot = lineplot,
    path = paste0(model_name),
    width = 30,
    height = 15,
    dpi = 300,
    units = "in"
  )
}

# TITLE: Creates line plot with predicted classes of model 1 vs. true classes
# vs. predicted classes of model 2
# predictions: list of predicted classes
# test_indices: indices of test sets
# df: dataframe containing all observations
# model_name: model name necessary for saving
# appendix: further arguments for saving
# RETURN: dataframe containing overview metrics per specification
generate_lineplot_comparison <- function(predictions, predictions2, test_indices, df, model_name, appendix = ""){
  predictions_1 <- predictions[[1]]
  predictions_2 <- predictions[[2]]
  predictions_3 <- predictions[[3]]

  predictions2_1 <- predictions2[[1]]
  predictions2_2 <- predictions2[[2]]
  predictions2_3 <- predictions2[[3]]

  tv_1 <- df$value[test_indices[[1]]]
  tv_2 <- df$value[test_indices[[2]]]
  tv_3 <- df$value[test_indices[[3]]]

  df_1 <- data.frame(cp = c(tv_1, predictions_1, predictions2_1), split = "Iteration 1", index = 1:length(predictions_1),
                     indicator = c(rep("True class", length(predictions_1)),
                                   rep("Predicted class (known predeccessor)", length(predictions_1)),
                                   rep("Predicted class (unknown predeccessor)", length(predictions_1))))
  df_2 <- data.frame(cp = c( tv_2, predictions_2, predictions2_2), split = "Iteration 2", index = 1:length(predictions_2),
                     indicator = c(rep("True class", length(predictions_2)),
                                   rep("Predicted class (known predeccessor)", length(predictions_2)),
                                   rep("Predicted class (unknown predeccessor)", length(predictions_2))))
  df_3 <- data.frame(cp = c( tv_3, predictions_3, predictions2_3), split = "Iteration 3", index = 1:length(predictions_3),
                     indicator = c(rep("True class", length(predictions_3)),
                                   rep("Predicted class (known predeccessor)", length(predictions_3)),
                                   rep("Predicted class (unknown predeccessor)", length(predictions_3))))

  df_combined <- rbind(df_1, df_2, df_3)
  lineplot <- ggplot(df_combined, aes(x = index, y = cp,
                                      colour = factor(indicator, levels = c("True class", "Predicted class (known predeccessor)", "Predicted class (unknown predeccessor)")),
                                      group = factor(indicator, levels = c("True class", "Predicted class (known predeccessor)", "Predicted class (unknown predeccessor)")))) +
    geom_line() + facet_wrap(~ split, nrow = 3) +
    scale_x_continuous(name = "Index t in test set") +
    scale_y_discrete(name = "Circulation pattern") +
    scale_colour_manual(values = c("#0dacec", "#ecb40d", "violet"), name = "Class") +
    theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1))

  ggsave(
    filename = paste0(model_name, "_forecast", "_lineplot_comparison.png"),
    plot = lineplot,
    path = paste0(model_name),
    width = 30,
    height = 15,
    dpi = 300,
    units = "in"
  )
}


# TITLE: Calls all of the above function for a given model result
# model_result: model result from nested_resampling_final()
# bool_forecast: logical, T if predecessor dependent varaibeles are used, otherwise F
# splits: splits used for nested resampling
# test_indices: indices of test sets
# df: data frame containing all observations
# model_name: model name necessary for saving
# RETURN: -
eval_pipe <-
  function(model_result,
           bool_forecast,
           model_name,
           splits,
           df,
           test_indices) {

    predictions <- get_result_element(model_result, 1, 2)
    predicted_classes <- get_result_element(model_result, 3, 2)

    # generate and save lineplot of prediction behaviour
    generate_lineplot_predictions(predicted_classes, test_indices, df, model_name)

    # create confusion matrices for predictions
    cms_pred <- get_avg_cms(model_result, F, 2)
    build_cm_output(cms_pred, model_name, paste0(model_name, "_cm_pred"))

    # create confusion matrices for transition days
    cms_transitions <- inspect_transitions(model_result, splits, F, 2)
    cms_transitions <- cms_transitions[c(1, 2)]
    # generate and save .tex cm output
    build_cm_output(cms_transitions, model_name, paste0(model_name, "_cm_transition_pred"))

    if (bool_forecast) {
      predictions_fc <- get_result_element(model_result, 1, 3)
      predicted_classes_fc <- get_result_element(model_result, 3, 3)
      cms_forecast <- get_avg_cms(model_result, F, 3)
      build_cm_output(cms_forecast, model_name, paste0(model_name, "_cm_forecast"))
      cms_transitions_forecast <-
        inspect_transitions(model_result, splits, F, 3)
      cms_transitions_forecast <- cms_transitions_forecast[c(1, 2)]
      build_cm_output(cms_transitions_forecast, model_name,
                      paste0(model_name, "_cm_transition_forecast"))
      generate_lineplot_predictions(predicted_classes_fc, test_indices, df, model_name, "_forecast")
      generate_lineplot_comparison(predicted_classes, predicted_classes_fc, test_indices, df, model_name)
    }
  }
