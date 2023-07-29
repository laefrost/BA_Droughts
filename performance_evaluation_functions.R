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
  # fit_hists <- lapply(results, function(result)
  #   result[[1]])
  # predictions <- lapply(results, function(result)
  #   result[[2]])
  #### das hier vlt noch je nach input anpassen
  if (!cm_extracted)
    cms <- get_result_element(cms, 2, pos)

  avg_cm_table <-
    Reduce('+', lapply(cms, function(cm)
      replace_na(cm$table))) / length(cms)
  avg_cm_by_class <-
    Reduce('+', lapply(cms, function(cm)
      replace_na(cm$byClass))) / length(cms)
  return(list(avg_cm_table, avg_cm_by_class))
}


# TITLE: computes cms for trainsition days
# predictions:
# cm_extracted: boolean, T if cms == list of cm objects; F if cms == result object from nested_resampling()
# pos: integer (2,3) indicating whether to get the is-cms or the oos-cms
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


# ------------------------------- Functions creating the evaluation putput per model


# TITLE: Creates dataframe combining different metrics from the confusion matrix, saves it into .tex file
# cms: list of avg. confusion matrices from get_avg_cms()
# RETURN: dataframe with combined combinations
build_cm_output <- function(avg_cms, model_name) {
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
  writeLines(cm_tex, paste0(model_name, ".tex"))

  combined_cm
}

################################################ Status: Tested and done
evaluate_final_splits <- function(results, pos) {
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

  return(c(round(unname(cw_f1), 2),
            format(round(avg_mf1,2), nsmall = 2),
           format(round(avg_mar,2), nsmall = 2),
           format(round(avg_map,2), nsmall = 2),
           format(round(avg_accuracy,2), nsmall = 2)))

}

# TITLE: Creates dataframe combining overview metrics of models
# cms: list of model results
# RETURN: dataframe with combined combinations
build_overview <-
  function(list_models_results,
           formulas,
           model_names) {
    overview_df <-
      data.frame(
        "Model" = rep("0", length(list_models_results)),
        "other" =rep("0", length(list_models_results)),
        "BM" = rep("0", length(list_models_results)),
        "HFA" = rep("0", length(list_models_results)),
        "HNA" = rep("0", length(list_models_results)),
        "HNFA" = rep("0", length(list_models_results)),
        "NEA" = rep("0", length(list_models_results)),
        "SEA" = rep("0", length(list_models_results)),
        "Avg. macro F1-Score" = rep(0, length(list_models_results)),
        "Avg. macro recall" = rep(0, length(list_models_results)),
        "Avg. macro precision" = rep(0, length(list_models_results)),
        "Avg. accuracy" =  rep(0, length(list_models_results))
      )
    for (model in 1:length(list_models_results)) {
      if (length(list_models_results[[model]][[1]]) == 3) {
        perf_wo <- evaluate_final_splits(list_models_results[[model]], 2)
        perf_wi <-
          evaluate_final_splits(list_models_results[[model]], 3)
        perf <- paste0(perf_wi, " (", perf_wo, ")")
      }
      else {
        perf <- evaluate_final_splits(list_models_results[[model]], 2)
      }
      overview_df[model,] <- c(formulas[[model]], perf)
    }
    overview_tex <- kable(overview_df, booktabs = TRUE, "latex")
    print(overview_tex)
    overview_df
  }


build_overview_transposed <- function(model_names, list_models_results) {
  rows <- c("other","BM","HFA","HNA","HNFA","NEA","SEA","Avg. macro F1-Score",
            "Avg. macro recall", "Avg. macro precision", "Avg. accuracy")
  overview_df <- data.frame(matrix(NA,    # Create empty data frame
                                    nrow = length(rows),
                                    ncol = length(model_names)))
  colnames(overview_df) <- model_names
  rownames(overview_df) <- rows
  for (model in 1:length(list_models_results)) {
    if (length(list_models_results[[model]][[1]]) == 3) {
      perf_wo <- evaluate_final_splits(list_models_results[[model]], 2)
      perf_wi <-
        evaluate_final_splits(list_models_results[[model]], 3)
      perf <- paste0(perf_wi, " (", perf_wo, ")")
    }
    else {
      perf <- evaluate_final_splits(list_models_results[[model]], 2)
    }
    overview_df[, model] <- c(perf)
  }
  overvie_tex <- kable(overview_df, booktabs = TRUE, "latex")
  print(overvie_tex)
  overview_df
}

generate_lineplot_predictions <- function(predictions, test_indices, df, plot_name){
  print(str(predictions))
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
  print(tail(df_combined))
  lineplot <- ggplot(df_combined, aes(x = index, y = cp,
                                      colour = factor(indicator, levels = c("True class", "Predicted class")),
                                      group = factor(indicator, levels = c("True class", "Predicted class")))) +
    geom_line() + facet_wrap(~ split, nrow = 3) +
    scale_x_continuous(name = "Index t in test set") +
    scale_y_discrete(name = "Circulation pattern") +
    scale_colour_manual(values = c("#0dacec", "#ecb40d"), name = "Class") +
    theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1))

  ggsave(
    paste0(plot_name, "_lineplot.png"),
    lineplot,
    width = 30,
    height = 15,
    dpi = 300,
    units = "in"
  )
}


generate_lineplot_comparison <- function(predictions, predictions2, test_indices, df, plot_name){
  print(str(predictions))
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
                                   rep("Predicted class", length(predictions_1)),
                                   rep("Predicted class 2", length(predictions_1))))
  df_2 <- data.frame(cp = c( tv_2, predictions_2, predictions2_2), split = "Iteration 2", index = 1:length(predictions_2),
                     indicator = c(rep("True class", length(predictions_2)),
                                   rep("Predicted class", length(predictions_2)),
                                   rep("Predicted class 2", length(predictions_2))))
  df_3 <- data.frame(cp = c( tv_3, predictions_3, predictions2_3), split = "Iteration 3", index = 1:length(predictions_3),
                     indicator = c(rep("True class", length(predictions_3)),
                                   rep("Predicted class", length(predictions_3)),
                                   rep("Predicted class 2", length(predictions_3))))

  df_combined <- rbind(df_1, df_2, df_3)
  print(tail(df_combined))
  lineplot <- ggplot(df_combined, aes(x = index, y = cp,
                                      colour = factor(indicator, levels = c("True class", "Predicted class", "Predicted class 2")),
                                      group = factor(indicator, levels = c("True class", "Predicted class", "Predicted class 2")))) +
    geom_line() + facet_wrap(~ split, nrow = 3) +
    scale_x_continuous(name = "Index t in test set") +
    scale_y_discrete(name = "Circulation pattern") +
    scale_colour_manual(values = c("#0dacec", "#ecb40d", "violet"), name = "Class") +
    theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 1))

  ggsave(
    paste0(plot_name, "_lineplot_comparison.png"),
    lineplot,
    width = 30,
    height = 15,
    dpi = 300,
    units = "in"
  )
}


eval_pipe <-
  function(model_result,
           bool_forecast,
           model_name,
           splits,
           df,
           test_indices) {

    predictions <- get_result_element(model_result, 1, 2)
    predicted_classes <- get_result_element(model_result, 3, 2)

    cms_pred <- get_avg_cms(model_result, F, 2)
    build_cm_output(cms_pred, paste0(model_name, "_cm_pred"))

    cms_transitions <- inspect_transitions(model_result, splits, F, 2)
    cms_transitions <- cms_transitions[c(1, 2)]
    build_cm_output(cms_transitions, paste0(model_name, "_cm_transition_pred"))

    generate_lineplot_predictions(predicted_classes, test_indices, df, model_name)

    build_roc(predictions, predicted_classes,test_indices, model_name)

    build_boxplot(
      predictions,
      predicted_classes,
      2,
      test_indices,
      df,
      paste0(model_name, "_boxplot_pred")
    )

    if (bool_forecast) {
      predictions_fc <- get_result_element(model_result, 1, 3)
      predicted_classes_fc <- get_result_element(model_result, 3, 3)
      cms_forecast <- get_avg_cms(model_result, F, 3)
      build_cm_output(cms_forecast, paste0(model_name, "_cm_forecast"))
      cms_transitions_forecast <-
        inspect_transitions(model_result, splits, F, 3)
      cms_transitions_forecast <- cms_transitions_forecast[c(1, 2)]
      build_cm_output(cms_transitions_forecast,
                      paste0(model_name, "_cm_transition_forecast"))
      build_boxplot(
        predictions_fc,
        predicted_classes_fc,
        3,
        test_indices,
        df,
        paste0(model_name, "_boxplot_forecast")
      )

      generate_lineplot_predictions(predicted_classes_fc, test_indices, df, paste0(model_name, "_forecast"))
      generate_lineplot_comparison(predicted_classes, predicted_classes_fc, test_indices, df, model_name)
    }
    # create One vs. ALL ROC's and save them in one plot ---> ToDo: write function
  }


#
#
# predictions <- get_result_element(mod_imgs_outer_res, 1, 2)
# pc_123 <- get_result_element(mod_imgs_outer_res, 3, 2)
# test_df <- build_boxplot(predictions, pc, 2, test_indcs_helper, df, "testplot")
#
# ggplot(test_df, aes(x = curr_class, y = probability, fill = curr_class)) +
#   geom_boxplot() +
#   scale_color_brewer(palette="Dark2") + facet_wrap(~type) +
#   theme(legend.position = "bottom") + guides(fill = guide_legend(nrow = 1, title = "Circulation pattern")) +
#   scale_x_discrete(name = "Circulation pattern") +
#   scale_y_continuous(name = "Probability", breaks = seq(0.0, 1.0, by = 0.25)) +
#   coord_cartesian(ylim = c(0, 1))

# create_ROCs <- function(model_result, pos){
#   predictions <- get_result_element(model_result, 1, 2)
#   predictions <- get_result_element(model_result, 1, 2)
# }

#
#
# str(mod_imgs_lags_length_outer_res)
# p<-ggplot(df_boxplot_test, aes(x=tc, y=rel_predict, fill=type)) +
#   geom_boxplot(position=position_dodge(1))
# p
#
# # bsp fpür tc = other
# df_boxplot_other <- df_boxplot_test %>% filter(curr_class == "other" , type == "FN") %>% select(curr_class, p_predicted, p_true, pc, tc)
#
# ggplot(df_boxplot_other, aes(x=type, y=rel_predict, fill=pc)) +
#   geom_boxplot(position=position_dodge(1))


# c_weigths <- c(unlist(class_weights), NA)
# nmb_of_obs <- lapply(unique_classes, function(class) {
#   nrow(df_og_length[df_og_length$value == class, ])})
# nmb_of_obs <- c(nmb_of_obs, sum(unlist(nmb_of_obs)))
# df_c_weigths <- rbind(nmb_of_obs, round(c_weigths,2))
# colnames(df_c_weigths) <-
#   c("other", "BM" ,   "HFA"   , "HNA"  , "HNFA",  "NEA"  , "SEA", "Sum")
# df_c_weigths <- df_c_weigths[, c(2,3,4,5,6,7,1,8)]
#
# kable(df_c_weigths, booktabs = TRUE, "latex") %>% #add_header_above(c("Labels" = 10)) %>%
#   #column_spec(8, bold = F, border_right = T) %>%
#   kable_styling(font_size = 11)

# hyper_grid
#
# kable(hyper_grid, booktabs = TRUE, "latex") %>% add_header_above(c("Hyperparameters" = 2)) %>%
#    #column_spec(8, bold = F, border_right = T) %>%
#    kable_styling(font_size = 11)
#
#
# df_viz_index <- df
# df_viz_index$index <- 1:nrow(df)
#
# train_splits_helper <- splits_helper[[1]]
# df_vizzz <- data.frame("Iteration" = c(1,1,1,2,2,2,3,3,3),
#              "t" = c(length(train_splits_helper[[1]][[1]]),length(train_splits_helper[[1]][[2]]), length(train_splits_helper[[1]][[3]]),
#                           length(train_splits_helper[[2]][[1]]),length(train_splits_helper[[2]][[2]]), length(train_splits_helper[[2]][[3]]),
#                           length(train_splits_helper[[3]][[1]]),length(train_splits_helper[[3]][[2]]), length(train_splits_helper[[3]][[3]])),
#              "Subset" = c("train", "validation", "test", "train", "validation", "test", "train", "validation", "test"))
# df_vizzz$Subset <- factor(df_vizzz$Subset, levels = c("test", "validation", "train"))
# df_vizzz$Iteration <- factor(df_vizzz$Iteration, levels = c("3", "2", "1"))
# ggplot(df_vizzz, aes(x=Iteration, y=t, fill=Subset)) + geom_bar(stat = "identity") + coord_flip()  +
#   scale_fill_manual(values=group.colors, breaks=c("train", "validation", "test")) +
#   theme(legend.position="bottom") #+
#   #scale_fill_discrete(breaks=c("train", "validation", "test"), values=group.colors)
# group.colors <- c(train = "chartreuse4", validation = "goldenrod2", test ="darkmagenta")
### 800 x 300
