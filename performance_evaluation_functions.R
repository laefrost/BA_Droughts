source("nested_resampling.R")
source("preprocessing.R")
source("splits.R")
library(ggplot2)
library(knitr)
library(kableExtra)


####################################################### Status: NOT Tested
# TITLE: returns elements of result-list from nested_resamling_final()
# element: integer (1,2,3) indicateing which element to retrieve
# pos: integer (2,3) indicating whether to get the is-cms or the oos-cms
# RETURN: list of elements from splits
get_result_element <- function(results, element, pos){
  elements <- lapply(results, function(result)
    result[[pos]][[element]])
  elements
}

####################################################### Status: Tested and done
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
  if(!cm_extracted)
    cms <- get_result_element(cms, 2, pos)

  avg_cm_table <-
    Reduce('+', lapply(cms, function(cm)
      replace_na(cm$table))) / length(cms)
  avg_cm_by_class <-
    Reduce('+', lapply(cms, function(cm)
      replace_na(cm$byClass))) / length(cms)
  return(list(avg_cm_table, avg_cm_by_class))
}


####################################################### Status: Tested and done
# TITLE: computes cms for trainsition days
# predictions:
# cm_extracted: boolean, T if cms == list of cm objects; F if cms == result object from nested_resampling()
# pos: integer (2,3) indicating whether to get the is-cms or the oos-cms
# RETURN: list of avg. cms (table, byClass)
inspect_transitions <- function(predictions, splits, predictions_extracted, pos){
  if (!predictions_extracted)
    predictions <- get_result_element(predictions, 1, pos)

  df$index <- 1:nrow(df)
  splits_outer <- splits[[1]]
  cms <- list()
  for (outer in 1:length(splits_outer)) {
    test_indcs <- splits_outer[[outer]][[3]]
    transition_days_df <-  df[(test_indcs),] %>% filter(transition == T)
    predictions_outer_df <- as.data.frame(predictions[[outer]]) %>% mutate(index = test_indcs)
    transition_predictions_df <- merge(x=transition_days_df,y=predictions_outer_df,
                                       by="index")

    predictions_pure <- transition_predictions_df %>% select(other, BM, HFA, HNA, HNFA, NEA, SEA)
    predicted_classes_tmp <- factor(colnames(predictions_pure)[max.col(predictions_pure)], levels(df$value))
    cms[[outer]] <- confusionMatrix(predicted_classes_tmp, transition_predictions_df$value)
  }
  avg_cms <- get_avg_cms(cms, T)
  cmb_cms <- c(avg_cms, cms)
  cmb_cms
}

df$index = 1:nrow(df)
df[hel, ]
####################################################### Status:TESTED and done
# TITLE: Creates dataframe combining different metrics from the confusion matrix, saves it into .tex file
# cms: list of avg. confusion matrices from get_avg_cms()
# RETURN: dataframe with combined combinations
build_cm_output <- function(avg_cms, model_name) {
  base_cm <- as.data.frame.matrix(round(avg_cms[[1]], 2))
  base_cm <- base_cm[c(2,3,4,5,6,7,1),c(2,3,4,5,6,7,1)]
  print(avg_cms[[2]])
  # create additional columns
  col_1 <- round(colSums(base_cm),2)
  precision_tmp <- unname(round(avg_cms[[2]][,"Precision"],2))
  precision_tmp <- precision_tmp[c(2,3,4,5,6,7,1)]
  combined_cm <- base_cm %>% mutate("colSums" = col_1, Precision = (precision_tmp))

  # create additional rows
  recall_tmp <- unname(round(avg_cms[[2]][,"Recall"],2))
  recall_tmp <- recall_tmp[c(2,3,4,5,6,7,1)]
  #sum_diag <- sum(diag(base_cm))
  row_1 <- c(rowSums(base_cm), NA, NA)
  row_2 <- c(recall_tmp, NA, NA)

  combined_cm <- rbind(combined_cm, row_1, row_2)

  cm_tex <- kable(combined_cm, booktabs = TRUE, "latex") %>% add_header_above(c("Labels" = 10)) %>%
    column_spec(8, bold = F, border_right = T) %>%
    kable_styling(font_size = 11)
  writeLines(cm_tex, paste0(model_name, ".tex"))

  combined_cm
}

####################################################### Status:TESTED and done
# TITLE: Creates dataframe combining overview metrics of models
# cms: list of model results
# RETURN: dataframe with combined combinations
build_overview <- function(list_models_results, pos) {
  overview_df <- data.frame("Avg.Macro F1-Score" = rep(0, length(list_models_results)),
                            "Avg.balanced Accuracy" = rep(0, length(list_models_results)))
  for (model in 1:length(list_models_results)){
    overview_df[model, ] <- evaluate_splits_rep_ho(list_models_results[[model]], pos)
  }
  overview_df
}


####################################################### Status: NOT Tested
# TITLE: Creates df for boxplots of predictions
# preidctions: list
# RETURN: dataframe with combined combinations
build_box_df <- function(predictions, predicted_classes, pos, test_indices, df) {
  # alle prediction und pc obejcte aus allen splits zusammen fügen
  df_predictions <- as.data.frame.matrix(rbind(predictions[[1]], predictions[[2]], predictions[[3]]))
  vec_pc <- c(predicted_classes[[1]], c(predicted_classes[[2]]), c(predicted_classes[[3]]))
  vec_tc <- c(df$value[test_indices[[1]]], df$value[test_indices[[2]]], df$value[test_indices[[3]]])
  df_predictions <- df_predictions %>% mutate(pc = vec_pc, tc = vec_tc)
  # pro Klasse dann df machen und die auch einfach aneinander batschen
  classes <- unique(df$value)
  df_boxplot <- data.frame()
  for (c in 1:length(classes)) {
    class <- classes[c]
    print(class)
    df_class <- df_predictions
    df_class <- df_class %>% mutate(curr_class = class, type = case_when(
      tc == class & tc == pc ~ "TP",
      tc == class & tc != pc ~ "FN",
      tc != class & class == pc ~ "FP",
      .default = NA
    )) %>% mutate(p_predicted = ) #%>% drop_na(type)
    print(str(df_class))
    df_class$p_predicted <- df_predictions[, pc]
    df_class$p_true <- df_predictions[, tc]
    df_class <- df_class %>% drop_na(type)
    print("AAAAAAFTER")
    print(str(df_class))
    print(df_class)
    df_boxplot = rbind(df_boxplot, df_class)
  }
  df_boxplot
}

eval_pipe <- function(model_result, bool_forecast, model_name, splits){
  predictions <- get_result_element(model_result, 1, 2)
  # create output for allg. cms
  cms_pred <- get_avg_cms(model_result, F, 2)
  build_cm_output(cms_pred, paste0(model_name, "_pred"))
  # create output for forecast cms if existing
  # create transition output
  cms_transitions <- inspect_transitions(model_result, splits, F, 2)
  cms_transitions <- cms_transitions[c(1,2)]
  build_cm_output(cms_transitions, paste0(model_name, "_transition_pred"))

  if (bool_forecast) {
    cms_forecast <- get_avg_cms(model_result, F, 3)
    build_cm_output(cms_forecast, paste0(model_name, "_forecast"))
    cms_transitions_forecast <- inspect_transitions(model_result, splits, F, 3)
    cms_transitions_forecast <- cms_transitions_forecast[c(1,2)]
    build_cm_output(cms_transitions_forecast, paste0(model_name, "_transition_forecast"))
  }

  # create One vs. ALL ROC's and save them in one plot ---> ToDo: write function
  # create Boxplot: Wirte function
}





mod_imgs_outer_res <- readRDS("mod_imgs_outer_res")
mod_imgs_lags_length_outer_res <- readRDS("mod_imgs_lags_length_outer_res")
splits_helper <- create_splits_nested(df)
test_indcs_helper <- lapply(splits_helper[[1]], function(split) split[[3]])

df[test_indcs_helper[[1]],]

test_indcs_helper[[1]]

pred_test <- get_result_element(mod_imgs_outer_res, 1, 2)
pc_test <- get_result_element(mod_imgs_outer_res, 3, 2)

cms_wi <- get_avg_cms(mod_imgs_lags_length_outer_res, F, 2)
build_cm_output(cms_wi, "imgs_lags_length_wi")

cms_transitions_forecast <- inspect_transitions(mod_imgs_lags_length_outer_res, splits_helper, F, 3)
build_cm_output(cms_transitions_forecast[c(1:2)], "test")







df_boxplot_test <- build_box_df(pred_test, pc_test, 2, test_indcs_helper, df)

test_indcs_helper[[1]]
get_avg_cms(mod_imgs_lags_length_outer_res, F, 3)

get_result_element(mod_imgs_lags_length_outer_res, 2, 2)


str(mod_imgs_lags_length_outer_res)
p<-ggplot(df_boxplot_test, aes(x=tc, y=rel_predict, fill=type)) +
  geom_boxplot(position=position_dodge(1))
p

# bsp fpür tc = other
df_boxplot_other <- df_boxplot_test %>% filter(curr_class == "other" , type == "FN") %>% select(curr_class, p_predicted, p_true, pc, tc)

ggplot(df_boxplot_other, aes(x=type, y=rel_predict, fill=pc)) +
  geom_boxplot(position=position_dodge(1))


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
