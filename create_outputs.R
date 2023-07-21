library(kableExtra)
library(knitr)
library(tibble)

source("performance_eval.R")
source("splits.R")

#test_results <- readRDS("mod_imgs_outer_res")
test_results2 <- readRDS("mod_lags_length_outer_res_nochmal")
evaluate_splits_rep_ho(test_results2, 2)
#evaluate_splits_rep_ho(test_results2)
#get_avg_cms(test_results, F)
avg_cms_latex <- get_avg_cms(test_results2, F, 3)
#
# result_lags <- test_results[[2]]
# result_lags_model <- test_results2[[1]][[4]]
# str(test_results2[[1]][[4]])
mod_lags_length_outer_res <- readRDS("mod_lags_length_outer_res")
mod_imgs_outer_res <- readRDS("mod_imgs_outer_res")
macro_scores <- evaluate_splits_rep_ho(mod_imgs_outer_res, 2)
cms_imgs <- get_avg_cms(mod_imgs_outer_res, F, 2)
build_cm_output(cms_imgs)
build_overview(list(mod_imgs_outer_res, mod_imgs_lags_outer_res),2)

mod_imgs_lags_outer_res <- readRDS("mod_imgs_lags_outer_res")
macro_scores1 <- evaluate_splits_rep_ho(mod_imgs_lags_outer_res, 2)
macro_scores2 <- evaluate_splits_rep_ho(mod_imgs_lags_outer_res, 3)
cms_imgs_lags_2 <- get_avg_cms(mod_imgs_lags_outer_res, F, 2)
cms_imgs_lags_3 <- get_avg_cms(mod_imgs_lags_outer_res, F, 3)

build_cm_output(cms_imgs)

####################################################### Status:TESTED and done
# TITLE: Creates dataframe combining different metrics from the confusion matrix
# cms: list of avg. confusion matrices from get_avg_cms()
# RETURN: dataframe with combined combinations
build_cm_output <- function(cms) {
  base_cm <- as.data.frame.matrix(round(cms[[1]], 2))
  base_cm <- base_cm[c(2,3,4,5,6,7,1),c(2,3,4,5,6,7,1)]
  print(class(base_cm))
  print(base_cm)
  # create additional columns
  col_1 <- round(colSums(base_cm),2)
  precision_tmp <- round(cms[[2]][,"Precision"],2)
  combined_cm <- base_cm %>% mutate("colSums" = col_1, Precision = unname(precision_tmp))

  # create additional rows
  recall_tmp <- unname(round(cms[[2]][,"Recall"],2))
  #sum_diag <- sum(diag(base_cm))
  row_1 <- c(rowSums(base_cm), NA, NA)
  row_2 <- c(recall_tmp, NA, NA)

  combined_cm <- rbind(combined_cm, row_1, row_2)
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

# Bsp. für Dataframe
cm_output <- build_cm_output(avg_cms_latex)
build_overview(list(mod_imgs_outer_res),2)
kable(cm_output, booktabs = TRUE, "latex") %>% add_header_above(c("Labels" = 10)) %>%
  #column_spec(1, bold = F, border_right = T) %>%
  column_spec(8, bold = F, border_right = T) %>%
  kable_styling(font_size = 11)

####################################################### Status: NOT Tested
# TITLE: Creates boxplots of predictions
# preidctions: list
# RETURN: dataframe with combined combinations
build_box_df <- function(precdictions, predicted_classes, pos, test_indices, df) {
  # alle prediction und pc obejcte aus allen splits zusammen fügen
  df_predictions <- rbind(predictions[[1]], predictions[[2]], predictions[[3]])
  vec_pc <- c(predicted_classes[[1]], c(predicted_classes[[1]]), c(predicted_classes[[3]]))
  vec_tc <- c(df$value[test_indices[[1]], ], df$value[test_indices[[2]], ], df$value[test_indices[[3]], ])
  df_predictions <- df_predictions %>% mutate(pc = vec_pc, tc = vec_tc)
  # pro Klasse dann df machen und die auch einfach aneinander batschen
  classes <- unique(df$value)
  df_boxplot <- data.frame()
  for (c in 1:length(classes)) {
    class <- classes[c]
    df_class <- df_predictions
    print("Befoooooore")
    print(str(df_class))
    print(df_class, n = 50)
    # create TP
    # create FP
    # create FN
    df_class <- df_class %>% mutate(type = case_when(
      tc == class & tc == pc ~ "TP",
      tc == class & tc != pc ~ "FN",
      tc != class & tc == pc ~ "FP",
      .default = NA
    )) %>% drop_na(type)
    print("AAAAAAFTER")
    print(str(df_class))
    print(df_class, n = 50)
    df_boxplot = rbind(df_boxplot, df_class)
  }
  df_boxplot
}
