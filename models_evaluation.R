source("nested_resampling.R")
source("preprocessing.R")
source("splits.R")
source("performance_evaluation_functions.R")
# -------------------------------- helper variables
splits_helper <- create_splits_nested(df)
test_indcs_helper <-
  lapply(splits_helper[[1]], function(split)
    split[[3]])
splits_old <- create_splits_nested(df_old)
test_indcs_helper_old <-
  lapply(splits_old[[1]], function(split)
    split[[3]])
# -------------------------------- Performance evaluation of models

# -------------------------------- Read all models
mod_imgs_outer_res <- readRDS("mod_imgs_outer_res")
mod_imgs_month_outer_res <- readRDS("mod_imgs_month_outer_res")
mod_imgs_season_outer_res <- readRDS("mod_imgs_season_outer_res")
mod_imgs_year_outer_res <- readRDS("mod_imgs_year_outer_res")
#mod_imgs_length_outer_res <- readRDS("mod_imgs_length_outer_res")
mod_imgs_lags_outer_res <- readRDS("mod_imgs_lags_outer_res")
mod_imgs_lags_length_outer_res <- readRDS("mod_imgs_lags_length_outer_res")
mod_imgs_season_year_outer_res <- readRDS("mod_imgs_season_year_outer_res")
mod_imgs_imgs_lagged_outer_res <- readRDS("mod_imgs_imgs_lagged_outer_res")
mod_imgs_imgs_lead_outer_res <- readRDS("mod_imgs_imgs_lead_outer_res")
mod_imgs_imgs_lagged_lead_outer_res <- readRDS("mod_imgs_imgs_lagged_lead_outer_res")
mod_imgs_imgs_lagged_outer_res_00001 <- readRDS("mod_imgs_imgs_lagged_outer_res_00001")
mod_imgs_imgs_lead_outer_res_00001 <- readRDS("mod_imgs_imgs_lead_outer_res_00001")
mod_imgs_imgs_lagged_lead_outer_res_00001 <- readRDS("mod_imgs_imgs_lagged_lead_outer_res_00001")
mod_imgs_imgs_lead_lags_outer_res_00001 <- readRDS("mod_imgs_imgs_lead_lags_outer_res_00001")
mod_imgs_imgs_lagged_lead_season_year_outer_res <- readRDS("mod_imgs_imgs_lagged_lead_season_year_outer_res")

#mod_imgs_imgs_lagged_lead_combined_outer_res <- readRDS("mod_imgs_imgs_lagged_lead_combined_outer_res")
#mod_imgs_imgs_lagged_combined_outer_res <- readRDS("mod_imgs_imgs_lagged_combined_outer_res")
#mod_imgs_lags_length_lead_image_outer_res <- readRDS("mod_imgs_lags_length_lead_image_outer_res")
#mod_imgs_lags_lead_image_outer_res <- readRDS("mod_imgs_lags_lead_image_outer_res")

list_models <- list(
  mod_imgs_outer_res,
  mod_imgs_month_outer_res,
  mod_imgs_season_outer_res,
  mod_imgs_year_outer_res,
  mod_imgs_lags_outer_res,
  mod_imgs_lags_length_outer_res,
  mod_imgs_season_year_outer_res,
  mod_imgs_imgs_lagged_outer_res,
  mod_imgs_imgs_lead_outer_res,
  mod_imgs_imgs_lagged_lead_outer_res
  #mod_imgs_imgs_lead_lags_outer_res_00001
)

str(mod_imgs_imgs_lagged_lead_outer_res)


list_models_name <- list(
  "I",
  "I, M",
  "I, S",
  "I, Y",
  "I, L",
  "I, L*Le",
  "I, S, Y",
  "I, I-1",
  "I, I+1",
  "I, I-1, I+1")
  #"I, L, I+1")


table(df$transition)
table(df$transition_last)
table(df$transition_last)
# -------------------------------- Build overview
ov <- build_overview(list_models, list_models_name, list_models_name)
ov1 <- build_overview_transposed(list_models_name, list_models)
ov_2 <- build_overview_transposed(list_models_name, list_models)

build_overview_transposed(list_models_name, list_models)
# -------------------------------- Do evaluation pipeline for each model
eval_pipe(mod_imgs_outer_res,
          F,
          "imgs",
          splits_old,
          df_old,
          test_indcs_helper_old)

eval_pipe(mod_imgs_lags_outer_res,
           T,
           "lags",
           splits_old,
           df_old,
          test_indcs_helper_old)

plot(mod_imgs_outer_res[[3]][[1]])

df_predictors <- data.frame("Model" = unlist(list_models_name),
                            "Structured" = c("-", "season", "month", "s(year)",
                                             "lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6",
                                             "lag_1 + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + curr_length:lag_1",
                                             "season + s(year)", "-", "-", "-"),
                            "Unstructured" = c(rep("d(images)", 7), "d(images) +  d(images_-1)",
                                               "d(images) + d(images_+1)",
                                               "d(images) +  d(images_-1) + d(images_+1)"))

kable(df_predictors, booktabs = TRUE, "latex") %>%
  column_spec(1, bold = F, border_right = T) %>%
  kable_styling(font_size = 11)



# cms_wi <- get_avg_cms(mod_imgs_lags_length_outer_res, F, 3)
# build_cm_output(cms_wi, "imgs_lags_length_wi")
# cms_transitions_forecast <- inspect_transitions(mod_imgs_lags_length_outer_res, splits_helper, F, 3)
# build_cm_output(cms_transitions_forecast[c(1:2)], "test")
# build_overview(list(xyz, xyz2), 2)
# build_overview(list(xyz, xyz2), 3)
#
# get_avg_cms(xyz, F, 3)
#
#
# df_boxplot_test <- build_box_df(pred_test, pc_test, 2, test_indcs_helper, df)
#
# test_indcs_helper[[1]]
# get_avg_cms(mod_imgs_lags_length_outer_res, F, 3)
#
# get_result_element(mod_imgs_lags_length_outer_res, 2, 2)



# # Bsp. für Dataframe
# cm_output <- build_cm_output(avg_cms_latex)
# build_overview(list(mod_imgs_outer_res),2)
# kable(cm_output, booktabs = TRUE, "latex") %>% add_header_above(c("Labels" = 10)) %>%
#   #column_spec(1, bold = F, border_right = T) %>%
#   column_spec(8, bold = F, border_right = T) %>%
#   kable_styling(font_size = 11)


df_splits_viz <- data.frame(matrix(nrow = 4, ncol = 3))
  # outer trainings splits
df_splits_viz[1,] <-  c("1900-1970", "1900-1980", "1900-1990")
df_splits_viz[2,] <-   c("1900-1950, 1950-1955, 1955-1960",
                         "1900-1960, 1960-1965, 1965-1970",
                         "1900-1970, 1970-1975, 1975-1980")
df_splits_viz[3,] <-   c("1900-1955, 1955-1960, 1960-1965", "1900-1965, 1965-1970, 1965-1975", "1900-1975, 1970-1980, 1975-1985")
df_splits_viz[4,] <-   c("1900-1960, 1960-1965, 1965-1970", "1900-1970, 1970-1975, 1975-1980", "1900-1980, 1980-1985, 1985-1990")


colnames(df_splits_viz) <- c("outer1", "outer2", "outer3")
rownames(df_splits_viz) <- c("trainingsset outer", "1inner", "2inner", "3inner")

kable(df_splits_viz, booktabs = TRUE, "latex") %>% #add_header_above(c("Labels" = 10)) %>%
   #column_spec(1, bold = F, border_right = T) %>%
   # column_spec(8, bold = F, border_right = T) %>%
   kable_styling(font_size = 11)
