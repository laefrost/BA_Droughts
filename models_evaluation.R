### INFO: Loads all fitted models from .RDS files and triggers performance
### evaluation pipeline

source("preprocessing.R")
source("splits.R")
source("nested_resampling.R")
source("performance_evaluation_functions.R")

# -------------------------------- helper variables
splits_helper <- create_splits_nested(df)
test_indcs_helper <-
  lapply(splits_helper[[1]], function(split)
    split[[3]])
# -------------------------------- Performance evaluation of models
### Read all models
mod_imgs_outer_res <- readRDS("mod_imgs_outer_res")
mod_imgs_month_outer_res <- readRDS("mod_imgs_month_outer_res")
mod_imgs_season_outer_res <- readRDS("mod_imgs_season_outer_res")
mod_imgs_year_outer_res <- readRDS("mod_imgs_year_outer_res")
mod_imgs_lags_outer_res <- readRDS("mod_imgs_lags_outer_res")
mod_imgs_lags_year_outer_res <-
  readRDS("mod_imgs_lags_year_outer_res")
mod_imgs_lags_length_outer_res <-
  readRDS("mod_imgs_lags_length_outer_res")
mod_imgs_season_year_outer_res <-
  readRDS("mod_imgs_season_year_outer_res")
mod_imgs_imgs_lagged_outer_res <-
  readRDS("mod_imgs_imgs_lagged_outer_res")
mod_imgs_imgs_lead_outer_res <-
  readRDS("mod_imgs_imgs_lead_outer_res")
mod_imgs_imgs_lagged_lead_outer_res <-
  readRDS("mod_imgs_imgs_lagged_lead_outer_res")


list_models <- list(
  mod_imgs_outer_res,
  mod_imgs_month_outer_res,
  mod_imgs_season_outer_res,
  mod_imgs_year_outer_res,
  mod_imgs_season_year_outer_res,
  mod_imgs_lags_outer_res,
  mod_imgs_lags_length_outer_res,
  mod_imgs_lags_year_outer_res,
  mod_imgs_imgs_lagged_outer_res,
  mod_imgs_imgs_lead_outer_res,
  mod_imgs_imgs_lagged_lead_outer_res
)

list_models_name <- list(
  "I",
  "I_M",
  "I_S",
  "I_Y",
  "I_S_Y",
  "I_L",
  "I_L_Le",
  "I_L_Y",
  "I_I-1",
  "I_I+1",
  "I_I-1_I+1"
)

list_models_ov <- list(
  "I",
  "I, M",
  "I, S",
  "I, Y",
  "I, S, Y",
  "I, L",
  "I, L, Le",
  "I, L, Y",
  "I, I-1",
  "I, I+1",
  "I, I-1, I+1"
)



# -------------------------------- Build overview
build_overview_transposed(list_models_ov, list_models, test_indcs_helper, y)
# -------------------------------- Do evaluation pipeline for each model
for (i in 1:length(list_models)) {
  model <- list_models[[i]]
  bool_forecast <- ifelse(length(model[[1]]) == 3, T, F)
  print(list_models_name[[i]])
  eval_pipe(
    model,
    bool_forecast,
    paste0(list_models_name[[i]]),
    splits_helper,
    df,
    test_indcs_helper
  )
}
