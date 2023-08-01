### Info: Contains functions for split-creation of (nested) resampling
### Called from: models.R, models_evaluation.R

# TITLE: Creates list of train-val-test-indces for folds
# start_year: first start year of test sets
# end_year: last start year of test sets
# length_test: length of test set
# length_val: length of validation set,
# df: data frame with all data points
# step: nmb. of year between folds
# RETURN: list with indices per split
create_splits <- function(start_year, end_year, length_test, length_val, df, step){
  end_test <- seq(start_year, end_year, by = step)
  end_val <- end_test - length_test
  end_train <- end_val - length_val
  indcs_list <- vector(mode = "list", length(end_test))
  for (fold in 1:length(end_test)) {
    indcs_train <- which(df[,"year"] <= end_train[fold])
    indcs_val <-  which(df[,"year"] > end_train[fold] & df[,"year"] <= end_val[fold])
    indcs_test <-  which(df[,"year"] > end_val[fold] & df[,"year"] <= end_test[fold])
    indcs_list[[fold]] <- list(indcs_train, indcs_val, indcs_test)
  }
  indcs_list
}


# TITLE: Creates nested list with indices for nested resampling
# df: data frame with all data points
create_splits_nested <- function(df){
  splits_outer <- create_splits(1990, 2010, 10, 10, df, 10)
  splits_inner <- vector(mode = "list", length = length(splits_outer))
   for (outer in 1:length(splits_outer)) {
     end_year_inner <- df[splits_outer[[outer]][[1]][length(splits_outer[[outer]][[1]])], "year"]
     splits_tmp <- create_splits(end_year_inner-10, end_year_inner, 5, 5, df, 5)
     splits_inner[[outer]] <- splits_tmp
   }
  list(splits_outer, splits_inner)
}
