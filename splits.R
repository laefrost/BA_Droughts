# TITLE: Selects indices of 3-way split (train - validation - test)
# start_year:
create_splits <- function(start_year, end_year, length_test, length_val, df_indcs, step){
  end_test <- seq(start_year, end_year, by = step)
  end_val <- end_test - length_test
  end_train <- end_val - length_val
  indcs_list <- vector(mode = "list", length(end_test))
  for (fold in 1:length(end_test)) {
    indcs_train <- which(df_indcs[,"year"] <= end_train[fold])
    indcs_val <-  which(df_indcs[,"year"] > end_train[fold] & df_indcs[,"year"] <= end_val[fold])
    indcs_test <-  which(df_indcs[,"year"] > end_val[fold] & df_indcs[,"year"] <= end_test[fold])
    indcs_list[[fold]] <- list(indcs_train, indcs_val, indcs_test)
  }
  indcs_list
}

create_splits_nested <- function(df_indcs){
  splits_outer <- create_splits(1990, 2010, 10, 10, df_indcs, 10)
  splits_inner <- vector(mode = "list", length = length(splits_outer))
   for (outer in 1:length(splits_outer)) {
     end_year_inner <- df[splits_outer[[outer]][[1]][length(splits_outer[[outer]][[1]])], "year"]
     splits_tmp <- create_splits(end_year_inner-10, end_year_inner, 5, 5, df_indcs, 5)
     splits_inner[[outer]] <- splits_tmp
   }
  list(splits_outer, splits_inner)
}
