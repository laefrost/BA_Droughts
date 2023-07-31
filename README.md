# Combining additive regression and convolutional neural networks for classifying circulation patterns associated with droughts 

Implements [semi-structured distributional regression](https://github.com/neural-structured-additive-learning/deepregression) for the classification of circulation patterns associated with droughts. 

## Structure
The main purpose and most important functions of each subsection are sketched in the following.

### Preprocessing and EDA
1. `preprocessing.R`
  * Does data preprocessing, creates both y and data for deepregression, creates class weigths
  * Called from: `models.R`, `models_evaluation.R`, `EDA.R`

2. `EDA.R`
  * Does EDA for circulation patterns of interest 
### Modelling
1. `nested_resampling.R`
  * Contains functions for nested resampling (`rep_ho()`, `nested_resampling_final()`)
  * `rep_ho()`:
     * Implements repreated hold-out using a (train-validation-test split)
     * Returns list of either: (training history per split, list(predictions, confusion matrix, predicted_classes), list(predictions, confusion matrix, predicted_classes)) or
       (training history per split, list(predictions, confusion matrix, predicted_classes))
    * Called by: `nested_resampling_final.R`
  * `nested_resampling_final()`:
    * Implements repeated hold outsplitting
    * Returns list of return values from `rep_ho()`
  
2. `models.R`
  * Initializes various deepregression objects and does nested resampling (`nested_resampling_final()`) for them
  * Saves return values from `nested_resampling_final()` into .RDS-files

### Evaluation
1. `performance_evaluation_functions.R`
  * Contains functions for automated performance evaluation (e.g. create avg. Confusion matrices)
  * Saves .tex/.png for confusion matrices/plots generated
  * Called from: `models_evaluation.R`

**2. models_evaluation.R**
  * Reads RDS-files per model and does performance evaluation for `performance_evaluation_functions.R` for each

### Result data
* Model folders: Contain outputs form performance_evaluation_functions.R
* .RDS files: Contain return values from `nested_resampling_final()` per model
