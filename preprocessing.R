# preprocessing.R
preprocess_data <- function(df, target) {
  if (!target %in% names(df)) {
    return(list(success = FALSE, message = "Target variable not found in dataset."))
  }
  model_df <- df
  if (!is.numeric(model_df[[target]])) {
    return(list(success = FALSE, message = "Target variable must be numeric for regression."))
  }
  predictors <- setdiff(names(model_df), target)
  if (length(predictors) == 0) {
    return(list(success = FALSE, message = "No predictor variables available."))
  }
  model_df <- model_df[!is.na(model_df[[target]]), ]
  if (nrow(model_df) == 0) {
    return(list(success = FALSE, message = "All rows have missing values in target variable."))
  }
  for (col in predictors) {
    if (is.numeric(model_df[[col]])) {
      if (all(is.na(model_df[[col]]))) {
        model_df[[col]] <- 0
      } else {
        model_df[[col]][is.na(model_df[[col]])] <- mean(model_df[[col]], na.rm = TRUE)
      }
    } else {
      if (all(is.na(model_df[[col]]))) {
        model_df[[col]] <- "Missing"
      } else {
        most_frequent <- names(sort(table(model_df[[col]]), decreasing = TRUE))[1]
        model_df[[col]][is.na(model_df[[col]])] <- most_frequent
      }
      model_df[[col]] <- as.factor(model_df[[col]])
    }
  }
  if (nrow(model_df) <= length(predictors)) {
    return(list(success = FALSE, message = "Not enough data points after preprocessing."))
  }
  cat_predictors <- names(model_df)[sapply(model_df, is.factor)]
  if (length(cat_predictors) > 0) {
    dmy <- dummyVars(~ ., data = model_df[, cat_predictors, drop = FALSE])
    cat_matrix <- predict(dmy, model_df)
    numeric_predictors <- setdiff(predictors, cat_predictors)
    numeric_data <- model_df[, c(target, numeric_predictors), drop = FALSE]
    final_df <- cbind(numeric_data, cat_matrix)
  } else {
    final_df <- model_df
  }
  colnames(final_df) <- make.names(colnames(final_df), unique = TRUE)
  nzv_info <- nearZeroVar(final_df, saveMetrics = TRUE)
  if (any(nzv_info$nzv)) {
    drop_cols <- rownames(nzv_info)[nzv_info$nzv]
    drop_cols <- setdiff(drop_cols, target)
    if (length(drop_cols) > 0) {
      final_df <- final_df[, !names(final_df) %in% drop_cols, drop = FALSE]
    }
  }
  num_cols <- names(final_df)[sapply(final_df[, -which(names(final_df) == target)], is.numeric)]
  if (length(num_cols) > 1) {
    cor_matrix <- cor(final_df[, num_cols], use = "complete.obs")
    high_corr <- findCorrelation(cor_matrix, cutoff = 0.9, names = TRUE)
    if (length(high_corr) > 0) {
      final_df <- final_df[, !names(final_df) %in% high_corr]
    }
  }
  preproc <- preProcess(final_df[, -which(names(final_df) == target)], method = c("center", "scale"))
  final_df_processed <- predict(preproc, final_df)
  return(list(success = TRUE, data = final_df_processed, preproc = preproc, dmy = dmy, message = "Data preprocessing successful."))
}