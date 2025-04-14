# Check and install required packages
required_packages <- c("shiny", "DT", "ggplot2", "dplyr", "GGally", "corrplot", 
                      "shinyWidgets", "caret", "tidyr", "randomForest", 
                      "xgboost", "e1071", "gbm", "earth")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load libraries
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(GGally)
library(corrplot)
library(shinyWidgets)
library(caret)
library(tidyr)
library(randomForest)
library(xgboost)
library(e1071)
library(gbm)
library(earth)

# UI Definition
ui <- fluidPage(
  titlePanel("Collaborative Data Analysis & Prediction Workspace"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      
      tags$hr(),
      tags$h4("Column Management"),
      uiOutput("column_selector_ui"),
      actionButton("drop_selected_columns", "Drop Selected Columns", class = "btn-warning"),
      verbatimTextOutput("drop_columns_message"),
      tags$hr(),
      
      uiOutput("drop_rows_ui"),
      
      checkboxInput("auto_clean", "Automatic Cleaning", FALSE),
      
      tags$h4("Prediction"),
      uiOutput("prediction_target_ui"),
      
      uiOutput("model_selection_ui"),
      
      selectInput("cv_method", "Cross-Validation Method:",
                  choices = c("None" = "none", 
                              "K-Fold" = "cv", 
                              "Repeated K-Fold" = "repeatedcv"),
                  selected = "cv"),
      numericInput("cv_k", "Number of Folds (K):", 5, min = 2, max = 20),
      conditionalPanel(
        condition = "input.cv_method == 'repeatedcv'",
        numericInput("cv_repeats", "Number of Repeats:", 3, min = 1, max = 10)
      ),
      
      uiOutput("predictor_inputs_ui"),
      actionButton("run_prediction", "Run Prediction", class = "btn-primary"),
      
      tags$hr(),
      tags$h4("Analysis Options"),
      uiOutput("heatmap_columns_ui"),
      actionButton("remove_outliers", "Remove Outliers", class = "btn-danger")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Overview", 
                 dataTableOutput("data_preview"), 
                 verbatimTextOutput("data_summary"),
                 verbatimTextOutput("row_count_before"),
                 verbatimTextOutput("col_count_before"),
                 verbatimTextOutput("row_count_after"),
                 verbatimTextOutput("col_count_after")),
        tabPanel("Column Names", verbatimTextOutput("column_names")),
        tabPanel("Numeric Columns", verbatimTextOutput("numeric_columns")),
        tabPanel("Null Values", verbatimTextOutput("null_values")),
        tabPanel("Duplicate Rows", verbatimTextOutput("duplicates")),
        tabPanel("Outlier Detection", 
                 fluidRow(
                   column(6, plotOutput("boxplot")),
                   column(6, plotOutput("scatterplot"))
                 ),
                 verbatimTextOutput("outlier_count")),
        tabPanel("Data Visualization",
                 selectInput("plot_type", "Select Plot Type",
                             choices = c("Pair Plot", "Scatter Plot", "Box Plot", 
                                         "Line Plot", "Residual Plot", "Q-Q Plot", "Histogram"),
                             selected = "Pair Plot"),
                 uiOutput("plot_specific_ui"),
                 plotOutput("data_viz_plot", height = "600px", width = "600px")),
        tabPanel("Heatmap", 
                 plotOutput("heatmap", height = "600px", width = "600px")),
        tabPanel("Model Comparison", 
                 verbatimTextOutput("model_comparison_results"),
                 plotOutput("model_comparison_plot", height = "400px", width = "600px")),
        tabPanel("Prediction Results", 
                 verbatimTextOutput("prediction_results"),
                 verbatimTextOutput("model_summary"),
                 plotOutput("variable_importance_plot", height = "400px", width = "600px"))
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Initialize reactive values
  original_data <- reactiveVal(NULL)
  cleaned_data <- reactiveVal(NULL)
  current_model <- reactiveVal(NULL)
  all_models <- reactiveVal(list())
  best_model <- reactiveVal(NULL)
  model_data <- reactiveVal(NULL)
  preproc_obj <- reactiveVal(NULL)  # Store preprocessing object
  drop_message <- reactiveVal("")
  
  # Load data when file is uploaded
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      df <- read.csv(input$file$datapath, na.strings = c("", "NA", "?"))
      if (nrow(df) == 0 || ncol(df) == 0) {
        showNotification("Uploaded CSV is empty.", type = "error")
        return()
      }
      original_data(df)
      cleaned_data(df)
      drop_message("")
    }, error = function(e) {
      showNotification(paste("Error reading CSV:", e$message), type = "error")
    })
  })
  
  # [Previous UI and data management code remains unchanged until run_prediction]
  
  # Preprocess data function (enhanced with multicollinearity check)
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
    # Check for multicollinearity
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
    return(list(success = TRUE, data = final_df_processed, preproc = preproc, message = "Data preprocessing successful."))
  }
  
  # Run prediction
  observeEvent(input$run_prediction, {
    req(cleaned_data(), input$target_variable, input$selected_models)
    
    tryCatch({
      if (length(input$selected_models) == 0) {
        output$prediction_results <- renderPrint({
          "Error: Please select at least one regression model."
        })
        return()
      }
      
      df <- cleaned_data()
      target <- input$target_variable
      
      # Preprocess data
      result <- preprocess_data(df, target)
      if (!result$success) {
        output$prediction_results <- renderPrint({
          cat("Preprocessing Error:", result$message, "\n")
        })
        output$model_summary <- renderPrint({ NULL })
        return()
      }
      model_data(result$data)
      preproc_obj(result$preproc)
      
      # Log preprocessed data dimensions
      cat("Preprocessed data dimensions:", dim(result$data), "\n")
      
      # Set up cross-validation
      if (input$cv_method == "none") {
        ctrl <- trainControl(method = "none")
      } else if (input$cv_method == "cv") {
        ctrl <- trainControl(method = "cv", number = input$cv_k, verboseIter = FALSE)
      } else {
        ctrl <- trainControl(method = "repeatedcv", number = input$cv_k, repeats = input$cv_repeats, verboseIter = FALSE)
      }
      
      # Train models
      model_results <- list()
      model_list <- list()
      formula <- as.formula(paste(target, "~ ."))
      
      withProgress(message = "Training models...", value = 0, {
        for (model_type in input$selected_models) {
          incProgress(1/length(input$selected_models), detail = paste("Training", model_type))
          tryCatch({
            tune_grid <- NULL
            actual_model_type <- model_type
            
            if (model_type == "glmnet") {
              tune_grid <- expand.grid(alpha = 0, lambda = seq(0.001, 1, length.out = 10))
            } else if (model_type == "lasso") {
              tune_grid <- expand.grid(alpha = 1, lambda = seq(0.001, 1, length.out = 10))
              actual_model_type <- "glmnet"
            } else if (model_type == "rf") {
              tune_grid <- expand.grid(mtry = min(floor(sqrt(ncol(result$data) - 1)), ncol(result$data) - 1))
            } else if (model_type == "xgbTree") {
              tune_grid <- expand.grid(
                nrounds = 100, max_depth = 6, eta = 0.3, gamma = 0,
                colsample_bytree = 1, min_child_weight = 1, subsample = 1
              )
            } else if (model_type == "svmRadial") {
              tune_grid <- expand.grid(sigma = 0.1, C = 1)
            } else if (model_type == "gbm") {
              tune_grid <- expand.grid(
                n.trees = 100, interaction.depth = 3, shrinkage = 0.1, n.minobsinnode = 10
              )
            } else if (model_type == "earth") {
              tune_grid <- expand.grid(degree = 1, nprune = 10)
            }
            
            model <- train(
              formula,
              data = result$data,
              method = actual_model_type,
              trControl = ctrl,
              tuneGrid = tune_grid,
              preProcess = NULL,  # Already preprocessed
              metric = "RMSE",
              na.action = na.omit,
              verbose = FALSE
            )
            model_list[[model_type]] <- model
            predictions <- predict(model, newdata = result$data)
            rmse <- sqrt(mean((result$data[[target]] - predictions)^2, na.rm = TRUE))
            mape <- mean(abs((result$data[[target]] - predictions) / result$data[[target]]), na.rm = TRUE) * 100
            r_squared <- tryCatch({
              summary(lm(predictions ~ result$data[[target]]))$r.squared
            }, error = function(e) NA)
            model_results[[model_type]] <- list(
              RMSE = rmse,
              MAPE = mape,
              R_Squared = r_squared,
              model = model
            )
            cat(sprintf("Model %s trained successfully. RMSE: %.2f\n", model_type, rmse))
          }, error = function(e) {
            model_results[[model_type]] <<- list(
              RMSE = NA,
              MAPE = NA,
              R_Squared = NA,
              Error = paste("Error training", model_type, ":", as.character(e))
            )
            cat(sprintf("Error training %s: %s\n", model_type, as.character(e)))
          })
        }
      })
      
      # Identify best model, proceed even if some models fail
      valid_models <- model_results[!sapply(model_results, function(x) is.na(x$RMSE))]
      if (length(valid_models) == 0) {
        output$prediction_results <- renderPrint({
          cat("Error: No valid models could be trained. Check the console for details.\n")
        })
        output$model_summary <- renderPrint({ NULL })
        return()
      }
      
      rmse_values <- sapply(valid_models, function(x) x$RMSE)
      best_model_name <- names(which.min(rmse_values))
      best_model(model_list[[best_model_name]])
      all_models(model_list)
      
      # Gather new inputs
      new_data <- data.frame(row.names = 1)
      for (col in names(df)) {
        if (col != target) {
          input_name <- paste0("input_", make.names(col))
          val <- input[[input_name]]
          if (!is.null(val)) {
            new_data[[col]] <- val
          } else {
            if (is.numeric(df[[col]])) {
              new_data[[col]] <- mean(df[[col]], na.rm = TRUE)
            } else {
              unique_vals <- unique(df[[col]])
              unique_vals <- unique_vals[!is.na(unique_vals)]
              new_data[[col]] <- unique_vals[1]
            }
          }
        }
      }
      
      # Preprocess new_data
      new_data_processed <- predict(preproc_obj(), new_data)
      # Ensure column alignment
      common_cols <- intersect(names(result$data)[names(result$data) != target], names(new_data_processed))
      new_data_processed <- new_data_processed[, common_cols, drop = FALSE]
      template <- result$data[1, names(result$data) != target]
      for (col in setdiff(names(template), names(new_data_processed))) {
        new_data_processed[[col]] <- 0  # Default for missing columns
      }
      new_data_processed <- new_data_processed[, names(template)]
      # Convert to data frame with proper structure
      new_data_processed <- as.data.frame(new_data_processed)
      
      # Make prediction
      prediction_value <- tryCatch({
        predict(best_model(), newdata = new_data_processed)
      }, error = function(e) {
        cat("Prediction error:", as.character(e), "\n")
        paste("Error making prediction:", as.character(e))
      })
      
      # Display results
      output$prediction_results <- renderPrint({
        cat("Model Comparison Results:\n")
        metrics_df <- do.call(rbind, lapply(names(model_results), function(name) {
          res <- model_results[[name]]
          if (!is.na(res$RMSE)) {
            data.frame(Model = name, RMSE = res$RMSE, MAPE = res$MAPE, R_Squared = res$R_Squared)
          } else {
            data.frame(Model = name, RMSE = NA, MAPE = NA, R_Squared = NA, Error = res$Error)
          }
        }))
        print(metrics_df)
        cat("\nBest Model:", best_model_name, "\n")
        cat("\nPrediction for target variable", target, ":", prediction_value, "\n")
      })
      
      output$model_summary <- renderPrint({
        cat("Best Model Summary:\n")
        print(best_model())
      })
      
      output$variable_importance_plot <- renderPlot({
        req(best_model())
        has_importance <- FALSE
        tryCatch({
          if ("varImp" %in% methods(class = class(best_model())[1])) {
            importance <- varImp(best_model(), scale = TRUE)
            imp_df <- as.data.frame(importance$importance)
            validate(need(nrow(imp_df) > 0, "No variable importance data available."))
            imp_df$Variable <- rownames(imp_df)
            imp_df$Importance <- imp_df$Overall
            imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]
            if (nrow(imp_df) > 15) {
              imp_df <- imp_df[1:15, ]
            }
            ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
              geom_bar(stat = "identity", fill = "steelblue") +
              coord_flip() +
              labs(title = "Variable Importance", x = "Variables", y = "Importance") +
              theme_minimal()
            has_importance <- TRUE
          }
        }, error = function(e) {
          has_importance <- FALSE
        })
        if (!has_importance) {
          plot.new()
          title("Variable importance not available for this model type")
        }
      })
      
      output$model_comparison_plot <- renderPlot({
        req(model_results)
        metrics_df <- do.call(rbind, lapply(names(model_results), function(name) {
          res <- model_results[[name]]
          if (!is.na(res$RMSE)) {
            data.frame(Model = name, RMSE = res$RMSE, MAPE = res$MAPE, R_Squared = res$R_Squared)
          } else {
            NULL
          }
        }))
        metrics_df <- na.omit(metrics_df)
        validate(need(nrow(metrics_df) > 0, "No valid models to compare."))
        metrics_long <- tidyr::pivot_longer(
          metrics_df, 
          cols = c("RMSE", "MAPE", "R_Squared"),
          names_to = "Metric", 
          values_to = "Value"
        )
        ggplot(metrics_long, aes(x = Model, y = Value, fill = Model)) +
          geom_bar(stat = "identity") +
          facet_wrap(~ Metric, scales = "free") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Model Comparison")
      })
      
      output$model_comparison_results <- renderPrint({
        req(model_results)
        cat("Model Comparison Results:\n")
        metrics_df <- do.call(rbind, lapply(names(model_results), function(name) {
          res <- model_results[[name]]
          if (!is.na(res$RMSE)) {
            data.frame(Model = name, RMSE = res$RMSE, MAPE = res$MAPE, R_Squared = res$R_Squared)
          } else {
            data.frame(Model = name, RMSE = NA, MAPE = NA, R_Squared = NA, Error = res$Error)
          }
        }))
        print(metrics_df)
      })
      
    }, error = function(e) {
      output$prediction_results <- renderPrint({
        paste("Error during prediction:", as.character(e))
      })
      output$model_summary <- renderPrint({ NULL })
      output$variable_importance_plot <- renderPlot({
        plot.new()
        title("Prediction failed")
      })
      output$model_comparison_results <- renderPrint({
        cat("Error: Model comparison could not be performed. Check the console for details.\n")
      })
      output$model_comparison_plot <- renderPlot({
        plot.new()
        title("Model comparison failed")
      })
      showNotification("Prediction failed. Check your inputs and dataset.", type = "error")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)