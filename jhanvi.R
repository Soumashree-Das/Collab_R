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
library(earth) # For MARS models

ui <- fluidPage(
  titlePanel("Collaborative Data Analysis & Prediction Workspace"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      
      # Column Management
      tags$hr(),
      tags$h4("Column Management"),
      uiOutput("column_selector_ui"),
      actionButton("drop_selected_columns", "Drop Selected Columns", class = "btn-warning"),
      verbatimTextOutput("drop_columns_message"),
      tags$hr(),
      
      # Row dropping
      uiOutput("drop_rows_ui"),
      
      # Auto-cleaning option
      checkboxInput("auto_clean", "Automatic Cleaning", FALSE),
      
      # Prediction inputs
      tags$h4("Prediction"),
      uiOutput("prediction_target_ui"),
      
      # Model selection - NEW
      uiOutput("model_selection_ui"),
      
      # Cross-validation options - NEW
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
      
      # Heatmap and outlier controls
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
                 verbatimTextOutput("outlier_count")
        ),
        tabPanel("Data Visualization",
                 selectInput("plot_type", "Select Plot Type",
                             choices = c("Pair Plot", "Scatter Plot", "Box Plot", 
                                         "Line Plot", "Residual Plot", "Q-Q Plot", "Histogram"),
                             selected = "Pair Plot"),
                 uiOutput("plot_specific_ui"),
                 plotOutput("data_viz_plot", height = "600px", width = "600px")
        ),
        tabPanel("Heatmap", 
                 plotOutput("heatmap", height = "600px", width = "600px")),
        
        # New tab for model comparison
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

server <- function(input, output, session) {
  # Initialize reactive values
  original_data <- reactiveVal(NULL)
  cleaned_data <- reactiveVal(NULL)
  current_model <- reactiveVal(NULL)
  all_models <- reactiveVal(list()) # Store all trained models
  best_model <- reactiveVal(NULL) # Store the best model
  drop_message <- reactiveVal("")
  
  # Load data when file is uploaded
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath, na.strings = c("", "NA", "?")) # Treat "?" as NA
    original_data(df)
    cleaned_data(df)
    drop_message("") # Reset drop message
  })
  
  # Column selector UI
  output$column_selector_ui <- renderUI({
    req(cleaned_data())
    selectInput("columns_to_drop", "Select Columns to Drop", 
                choices = names(cleaned_data()), 
                multiple = TRUE,
                selected = NULL)
  })
  
  # Row dropping UI
  output$drop_rows_ui <- renderUI({
    req(cleaned_data())
    tagList(
      textInput("drop_rows", "Drop Rows (Enter Row Indices, comma-separated)", value = ""),
      actionButton("drop_selected_rows", "Drop Selected Rows", class = "btn-warning")
    )
  })
  
  # Model selection UI - NEW
  output$model_selection_ui <- renderUI({
    checkboxGroupInput("selected_models", "Select Regression Models:",
                       choices = c(
                         "Linear Regression" = "lm",
                         "Ridge Regression" = "glmnet",
                         "Lasso Regression" = "lasso",
                         "Random Forest" = "rf",
                         "Gradient Boosting" = "gbm",
                         "XGBoost" = "xgbTree",
                         "Support Vector Regression" = "svmRadial",
                         "MARS" = "earth"
                       ),
                       selected = c("lm", "rf", "xgbTree"))
  })
  
  # Action to drop columns when button is clicked
  observeEvent(input$drop_selected_columns, {
    req(cleaned_data(), input$columns_to_drop)
    
    df <- cleaned_data()
    columns_to_drop <- input$columns_to_drop
    
    if(length(columns_to_drop) > 0) {
      # Make sure selected columns exist in the dataset
      valid_columns <- intersect(columns_to_drop, names(df))
      
      if(length(valid_columns) > 0) {
        # Drop the columns
        df <- df %>% select(-all_of(valid_columns))
        cleaned_data(df)
        drop_message(paste("Successfully dropped columns:", paste(valid_columns, collapse=", ")))
      } else {
        drop_message("No valid columns selected for dropping.")
      }
    } else {
      drop_message("No columns selected for dropping.")
    }
  })
  
  # Action to drop rows when button is clicked
  observeEvent(input$drop_selected_rows, {
    req(cleaned_data(), input$drop_rows)
    
    df <- cleaned_data()
    
    if(input$drop_rows != "") {
      row_indices <- as.numeric(unlist(strsplit(input$drop_rows, ",")))
      row_indices <- na.omit(row_indices)
      
      if(length(row_indices) > 0) {
        # Filter out indices that are out of bounds
        valid_indices <- row_indices[row_indices > 0 & row_indices <= nrow(df)]
        
        if(length(valid_indices) > 0) {
          df <- df[-valid_indices, , drop = FALSE]
          cleaned_data(df)
          drop_message(paste("Successfully dropped", length(valid_indices), "rows."))
        } else {
          drop_message("No valid row indices provided.")
        }
      } else {
        drop_message("No row indices provided.")
      }
    }
  })
  
  # Display drop message
  output$drop_columns_message <- renderText({
    drop_message()
  })
  
  # Auto clean functionality
  observeEvent(input$auto_clean, {
    req(cleaned_data())
    if(input$auto_clean) {
      df <- cleaned_data()
      
      # 1️⃣ Remove columns with more than 50% missing values
      missing_threshold <- 0.5
      drop_missing <- names(df)[colMeans(is.na(df)) > missing_threshold]
      if(length(drop_missing) > 0) {
        df <- df %>% select(-all_of(drop_missing))
      }
      
      # 2️⃣ Remove near-zero variance columns
      if (ncol(df) > 1) {
        nzv <- nearZeroVar(df, saveMetrics = TRUE)
        drop_nzv <- rownames(nzv[nzv$nzv, ])
        if(length(drop_nzv) > 0) {
          df <- df %>% select(-all_of(drop_nzv))
        }
      }
      
      # 3️⃣ Remove highly correlated columns (correlation > 0.9)
      num_cols <- select_if(df, is.numeric)
      if (ncol(num_cols) > 1) {
        cor_matrix <- cor(num_cols, use = "complete.obs")
        high_corr <- findCorrelation(cor_matrix, cutoff = 0.9, names = TRUE)
        if(length(high_corr) > 0) {
          df <- df %>% select(-all_of(high_corr))
        }
      }
      
      cleaned_data(df)
      drop_message("Auto-cleaning applied successfully.")
    }
  })
  
  # Dataset stats
  output$row_count_before <- renderPrint({
    req(original_data())
    paste("Initial number of rows:", nrow(original_data()))
  })
  
  output$col_count_before <- renderPrint({
    req(original_data())
    paste("Initial number of columns:", ncol(original_data()))
  })
  
  output$row_count_after <- renderPrint({
    req(cleaned_data())
    paste("Current number of rows:", nrow(cleaned_data()))
  })
  
  output$col_count_after <- renderPrint({
    req(cleaned_data())
    paste("Current number of columns:", ncol(cleaned_data()))
  })
  
  # Data preview and summary
  output$data_preview <- renderDataTable({ 
    req(cleaned_data())
    cleaned_data() 
  })
  
  output$data_summary <- renderPrint({ 
    req(cleaned_data())
    summary(cleaned_data()) 
  })
  
  output$column_names <- renderPrint({ 
    req(cleaned_data())
    colnames(cleaned_data()) 
  })
  
  output$numeric_columns <- renderPrint({
    req(cleaned_data())
    num_cols <- names(select_if(cleaned_data(), is.numeric))
    paste(num_cols, collapse = ", ")
  })
  
  output$null_values <- renderPrint({
    req(cleaned_data())
    colSums(is.na(cleaned_data()))
  })
  
  output$duplicates <- renderPrint({
    req(cleaned_data())
    sum(duplicated(cleaned_data()))
  })
  
  # Boxplot for outlier detection
  output$boxplot <- renderPlot({
    req(cleaned_data())
    num_cols <- names(select_if(cleaned_data(), is.numeric))
    if (length(num_cols) > 0) {
      df_long <- tidyr::pivot_longer(cleaned_data(), all_of(num_cols), names_to = "Variable", values_to = "Value")
      ggplot(df_long, aes(x = Variable, y = Value)) +
        geom_boxplot() +
        labs(title = "Boxplots of All Numeric Columns", x = "Numeric Variables", y = "Values") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      plot.new()
      title("No Numeric Columns Available")
    }
  })
  
  # Outlier count
  output$outlier_count <- renderPrint({
    req(cleaned_data())
    num_cols <- names(select_if(cleaned_data(), is.numeric))
    if (length(num_cols) > 0) {
      outlier_counts <- sapply(num_cols, function(col) {
        Q1 <- quantile(cleaned_data()[[col]], 0.25, na.rm = TRUE)
        Q3 <- quantile(cleaned_data()[[col]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        sum(cleaned_data()[[col]] < (Q1 - 1.5 * IQR) | cleaned_data()[[col]] > (Q3 + 1.5 * IQR), na.rm = TRUE)
      })
      print(outlier_counts)
    } else {
      print("No numeric columns available to detect outliers.")
    }
  })
  
  # Scatterplot
  output$scatterplot <- renderPlot({
    req(cleaned_data())
    num_cols <- names(select_if(cleaned_data(), is.numeric))
    if (length(num_cols) > 1) {
      ggplot(cleaned_data(), aes_string(x = num_cols[1], y = num_cols[2])) +
        geom_point() +
        labs(title = "Scatter Plot", x = num_cols[1], y = num_cols[2])
    } else {
      plot.new()
      title("Not Enough Numeric Columns for Scatter Plot")
    }
  })
  
  # Outlier removal
  observeEvent(input$remove_outliers, {
    showModal(
      modalDialog(
        title = "Remove Outliers?",
        "Outlier removal helps improve model accuracy by eliminating extreme values that may skew predictions.",
        easyClose = FALSE,
        footer = tagList(
          actionButton("confirm_remove", "Yes, Remove Outliers"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  observeEvent(input$confirm_remove, {
    req(cleaned_data())
    num_cols <- names(select_if(cleaned_data(), is.numeric))
    if (length(num_cols) > 0) {
      new_data <- cleaned_data()
      for (col in num_cols) {
        Q1 <- quantile(new_data[[col]], 0.25, na.rm = TRUE)
        Q3 <- quantile(new_data[[col]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        new_data <- new_data[new_data[[col]] >= (Q1 - 1.5 * IQR) &
                               new_data[[col]] <= (Q3 + 1.5 * IQR) | is.na(new_data[[col]]), ]
      }
      cleaned_data(new_data)
      drop_message(paste("Removed outliers from", length(num_cols), "numeric columns."))
      removeModal()
    }
  })
  
  # Heatmap UI
  output$heatmap_columns_ui <- renderUI({
    req(cleaned_data())
    num_cols <- names(select_if(cleaned_data(), is.numeric))
    selectInput("heatmap_cols", "Select Columns for Heatmap",
                choices = num_cols,
                multiple = TRUE,
                selected = num_cols[1:min(5, length(num_cols))])
  })
  
  # Heatmap plot
  output$heatmap <- renderPlot({
    req(cleaned_data(), input$heatmap_cols)
    num_data <- cleaned_data()[, input$heatmap_cols, drop = FALSE]
    if (ncol(num_data) < 2) {
      plot.new()
      title("Please Select at Least 2 Numeric Columns")
      return(NULL)
    }
    corr_matrix <- cor(num_data, use = "complete.obs")
    corrplot(
      corr_matrix,
      method = "color",
      type = "upper",
      tl.cex = 0.7,
      tl.col = "black",
      tl.srt = 45,
      number.cex = 0.5,
      addCoef.col = "black",
      diag = FALSE,
      mar = c(0, 0, 1, 0)
    )
  })
  
  # Prediction UI
  output$prediction_target_ui <- renderUI({
    req(cleaned_data())
    colnames <- names(cleaned_data())
    num_cols <- names(select_if(cleaned_data(), is.numeric))
    selectInput("target_variable", "Select Target Variable (Numeric)", choices = num_cols)
  })
  
  output$predictor_inputs_ui <- renderUI({
    req(cleaned_data(), input$target_variable)
    df <- cleaned_data()
    predictors <- setdiff(names(df), input$target_variable)
    input_fields <- lapply(predictors, function(col) {
      if (is.numeric(df[[col]])) {
        numericInput(paste0("input_", col), label = col, value = mean(df[[col]], na.rm = TRUE))
      } else {
        choices <- unique(df[[col]])
        choices <- choices[!is.na(choices)]
        if (length(choices) == 0) choices <- "Unknown"
        selectInput(paste0("input_", col), label = col, choices = as.character(choices))
      }
    })
    do.call(tagList, input_fields)
  })
  
  # Plot-specific UI controls
  output$plot_specific_ui <- renderUI({
    req(cleaned_data())
    df <- cleaned_data()
    num_cols <- names(select_if(df, is.numeric))
    cat_cols <- names(select_if(df, is.character))
    
    if (input$plot_type == "Scatter Plot") {
      tagList(
        selectInput("scatter_x", "Select X Variable (Numeric)", choices = num_cols),
        selectInput("scatter_y", "Select Y Variable (Numeric)", choices = num_cols)
      )
    } else if (input$plot_type == "Box Plot") {
      tagList(
        selectInput("box_x", "Select X Variable (Categorical)", choices = c(cat_cols, num_cols)),
        selectInput("box_y", "Select Y Variable (Numeric)", choices = num_cols)
      )
    } else if (input$plot_type == "Line Plot") {
      tagList(
        selectInput("line_x", "Select X Variable (Ordered)", choices = names(df)),
        selectInput("line_y", "Select Y Variable (Numeric)", choices = num_cols)
      )
    } else if (input$plot_type == "Histogram") {
      tagList(
        selectInput("hist_var", "Select Variable for Histogram", choices = num_cols),
        sliderInput("bins", "Number of Bins:", min = 5, max = 50, value = 15)
      )
    } else if (input$plot_type %in% c("Residual Plot", "Q-Q Plot")) {
      tagList(
        selectInput("target_variable_viz", "Select Target Variable for Model", choices = num_cols)
      )
    } else {
      NULL
    }
  })
  
  # Visualization plots
  output$data_viz_plot <- renderPlot({
    req(cleaned_data())
    df <- cleaned_data()
    num_cols <- names(select_if(df, is.numeric))
    
    if (input$plot_type == "Pair Plot") {
      req(num_cols)
      if (length(num_cols) < 2) {
        plot.new()
        title("Need at least 2 numeric columns for pair plot")
        return(NULL)
      }
      num_data <- df[, num_cols, drop = FALSE]
      # Limit to at most 8 columns to avoid performance issues
      if (ncol(num_data) > 8) {
        num_data <- num_data[, 1:8]
      }
      ggpairs(num_data) +
        ggtitle("Pair Plot of Numeric Variables")
      
    } else if (input$plot_type == "Scatter Plot") {
      req(input$scatter_x, input$scatter_y)
      ggplot(df, aes_string(x = input$scatter_x, y = input$scatter_y)) +
        geom_point() +
        labs(title = paste("Scatter Plot of", input$scatter_x, "vs", input$scatter_y),
             x = input$scatter_x, y = input$scatter_y)
      
    } else if (input$plot_type == "Box Plot") {
      req(input$box_x, input$box_y)
      ggplot(df, aes_string(x = input$box_x, y = input$box_y)) +
        geom_boxplot() +
        labs(title = paste("Box Plot of", input$box_y, "by", input$box_x),
             x = input$box_x, y = input$box_y) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$plot_type == "Line Plot") {
      req(input$line_x, input$line_y)
      ggplot(df, aes_string(x = input$line_x, y = input$line_y, group = 1)) +
        geom_line() +
        labs(title = paste("Line Plot of", input$line_y, "over", input$line_x),
             x = input$line_x, y = input$line_y) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$plot_type == "Histogram") {
      req(input$hist_var)
      ggplot(df, aes_string(x = input$hist_var)) +
        geom_histogram(bins = input$bins, fill = "blue", alpha = 0.5) +
        labs(title = paste("Histogram of", input$hist_var),
             x = input$hist_var, y = "Frequency")
      
    } else if (input$plot_type %in% c("Residual Plot", "Q-Q Plot")) {
      req(input$target_variable_viz)
      target <- input$target_variable_viz
      predictors <- setdiff(names(df), target)
      
      # Ensure we have numeric predictors
      num_predictors <- intersect(predictors, num_cols)
      if (length(num_predictors) == 0) {
        plot.new()
        title("No numeric predictors available for model")
        return(NULL)
      }
      
      # Subset to target and numeric predictors
      model_df <- df %>% select(all_of(c(target, num_predictors)))
      model_df <- na.omit(model_df)
      
      if (nrow(model_df) <= length(num_predictors) + 1) {
        plot.new()
        title("Dataset too small for regression diagnostics")
        return(NULL)
      }
      
      model <- tryCatch({
        train(as.formula(paste(target, "~ .")), 
              data = model_df, 
              method = "lm",
              trControl = trainControl(method = "none"))
      }, error = function(e) {
        plot.new()
        title("Failed to train model for diagnostics")
        return(NULL)
      })
      
      if (is.null(model)) return(NULL)
      
      predictions <- predict(model, newdata = model_df)
      residuals <- model_df[[target]] - predictions
      
      if (input$plot_type == "Residual Plot") {
        ggplot(data.frame(Predicted = predictions, Residuals = residuals), 
               aes(x = Predicted, y = Residuals)) +
          geom_point() +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals")
      } else {
        # Q-Q Plot
        df_qq <- data.frame(residuals = residuals)
        ggplot(df_qq, aes(sample = residuals)) +
          stat_qq() +
          stat_qq_line(color = "red") +
          labs(title = "Normal Q-Q Plot of Residuals")
      }
    }
  })
  
  # Helper function to preprocess data for modeling
  preprocess_data <- function(df, target) {
    # Ensure target is numeric
    if (!is.numeric(df[[target]])) {
      return(list(success = FALSE, message = "Target variable must be numeric for regression."))
    }
    
    predictors <- setdiff(names(df), target)
    
    # Subset the data and handle NA values
    model_df <- df %>% select(all_of(c(target, predictors)))
    
    # For numeric predictors, replace NA with mean
    # For categorical predictors, replace NA with mode (most common value)
    model_df <- model_df %>% mutate(
      across(where(is.numeric), ~replace(., is.na(.), mean(., na.rm = TRUE))),
      across(where(~!is.numeric(.)), ~replace(., is.na(.), as.character(names(which.max(table(.))))))
    )
    
    # Convert categorical predictors to factors
    model_df <- model_df %>% mutate(across(where(~!is.numeric(.)), as.factor))
    
    # Check if we have enough data
    if (nrow(model_df) < 2) {
      return(list(success = FALSE, message = "Error: At least 2 rows are required for prediction modeling."))
    } else if (nrow(model_df) <= length(predictors) + 1) {
      return(list(success = FALSE, message = "Dataset too small for regression. Consider removing some predictors."))
    }
    
    return(list(success = TRUE, data = model_df))
  }
  
  # Run prediction with multiple models
  observeEvent(input$run_prediction, {
    req(cleaned_data(), input$target_variable, input$selected_models)
    
    # Check if at least one model is selected
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
        result$message
      })
      output$model_summary <- renderPrint({
        NULL
      })
      return()
    }
    
    model_df <- result$data
    
    # Prepare cross-validation settings
    if (input$cv_method == "none") {
      trControl <- trainControl(method = "none")
    } else if (input$cv_method == "cv") {
      trControl <- trainControl(method = "cv", number = input$cv_k)
    } else if (input$cv_method == "repeatedcv") {
      trControl <- trainControl(method = "repeatedcv", number = input$cv_k, repeats = input$cv_repeats)
    }
    
    # Initialize storage for model results
    models_list <- list()
    model_results <- data.frame(
      Model = character(),
      RMSE = numeric(),
      MAE = numeric(),
      Rsquared = numeric(),
      stringsAsFactors = FALSE
    )
    
    withProgress(message = 'Training models...', value = 0, {
      # Train all selected models
      for (i in seq_along(input$selected_models)) {
        method <- input$selected_models[i]
        model_name <- names(which(c(
          "lm" = "Linear Regression",
          "glmnet" = "Ridge Regression",
          "lasso" = "Lasso Regression",
          "rf" = "Random Forest", 
          "gbm" = "Gradient Boosting", 
          "xgbTree" = "XGBoost",
          "svmRadial" = "Support Vector Regression",
          "earth" = "MARS"
        ) == method))
        
        incProgress(1/length(input$selected_models), detail = paste("Training", model_name))
        
        tryCatch({
          # Set method-specific tuning parameters
          tuneGrid <- NULL
          if (method == "glmnet") {
            tuneGrid <- expand.grid(alpha = 0, lambda = seq(0.001, 1, length = 10))
          } else if (method == "lasso") {
            method <- "glmnet"  # Use glmnet with alpha=1 for lasso
            tuneGrid <- expand.grid(alpha = 1, lambda = seq(0.001, 1, length = 10))
          }
          
          model <- train(
            as.formula(paste(target, "~ .")), 
            data = model_df, 
            method = method,
            trControl = trControl,
            tuneGrid = tuneGrid,
            preProcess = c("center", "scale"),  # Standardize predictors
            # For many models, maximize = TRUE is the default for accuracy metrics
            maximize = TRUE
          )
          
          # Store model
          models_list[[model_name]] <- model
          
          # Extract performance metrics
          if (is.null(model$results$RMSE)) {
            # For models without cross-validation, compute metrics manually
            pred <- predict(model, newdata = model_df)
            rmse <- sqrt(mean((model_df[[target]] - pred)^2, na.rm = TRUE))
            mae <- mean(abs(model_df[[target]] - pred), na.rm = TRUE)
            rsq <- cor(model_df[[target]], pred, use = "complete.obs")^2
          } else {
            # For models with cross-validation, use the best performance metrics
            best_idx <- which.min(model$results$RMSE)
            rmse <- model$results$RMSE[best_idx]
            mae <- if(is.null(model$results$MAE)) NA else model$results$MAE[best_idx]
            rsq <- model$results$Rsquared[best_idx]
          }
          
          # Add to results table
          model_results <- rbind(model_results, data.frame(
            Model = model_name,
            RMSE = rmse,
            MAE = mae,
            Rsquared = rsq,
            stringsAsFactors = FALSE
          ))
        }, error = function(e) {
          # Log errors but continue with other models
          cat("Error training", model_name, ":", e$message, "\n")
        })
      }
    })
    
    # Store all models
    all_models(models_list)
    
    # Determine best model (lowest RMSE)
    if (nrow(model_results) > 0) {
      best_model_name <- model_results$Model[which.min(model_results$RMSE)]
      best_model(models_list[[best_model_name]])
      
      # Create new data for prediction from inputs
      new_data <- data.frame(row.names = 1)
      predictors <- setdiff(names(model_df), target)
      for (col in predictors) {
        input_value <- input[[paste0("input_", col)]]
        # Handle numeric vs categorical differently
        if (is.numeric(model_df[[col]])) {
          new_data[[col]] <- as.numeric(input_value)
        } else {
          new_data[[col]] <- as.character(input_value)
        }
      }
      
      # Convert factors to match the model
      for (col in predictors) {
        if (is.factor(model_df[[col]])) {
          new_data[[col]] <- factor(new_data[[col]], levels = levels(model_df[[col]]))
        }
      }
      
      # Make prediction with best model
      best_prediction <- tryCatch({
        predict(models_list[[best_model_name]], newdata = new_data)
      }, error = function(e) {
        paste("Error making prediction with best model:", e$message)
      })
      
      # Display prediction results
      output$prediction_results <- renderPrint({
        cat("Model Comparison Results:\n")
        print(model_results[order(model_results$RMSE), ])
        cat("\nBest Model: ", best_model_name, "\n")
        if (is.character(best_prediction)) {
          cat(best_prediction)
        } else {
          cat("Predicted", target, ":", round(best_prediction, 4))
        }
      })
      
      # Display model comparison plot
      output$model_comparison_plot <- renderPlot({
        model_results$Model <- factor(model_results$Model, 
            levels = model_results$Model[order(model_results$RMSE)])
        
        ggplot(model_results, aes(x = Model, y = RMSE, fill = Model)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          labs(title = "Model Comparison (Lower RMSE is Better)",
               x = "Regression Model", 
               y = "Root Mean Square Error (RMSE)") +
          theme_minimal() +
          theme(legend.position = "none")
      })
      
      # Display the best model summary
      output$model_summary <- renderPrint({
        best <- best_model()
        if (!is.null(best)) {
          cat("Best Model Summary:\n")
          print(best)
          
          # For linear models, also show coefficients
          if (best$method %in% c("lm", "glmnet")) {
            cat("\nModel Coefficients:\n")
            print(coef(best$finalModel))
          }
        }
      })
      
      # Display variable importance plot if available
      output$variable_importance_plot <- renderPlot({
        best <- best_model()
        if (is.null(best)) return(NULL)
        
        # Try to extract variable importance
        importance <- tryCatch({
          if (best$method %in% c("rf", "gbm", "xgbTree", "earth")) {
            varImp(best)
          } else {
            NULL
          }
        }, error = function(e) {
          NULL
        })
        
        if (!is.null(importance)) {
          # Plot variable importance
          plot(importance, main = "Variable Importance for Best Model")
        } else {
          # For models without built-in importance
          if (best$method %in% c("lm", "glmnet")) {
            # For linear models, create a custom importance plot from coefficients
            coefs <- coef(best$finalModel)
            
            # For glmnet, extract coefficients for best lambda
            if (best$method == "glmnet") {
              best_lambda_idx <- which(best$finalModel$lambda == best$bestTune$lambda)
              coefs <- coef(best$finalModel, s = best$bestTune$lambda)
            }
            
            # Exclude intercept, convert to data frame, and take absolute values
            coef_df <- data.frame(
              Variable = rownames(coefs)[-1],
              Importance = abs(as.numeric(coefs)[-1])
            )
            
            # Sort by importance
            coef_df <- coef_df[order(coef_df$Importance, decreasing = TRUE), ]
            coef_df$Variable <- factor(coef_df$Variable, levels = coef_df$Variable)
            
            ggplot(coef_df, aes(x = Variable, y = Importance)) +
              geom_bar(stat = "identity", fill = "skyblue") +
              coord_flip() +
              labs(title = "Variable Importance (Absolute Coefficient Values)",
                   x = "Predictor Variable", 
                   y = "Importance") +
              theme_minimal()
          } else {
            # For other models without variable importance
            plot.new()
            title("Variable importance not available for this model type")
          }
        }
      })
      
    } else {
      output$prediction_results <- renderPrint({
        "All models failed to train. Please check your data or model selections."
      })
      output$model_summary <- renderPrint({
        NULL
      })
    }
  })
  
  # Model comparison tab - display comparison of all models
  output$model_comparison_results <- renderPrint({
    models <- all_models()
    if (length(models) == 0) {
      "No models have been trained yet. Please run prediction first."
    } else {
      cat("Model Performance Comparison:\n")
      model_results <- data.frame(
        Model = names(models),
        RMSE = sapply(models, function(m) {
          if (is.null(m$results$RMSE)) {
            pred <- predict(m, newdata = m$trainingData)
            sqrt(mean((m$trainingData$.outcome - pred)^2, na.rm = TRUE))
          } else {
            min(m$results$RMSE)
          }
        }),
        Rsquared = sapply(models, function(m) {
          if (is.null(m$results$Rsquared)) {
            pred <- predict(m, newdata = m$trainingData)
            cor(m$trainingData$.outcome, pred, use = "complete.obs")^2
          } else {
            max(m$results$Rsquared)
          }
        }),
        stringsAsFactors = FALSE
      )
      model_results <- model_results[order(model_results$RMSE), ]
      print(model_results)
      
      cat("\nBest Model: ", model_results$Model[1], "\n")
      
      # Print information about the cross-validation used
      if (input$cv_method == "none") {
        cat("\nNote: No cross-validation was used. Consider using cross-validation for more reliable results.\n")
      } else if (input$cv_method == "cv") {
        cat("\nCross-validation: ", input$cv_k, "-fold CV\n")
      } else if (input$cv_method == "repeatedcv") {
        cat("\nCross-validation: ", input$cv_k, "-fold CV with ", input$cv_repeats, " repeats\n")
      }
    }
  })
}

shinyApp(ui, server)