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
  dmy_obj <- reactiveVal(NULL)      # Store dummyVars object
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
  
  # Model selection UI
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
                       selected = c("lm", "rf"))
  })
  
  # Action to drop columns
  observeEvent(input$drop_selected_columns, {
    req(cleaned_data(), input$columns_to_drop)
    df <- cleaned_data()
    columns_to_drop <- input$columns_to_drop
    if (length(columns_to_drop) > 0) {
      valid_columns <- intersect(columns_to_drop, names(df))
      if (length(valid_columns) > 0) {
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
  
  # Action to drop rows
  observeEvent(input$drop_selected_rows, {
    req(cleaned_data(), input$drop_rows)
    df <- cleaned_data()
    if (input$drop_rows != "") {
      row_indices <- as.numeric(unlist(strsplit(input$drop_rows, ",")))
      row_indices <- na.omit(row_indices)
      if (length(row_indices) > 0) {
        valid_indices <- row_indices[row_indices > 0 & row_indices <= nrow(df)]
        if (length(valid_indices) > 0) {
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
    if (input$auto_clean) {
      df <- cleaned_data()
      missing_threshold <- 0.5
      drop_missing <- names(df)[colMeans(is.na(df)) > missing_threshold]
      if (length(drop_missing) > 0) {
        df <- df %>% select(-all_of(drop_missing))
      }
      if (ncol(df) > 1) {
        nzv <- nearZeroVar(df, saveMetrics = TRUE)
        drop_nzv <- rownames(nzv[nzv$nzv, ])
        if (length(drop_nzv) > 0) {
          df <- df %>% select(-all_of(drop_nzv))
        }
      }
      num_cols <- select_if(df, is.numeric)
      if (ncol(num_cols) > 1) {
        cor_matrix <- cor(num_cols, use = "complete.obs")
        high_corr <- findCorrelation(cor_matrix, cutoff = 0.9, names = TRUE)
        if (length(high_corr) > 0) {
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
    validate(need(nrow(cleaned_data()) > 0, "No data available to display."))
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
    validate(need(length(num_cols) > 0, "No numeric columns available."))
    df_long <- tidyr::pivot_longer(cleaned_data(), all_of(num_cols), names_to = "Variable", values_to = "Value")
    ggplot(df_long, aes(x = Variable, y = Value)) +
      geom_boxplot() +
      labs(title = "Boxplots of Numeric Columns", x = "Variables", y = "Values") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
    validate(need(length(num_cols) > 1, "Need at least 2 numeric columns for scatter plot."))
    ggplot(cleaned_data(), aes_string(x = num_cols[1], y = num_cols[2])) +
      geom_point() +
      labs(title = "Scatter Plot", x = num_cols[1], y = num_cols[2])
  })
  
  # Outlier removal
  observeEvent(input$remove_outliers, {
    showModal(
      modalDialog(
        title = "Remove Outliers?",
        "Outlier removal helps improve model accuracy by eliminating extreme values.",
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
    validate(
      need(ncol(num_data) >= 2, "Please select at least 2 numeric columns."),
      need(all(sapply(num_data, is.numeric)), "Selected columns must be numeric.")
    )
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
    num_cols <- names(select_if(cleaned_data(), is.numeric))
    validate(need(length(num_cols) > 0, "No numeric columns available for target variable."))
    selectInput("target_variable", "Select Target Variable (Numeric)", choices = num_cols)
  })
  
  # Predictor inputs UI
  output$predictor_inputs_ui <- renderUI({
    req(cleaned_data(), input$target_variable)
    df <- cleaned_data()
    predictors <- setdiff(names(df), input$target_variable)
    validate(need(length(predictors) > 0, "No predictor variables available."))
    
    input_fields <- lapply(predictors, function(col) {
      if (is.numeric(df[[col]])) {
        numericInput(paste0("input_", make.names(col)), label = col, 
                     value = mean(df[[col]], na.rm = TRUE))
      } else {
        unique_vals <- unique(df[[col]])
        unique_vals <- unique_vals[!is.na(unique_vals)]
        if (length(unique_vals) == 0) return(NULL)
        unique_vals <- as.character(unique_vals)
        default_val <- unique_vals[1]
        selectInput(paste0("input_", make.names(col)), label = col,
                    choices = unique_vals, selected = default_val)
      }
    })
    input_fields <- input_fields[!sapply(input_fields, is.null)]
    do.call(tagList, input_fields)
  })
  
  # Plot-specific UI
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
    if (nrow(df) > 10000) {
      df <- df[sample(nrow(df), 10000), ]
      showNotification("Dataset sampled to 10,000 rows for visualization.", type = "warning")
    }
    num_cols <- names(select_if(df, is.numeric))
    
    if (input$plot_type == "Pair Plot") {
      validate(need(length(num_cols) >= 2, "Need at least 2 numeric columns for pair plot."))
      num_data <- df[, num_cols, drop = FALSE]
      if (ncol(num_data) > 8) {
        num_data <- num_data[, 1:8]
        showNotification("Limited to first 8 numeric columns for performance.", type = "warning")
      }
      ggpairs(num_data) +
        ggtitle("Pair Plot of Numeric Variables")
      
    } else if (input$plot_type == "Scatter Plot") {
      req(input$scatter_x, input$scatter_y)
      ggplot(df, aes_string(x = input$scatter_x, y = input$scatter_y)) +
        geom_point() +
        labs(title = paste("Scatter Plot of", input$scatter_x, "vs", input$scatter_y))
      
    } else if (input$plot_type == "Box Plot") {
      req(input$box_x, input$box_y)
      ggplot(df, aes_string(x = input$box_x, y = input$box_y)) +
        geom_boxplot() +
        labs(title = paste("Box Plot of", input$box_y, "by", input$box_x)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$plot_type == "Line Plot") {
      req(input$line_x, input$line_y)
      ggplot(df, aes_string(x = input$line_x, y = input$line_y, group = 1)) +
        geom_line() +
        labs(title = paste("Line Plot of", input$line_y, "over", input$line_x)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$plot_type == "Histogram") {
      req(input$hist_var)
      ggplot(df, aes_string(x = input$hist_var)) +
        geom_histogram(bins = input$bins, fill = "blue", alpha = 0.5) +
        labs(title = paste("Histogram of", input$hist_var), x = input$hist_var, y = "Frequency")
      
    } else if (input$plot_type %in% c("Residual Plot", "Q-Q Plot")) {
      req(input$target_variable_viz)
      target <- input$target_variable_viz
      predictors <- setdiff(names(df), target)
      num_predictors <- intersect(predictors, num_cols)
      validate(need(length(num_predictors) > 0, "No numeric predictors available for model."))
      
      model_df <- df %>% select(all_of(c(target, num_predictors)))
      model_df <- na.omit(model_df)
      validate(need(nrow(model_df) > length(num_predictors) + 1, "Dataset too small for regression diagnostics."))
      
      model <- tryCatch({
        train(as.formula(paste(target, "~ .")), 
              data = model_df, 
              method = "lm",
              trControl = trainControl(method = "none"))
      }, error = function(e) {
        NULL
      })
      if (is.null(model)) {
        plot.new()
        title("Failed to train model for diagnostics.")
        return()
      }
      
      predictions <- predict(model, newdata = model_df)
      residuals <- model_df[[target]] - predictions
      
      if (input$plot_type == "Residual Plot") {
        ggplot(data.frame(Predicted = predictions, Residuals = residuals), 
               aes(x = Predicted, y = Residuals)) +
          geom_point() +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals")
      } else {
        ggplot(data.frame(residuals = residuals), aes(sample = residuals)) +
          stat_qq() +
          stat_qq_line(color = "red") +
          labs(title = "Normal Q-Q Plot of Residuals")
      }
    }
  })
  
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
    return(list(success = TRUE, data = final_df_processed, preproc = preproc, dmy = dmy, message = "Data preprocessing successful."))
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
      dmy_obj(result$dmy)  # Store dummyVars object
      
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
      
      # Log best model and its expected predictors
      cat("Best model:", best_model_name, "\n")
      cat("Expected predictors:", paste(names(result$data)[names(result$data) != target], collapse = ", "), "\n")
      
      # Gather new inputs and include all original predictors
      new_data <- data.frame(row.names = 1)
      for (col in setdiff(names(df), target)) {  # Include all original predictors
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
            new_data[[col]] <- if (length(unique_vals) > 0) unique_vals[1] else NA
          }
        }
      }
      
      # Preprocess new_data with the same dummyVars and preProcess objects
      if (!is.null(dmy_obj())) {
        cat_predictors <- names(df)[sapply(df, is.factor)]
        if (length(cat_predictors) > 0) {
          new_cat_data <- new_data[, cat_predictors, drop = FALSE]
          # Handle cases where new data might have unseen levels
          for (col in names(new_cat_data)) {
            if (!all(levels(df[[col]]) %in% new_cat_data[[col]])) {
              new_cat_data[[col]] <- factor(new_cat_data[[col]], levels = levels(df[[col]]))
            }
          }
          new_cat_matrix <- predict(dmy_obj(), new_cat_data)
          new_numeric_data <- new_data[, setdiff(names(new_data), cat_predictors), drop = FALSE]
          new_data <- cbind(new_numeric_data, new_cat_matrix)
        }
      }
      # Ensure all columns are present, even if NA
      for (col in setdiff(names(result$data)[names(result$data) != target], names(new_data))) {
        new_data[[col]] <- NA
      }
      new_data <- new_data[, names(result$data)[names(result$data) != target]]  # Match order
      new_data_processed <- predict(preproc_obj(), new_data)
      cat("New data processed columns:", paste(names(new_data_processed), collapse = ", "), "\n")
      cat("New data processed dimensions:", dim(new_data_processed), "\n")
      
      # Validate and align new_data_processed
      if (ncol(new_data_processed) != ncol(result$data) - 1) {  # -1 for target
        cat("Warning: Column mismatch between new data and training data.\n")
        for (col in setdiff(names(result$data)[names(result$data) != target], names(new_data_processed))) {
          new_data_processed[[col]] <- 0  # Default for missing columns
        }
        new_data_processed <- new_data_processed[, names(result$data)[names(result$data) != target]]
      }
      cat("Aligned new data columns:", paste(names(new_data_processed), collapse = ", "), "\n")
      
      # Make prediction with detailed error handling
      prediction_value <- tryCatch({
        predict(best_model(), newdata = as.data.frame(new_data_processed))
      }, error = function(e) {
        cat("Prediction error:", as.character(e), "\n")
        cat("New data processed structure:", str(new_data_processed), "\n")
        cat("Training data structure:", str(result$data[, names(result$data) != target]), "\n")
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
        if (nrow(metrics_df) == 0) {
          plot.new()
          title("No valid models to compare due to prediction failure.")
          cat("Warning: No valid metrics_df for comparison plot.\n")
        } else {
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
        }
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