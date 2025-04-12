library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(GGally)
library(corrplot)
library(shinyWidgets)
library(caret)
library(tidyr)

ui <- fluidPage(
  titlePanel("Collaborative Data Analysis & Prediction Workspace"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      
      # New approach for column dropping
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
      uiOutput("predictor_inputs_ui"),
      actionButton("run_prediction", "Run Prediction"),
      
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
        tabPanel("Prediction Results", 
                 verbatimTextOutput("prediction_results"),
                 verbatimTextOutput("model_summary"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Initialize reactive values
  original_data <- reactiveVal(NULL)
  cleaned_data <- reactiveVal(NULL)
  current_model <- reactiveVal(NULL)
  drop_message <- reactiveVal("")
  
  # Load data when file is uploaded
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath, na.strings = c("", "NA", "?")) # Treat "?" as NA
    original_data(df)
    cleaned_data(df)
    drop_message("") # Reset drop message
  })
  
  # Column selector UI - this is the new approach
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
    selectInput("target_variable", "Select Target Variable", choices = colnames)
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
  
  # Run prediction
  observeEvent(input$run_prediction, {
    req(cleaned_data(), input$target_variable)
    df <- cleaned_data()
    target <- input$target_variable
    predictors <- setdiff(names(df), target)
    
    # Check if target is numeric
    if (!is.numeric(df[[target]])) {
      output$prediction_results <- renderPrint({
        "Error: Target variable must be numeric for linear regression."
      })
      output$model_summary <- renderPrint({
        NULL
      })
      current_model(NULL)
      return()
    }
    
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
    
    output$prediction_results <- renderPrint({
      if (nrow(model_df) < 2) {
        current_model(NULL)
        "Error: At least 2 rows are required for prediction modeling."
      } else if (nrow(model_df) <= length(predictors) + 1) {
        mean_value <- mean(model_df[[target]], na.rm = TRUE)
        current_model(NULL)
        paste("Dataset too small for regression. Using mean prediction.\n",
              "Predicted", target, ":", mean_value)
      } else {
        # Try to build the model
        model <- tryCatch({
          train(as.formula(paste(target, "~ .")), 
                data = model_df, 
                method = "lm",
                trControl = trainControl(method = "none"))
        }, error = function(e) {
          cat("Model training error:", e$message, "\n")
          return(NULL)
        })
        
        current_model(model)
        
        if (is.null(model)) {
          "Error: Failed to train the model. Check data consistency or predictors."
        } else {
          # Create new data for prediction from inputs
          new_data <- data.frame(row.names = 1)
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
          
          # Make the prediction
          prediction <- tryCatch({
            predict(model, newdata = new_data)
          }, error = function(e) {
            paste("Error making prediction:", e$message)
          })
          
          if (is.character(prediction)) {
            prediction
          } else {
            paste("Predicted", target, ":", round(prediction, 4))
          }
        }
      }
    })
    
    # Display model summary if available
    output$model_summary <- renderPrint({
      model <- current_model()
      if (!is.null(model)) {
        cat("Model Summary:\n")
        summary(model$finalModel)
      }
    })
  })
}

shinyApp(ui, server)