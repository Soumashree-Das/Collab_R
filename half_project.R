library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(GGally)
library(corrplot)
library(shinyWidgets)
library(caret)

ui <- fluidPage(
  titlePanel("Collaborative Data Analysis & Prediction Workspace"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("drop_columns_ui"),  # UI for selecting columns to drop
      uiOutput("drop_rows_ui"),     # UI for selecting rows to drop
      checkboxInput("auto_clean", "Automatic Cleaning", FALSE),  # Checkbox for automatic cleaning
      uiOutput("prediction_target_ui"),
      uiOutput("predictor_inputs_ui"),
      actionButton("run_prediction", "Run Prediction")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Overview", dataTableOutput("data_preview"), verbatimTextOutput("data_summary")),
        tabPanel("Column Names", verbatimTextOutput("column_names")),
        tabPanel("Numeric Columns", verbatimTextOutput("numeric_columns")),
        tabPanel("Null Values", verbatimTextOutput("null_values")),
        tabPanel("Duplicate Rows", verbatimTextOutput("duplicates")),
        tabPanel("Outlier Detection", plotOutput("boxplot"), plotOutput("scatterplot")),
        tabPanel("Histograms", plotOutput("histogram")),
        tabPanel("Heatmap", plotOutput("heatmap")),
        tabPanel("Pair Plot", plotOutput("pairplot")),
        tabPanel("Prediction Results", verbatimTextOutput("prediction_results"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # UI for selecting columns to drop
  output$drop_columns_ui <- renderUI({
    req(data())
    selectInput("drop_columns", "Select Columns to Drop", choices = names(data()), multiple = TRUE)
  })
  
  # UI for selecting rows to drop
  output$drop_rows_ui <- renderUI({
    req(data())
    textInput("drop_rows", "Drop Rows (Enter Row Indices, comma-separated)", value = "")
  })
  
  # Reactive expression for data cleaning
  cleaned_data <- reactive({
    req(data())
    df <- data()
    
    # Drop selected columns
    if (!is.null(input$drop_columns)) {
      df <- df %>% select(-all_of(input$drop_columns))
    }
    
    # Drop selected rows
    if (input$drop_rows != "") {
      row_indices <- as.numeric(unlist(strsplit(input$drop_rows, ",")))  # Convert input to numeric vector
      row_indices <- na.omit(row_indices)  # Remove invalid values
      df <- df[-row_indices, , drop = FALSE]  # Drop rows safely
    }
    
    # Automatic cleaning
    if (input$auto_clean) {
      # 1️⃣ Remove columns with more than 50% missing values
      missing_threshold <- 0.5  # User can modify this threshold
      drop_missing <- names(df)[colMeans(is.na(df)) > missing_threshold]
      df <- df %>% select(-all_of(drop_missing))
      
      # 2️⃣ Remove near-zero variance columns
      if (ncol(df) > 1) {  # Ensure at least 1 column remains
        nzv <- nearZeroVar(df, saveMetrics = TRUE)
        drop_nzv <- rownames(nzv[nzv$nzv, ])
        df <- df %>% select(-all_of(drop_nzv))
      }
      
      # 3️⃣ Remove highly correlated columns (correlation > 0.9)
      num_cols <- select(df, where(is.numeric))
      if (ncol(num_cols) > 1) {
        cor_matrix <- cor(num_cols, use = "complete.obs")
        high_corr <- findCorrelation(cor_matrix, cutoff = 0.9, names = TRUE)
        df <- df %>% select(-all_of(high_corr))
      }
    }
    df
  })
  
  output$data_preview <- renderDataTable({ cleaned_data() })
  output$data_summary <- renderPrint({ summary(cleaned_data()) })
  output$column_names <- renderPrint({ colnames(cleaned_data()) })
  
  output$numeric_columns <- renderPrint({
    num_cols <- names(Filter(is.numeric, cleaned_data()))
    paste(num_cols, collapse = ", ")
  })
  
  output$null_values <- renderPrint({
    colSums(is.na(cleaned_data()))
  })
  
  output$duplicates <- renderPrint({
    sum(duplicated(cleaned_data()))
  })
  
  output$boxplot <- renderPlot({
    req(cleaned_data())
    num_cols <- names(Filter(is.numeric, cleaned_data()))
    if (length(num_cols) > 0) {
      ggplot(data(), aes_string(x = "", y = num_cols[1])) +
        geom_boxplot() +
        labs(title = paste("Boxplot of", num_cols[1]))
    }
  })
  
  output$scatterplot <- renderPlot({
    req(cleaned_data())
    num_cols <- names(Filter(is.numeric, cleaned_data()))
    if (length(num_cols) > 1) {
      ggplot(data(), aes_string(x = num_cols[1], y = num_cols[2])) +
        geom_point() +
        labs(title = "Scatter Plot")
    }
  })
  
  #code for histogram 
  output$histogram <- renderPlot({
    req(cleaned_data())
    num_cols <- names(Filter(is.numeric, cleaned_data()))
    if (length(num_cols) > 0) {
      ggplot(data(), aes_string(x = num_cols[1])) +
        geom_histogram(binwidth = 10, fill = "blue", alpha = 0.5) +
        labs(title = paste("Histogram of", num_cols[1]))
    }
  })
  
  output$heatmap <- renderPlot({
    req(cleaned_data())
    num_data <- Filter(is.numeric, cleaned_data())
    corr_matrix <- cor(num_data, use = "complete.obs")
    corrplot(corr_matrix, method = "color")
  })
  
  output$pairplot <- renderPlot({
    req(cleaned_data())
    num_data <- Filter(is.numeric, cleaned_data())
    ggpairs(num_data)
  })
  
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
        selectInput(paste0("input_", col), label = col, choices = unique(df[[col]]))
      }
    })
    do.call(tagList, input_fields)
  })
  
  observeEvent(input$run_prediction, {
    req(cleaned_data(), input$target_variable)
    df <- cleaned_data()
    target <- input$target_variable
    predictors <- setdiff(names(df), target)
    df <- df %>% select(all_of(c(target, predictors)))
    df <- na.omit(df)
    
    model <- train(as.formula(paste(target, "~ .")), data = df, method = "lm")
    
    new_data <- as.data.frame(lapply(predictors, function(col) {
      input[[paste0("input_", col)]]
    }))
    colnames(new_data) <- predictors
    
    prediction <- predict(model, new_data)
    output$prediction_results <- renderPrint({
      paste("Predicted", target, ":", prediction)
    })
  })
}

shinyApp(ui, server)