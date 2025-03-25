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
  
  output$data_preview <- renderDataTable({ data() })
  output$data_summary <- renderPrint({ summary(data()) })
  output$column_names <- renderPrint({ colnames(data()) })
  
  output$numeric_columns <- renderPrint({
    num_cols <- names(Filter(is.numeric, data()))
    paste(num_cols, collapse = ", ")
  })
  
  output$null_values <- renderPrint({
    colSums(is.na(data()))
  })
  
  output$duplicates <- renderPrint({
    sum(duplicated(data()))
  })
  
  output$boxplot <- renderPlot({
    req(data())
    num_cols <- names(Filter(is.numeric, data()))
    if (length(num_cols) > 0) {
      ggplot(data(), aes_string(x = "", y = num_cols[1])) +
        geom_boxplot() +
        labs(title = paste("Boxplot of", num_cols[1]))
    }
  })
  
  output$scatterplot <- renderPlot({
    req(data())
    num_cols <- names(Filter(is.numeric, data()))
    if (length(num_cols) > 1) {
      ggplot(data(), aes_string(x = num_cols[1], y = num_cols[2])) +
        geom_point() +
        labs(title = "Scatter Plot")
    }
  })
  
  #code for histogram 
  output$histogram <- renderPlot({
    req(data())
    num_cols <- names(Filter(is.numeric, data()))
    if (length(num_cols) > 0) {
      ggplot(data(), aes_string(x = num_cols[1])) +
        geom_histogram(binwidth = 10, fill = "blue", alpha = 0.5) +
        labs(title = paste("Histogram of", num_cols[1]))
    }
  })
  
  output$heatmap <- renderPlot({
    req(data())
    num_data <- Filter(is.numeric, data())
    corr_matrix <- cor(num_data, use = "complete.obs")
    corrplot(corr_matrix, method = "color")
  })
  
  output$pairplot <- renderPlot({
    req(data())
    num_data <- Filter(is.numeric, data())
    ggpairs(num_data)
  })
  
  output$prediction_target_ui <- renderUI({
    req(data())
    colnames <- names(data())
    selectInput("target_variable", "Select Target Variable", choices = colnames)
  })
  
  output$predictor_inputs_ui <- renderUI({
    req(data(), input$target_variable)
    df <- data()
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
    req(data(), input$target_variable)
    df <- data()
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