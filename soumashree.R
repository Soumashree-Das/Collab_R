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
      uiOutput("prediction_target_ui"),
      uiOutput("predictor_inputs_ui"),
      uiOutput("heatmap_columns_ui"),
      actionButton("remove_outliers", "Remove Outliers", class = "btn-danger"),
      actionButton("run_prediction", "Run Prediction")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Overview",
                 dataTableOutput("data_preview"),
                 verbatimTextOutput("data_summary"),
                 verbatimTextOutput("row_count_before"),
                 verbatimTextOutput("col_count_before"),
                 verbatimTextOutput("row_count_after"),
                 verbatimTextOutput("col_count_after")
        ),
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
        tabPanel("Histograms", plotOutput("histogram")),
        tabPanel("Heatmap", plotOutput("heatmap", height = "600px", width = "600px")),
        tabPanel("Prediction Results", verbatimTextOutput("prediction_results"))
      )
    )
  )
)

server <- function(input, output, session) {
  original_data <- reactiveVal(NULL)
  cleaned_data <- reactiveVal(NULL)

  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath)
    original_data(df)
    cleaned_data(df)
  })

  data <- reactive({
    req(cleaned_data())
    cleaned_data()
  })

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
    paste("Rows after outlier removal:", nrow(cleaned_data()))
  })

  output$col_count_after <- renderPrint({
    req(cleaned_data())
    paste("Columns after outlier removal:", ncol(cleaned_data()))
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
      df_long <- tidyr::pivot_longer(data(), all_of(num_cols), names_to = "Variable", values_to = "Value")
      ggplot(df_long, aes(x = Variable, y = Value)) +
        geom_boxplot() +
        labs(title = "Boxplots of All Numeric Columns", x = "Numeric Variables", y = "Values") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      plot.new()
      title("No Numeric Columns Available")
    }
  })

  output$outlier_count <- renderPrint({
    req(data())
    num_cols <- names(Filter(is.numeric, data()))
    if (length(num_cols) > 0) {
      outlier_counts <- sapply(num_cols, function(col) {
        Q1 <- quantile(data()[[col]], 0.25, na.rm = TRUE)
        Q3 <- quantile(data()[[col]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        sum(data()[[col]] < (Q1 - 1.5 * IQR) | data()[[col]] > (Q3 + 1.5 * IQR), na.rm = TRUE)
      })
      print(outlier_counts)
    } else {
      print("No numeric columns available to detect outliers.")
    }
  })

  output$scatterplot <- renderPlot({
    req(data())
    num_cols <- names(Filter(is.numeric, data()))
    if (length(num_cols) > 1) {
      ggplot(data(), aes_string(x = num_cols[1], y = num_cols[2])) +
        geom_point() +
        labs(title = "Scatter Plot", x = num_cols[1], y = num_cols[2])
    } else {
      plot.new()
      title("Not Enough Numeric Columns for Scatter Plot")
    }
  })

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
    num_cols <- names(Filter(is.numeric, data()))
    if (length(num_cols) > 0) {
      new_data <- cleaned_data()
      for (col in num_cols) {
        Q1 <- quantile(new_data[[col]], 0.25, na.rm = TRUE)
        Q3 <- quantile(new_data[[col]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        new_data <- new_data[new_data[[col]] >= (Q1 - 1.5 * IQR) &
                               new_data[[col]] <= (Q3 + 1.5 * IQR), ]
      }
      cleaned_data(new_data)
      removeModal()
    }
  })

  output$histogram <- renderPlot({
    req(data())
    num_cols <- names(Filter(is.numeric, data()))
    if (length(num_cols) > 0) {
      ggplot(data(), aes_string(x = num_cols[1])) +
        geom_histogram(binwidth = 10, fill = "blue", alpha = 0.5) +
        labs(title = paste("Histogram of", num_cols[1]))
    }
  })

  output$heatmap_columns_ui <- renderUI({
    req(data())
    num_cols <- names(Filter(is.numeric, data()))
    selectInput("heatmap_cols", "Select Columns for Heatmap",
                choices = num_cols,
                multiple = TRUE,
                selected = num_cols[1:min(5, length(num_cols))])
  })

  output$heatmap <- renderPlot({
    req(data(), input$heatmap_cols)
    num_data <- data()[, input$heatmap_cols, drop = FALSE]
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
        # Ensure choices are not NA and convert to character
        choices <- unique(df[[col]])
        choices <- choices[!is.na(choices)]
        selectInput(paste0("input_", col), label = col, choices = as.character(choices))
      }
    })
    do.call(tagList, input_fields)
  })

  observeEvent(input$run_prediction, {
    req(data(), input$target_variable)
    df <- data()
    target <- input$target_variable
    predictors <- setdiff(names(df), target)
    
    # Subset the data
    df <- df %>% select(all_of(c(target, predictors)))
    
    # Replace NA with 0
    df <- df %>% mutate_all(~replace(., is.na(.), 0))
    
    # Convert categorical predictors to factors, ensure target is numeric
    df <- df %>% mutate(across(where(~!is.numeric(.)), as.factor))
    if (!is.numeric(df[[target]])) {
      output$prediction_results <- renderPrint({
        "Error: Target variable must be numeric for linear regression."
      })
      return()
    }
    
    output$prediction_results <- renderPrint({
      if (nrow(df) < 2) {
        "Error: At least 2 rows are required for prediction modeling."
      } else if (nrow(df) <= length(predictors) + 1) {
        mean_value <- mean(df[[target]], na.rm = TRUE)
        new_data <- as.data.frame(lapply(predictors, function(col) {
          value <- input[[paste0("input_", col)]]
          if (is.numeric(df[[col]])) as.numeric(value) else as.character(value)
        }))
        colnames(new_data) <- predictors
        paste("Dataset too small for regression. Using mean prediction.\n",
              "Predicted", target, ":", mean_value)
      } else if (all(duplicated(df[[target]])[-1L]) || 
                 any(sapply(df[predictors], function(x) all(duplicated(x)[-1L])))) {
        mean_value <- mean(df[[target]], na.rm = TRUE)
        paste("No variation in data. Using mean prediction.\nPredicted", target, ":", mean_value)
      } else {
        model <- tryCatch({
          train(as.formula(paste(target, "~ .")), 
                data = df, 
                method = "lm",
                trControl = trainControl(method = "none"))
        }, error = function(e) {
          cat("Model training error:", e$message, "\n")
          return(NULL)
        })
        
        if (is.null(model)) {
          "Error: Failed to train the model. Check data consistency or predictors."
        } else {
          # Create new data for prediction
          new_data <- as.data.frame(lapply(predictors, function(col) {
            value <- input[[paste0("input_", col)]]
            if (is.numeric(df[[col]])) as.numeric(value) else as.character(value)
          }))
          colnames(new_data) <- predictors
          new_data <- new_data %>% mutate_all(~replace(., is.na(.), 0))
          
          # Align factor levels in new_data with df
          for (col in predictors) {
            if (is.factor(df[[col]])) {
              # Set levels to match df, replace unknown levels with the most common level
              new_data[[col]] <- factor(new_data[[col]], levels = levels(df[[col]]))
              if (any(is.na(new_data[[col]]))) {
                most_common <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
                new_data[[col]][is.na(new_data[[col]])] <- most_common
              }
            }
          }
          
          prediction <- tryCatch({
            predict(model, newdata = new_data)
          }, error = function(e) {
            paste("Error making prediction:", e$message)
          })
          
          if (is.character(prediction)) {
            prediction
          } else {
            paste("Predicted", target, ":", prediction)
          }
        }
      }
    })
  })
}

shinyApp(ui, server)
