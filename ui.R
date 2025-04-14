# ui.R
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