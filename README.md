# Collab_R

# How to run
Please download main.R, ui.R, utils.R, preprocessing.R, server.R files to run the program
P.S. All other files are our previous codes

---


# 📊 R Shiny AutoML & Data Exploration App

This R Shiny application is a powerful, interactive tool designed to simplify the process of data analysis, cleaning, visualization, and machine learning — all in one seamless workflow. It is ideal for data enthusiasts, researchers, and students who want to analyze any dataset without writing complex code.

---

## 🚀 Features

- **🔍 Dataset Overview**
  - Upload any `.csv` file
  - Automatically displays dataset summary, column names, structure, missing values, and duplicates

- **🧹 Data Cleaning**
  - Detect and handle missing values
  - Remove duplicate rows
  - Identify and optionally remove outliers manually or automatically

- **📊 Visualizations**
  - Histogram, Boxplot, Scatterplot
  - Correlation heatmap
  - Pair plots
  - Dynamic selection of variables for customized plotting

- **📈 Outlier Detection**
  - Boxplot-based outlier detection
  - Visual interface to remove or retain outliers

- **🤖 Machine Learning Models**
  - Automatically runs multiple ML models (e.g., Linear Regression, Decision Tree, Random Forest, etc.)
  - Compares models based on accuracy, RMSE, and other relevant metrics
  - Highlights the best-performing model

- **🔮 Prediction Engine**
  - Interactive UI for users to select target variable
  - Automatically detects whether prediction is regression or classification
  - Uses the best-performing model for final prediction
  - Allows custom input values for prediction

---

## 🛠️ Tech Stack

- **Language:** R  
- **Framework:** Shiny  
- **Packages Used:**  
  - `tidyverse` – Data manipulation  
  - `ggplot2` – Visualization  
  - `plotly` – Interactive plots  
  - `DT` – Data tables  
  - `caret` – Model training and evaluation  
  - `shinyWidgets`, `shinydashboard`, `shinyjs` – UI enhancements

---

## 📂 How to Use

1. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/shiny-dataset-analyzer.git

