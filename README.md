# Collab_R

# How to run
Please download main.R, ui.R, utils.R, preprocessing.R, server.R files to run the program
P.S. All other files are our previous codes

---


# 📊 R Shiny AutoML & Data Exploration App

This R Shiny application is a powerful, interactive tool designed to simplify data analysis, cleaning, visualization, and machine learning—all in one seamless workflow. It is ideal for data enthusiasts, researchers, and students who want to analyze any dataset without writing complex code.

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
  - Interactive UI for users to select the target variable
  - Automatically detects whether the prediction is regression or classification
  - Uses the best-performing model for final prediction
  - Allows custom input values for prediction

---

## 🛠️ Tech Stack

- **Language:** R  
- **Framework:** Shiny  
- **Packages Used:**
  - `dplyr` - Data Manipulation
  - `GGally` - Better Visualizations (pair plots)
  - `corrplot` - Correlation Matrices
  - `tidyr` - Data reshaping
  - `randomForest` - Random Forest ML Model
  - `xgboost` - Extreme Gradient Boosting ML Powerhouse
  - `e1071` - Support Vector Machine
  - `gbm` - Gradient Boosting Machine
  - `earth` - Multivariate Adaptive Regression Splines
  - `ggplot2` – Visualization  
  - `plotly` – Interactive plots  
  - `DT` – Data tables  
  - `caret` – Model training and evaluation  
  - `shinyWidgets` – UI enhancements

---


