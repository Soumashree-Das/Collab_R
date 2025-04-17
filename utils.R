# utils.R
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