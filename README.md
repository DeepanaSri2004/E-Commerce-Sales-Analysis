# E-Commerce Sales Analysis & Machine Learning Prediction

## Project Overview

This R project performs an analysis of e-commerce sales data and applies machine learning techniques to classify sales into **High** and **Low** sales levels.

The project includes data preparation, sales analysis, visualization, and machine learning model comparison using Decision Tree and Random Forest algorithms.

## Objectives

- Calculate total sales and quantities
- Analyze monthly sales trends
- Identify top-selling products
- Analyze sales by country
- Study the relationship between quantity and sales
- Classify sales into High and Low categories
- Build a Decision Tree classification model
- Build a Random Forest classification model
- Compare model performance using Accuracy, Precision, Recall, and F1 Score

## Technologies Used

- R
- dplyr
- ggplot2
- lubridate
- rpart
- rpart.plot
- randomForest
- caret

## Analysis Performed

### 1. Sales Calculation
Sales are calculated using:

`Sales = Quantity × UnitPrice`

### 2. Time-Based Analysis
The project extracts:

- Year
- Month
- Month Name
- Day
- Hour

### 3. Sales Analysis
The project analyzes:

- Total sales
- Total quantity
- Total customers
- Total products
- Total countries
- Monthly sales
- Top 10 products by sales
- Top 10 countries by sales

### 4. Machine Learning

Sales are classified into two categories:

- High
- Low

The dataset is divided into:

- 80% Training Data
- 20% Testing Data

Two classification algorithms are used:

1. Decision Tree
2. Random Forest

The models are evaluated using:

- Accuracy
- Precision
- Recall
- F1 Score

## Model Comparison

The R project compares the Decision Tree and Random Forest models using the above performance metrics.

## How to Run

1. Install R and RStudio.
2. Install the required packages:

```r
install.packages(c(
  "dplyr",
  "ggplot2",
  "lubridate",
  "rpart",
  "rpart.plot",
  "randomForest",
  "caret"
))
```

3. Make sure the `Online_Retail` dataset is loaded in R.
4. Open `E-commerce.R`.
5. Run the script from top to bottom.

## Project Files

```text
E-Commerce-Sales-Analysis/
├── E-commerce.R
├── README.md
└── .gitignore
```

## Author

Deepanasri S
