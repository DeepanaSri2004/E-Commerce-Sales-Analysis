# ============================================
# E-COMMERCE SALES ANALYSIS
# ============================================
# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
# Copy the existing dataset
data <- Online_Retail
# Check data
head(data)
# Check structure
str(data)
# Check dimensions
dim(data)
# Check column names
names(data)
# Create Sales column
data <- data %>%
  mutate(Sales = Quantity * UnitPrice)
# Check the new column
head(data[, c("Quantity", "UnitPrice", "Sales")])
# STEP 5: Create Date Features
data <- data %>%
  mutate(
    Year = year(InvoiceDate),
    Month = month(InvoiceDate),
    Month_Name = month(InvoiceDate, label = TRUE),
    Day = day(InvoiceDate),
    Hour = hour(InvoiceDate)
  )
# Check the new columns
head(data[, c("InvoiceDate", "Year", "Month", "Month_Name", "Day", "Hour")])
# STEP 6: Basic Sales Summary
total_sales <- sum(data$Sales)
total_quantity <- sum(data$Quantity)
total_customers <- n_distinct(data$CustomerID)
total_products <- n_distinct(data$StockCode)
total_countries <- n_distinct(data$Country)
total_sales
total_quantity
total_customers
total_products
total_countries
# STEP 7: Monthly Sales Analysis
monthly_sales <- data %>%
  group_by(Year, Month, Month_Name) %>%
  summarise(
    Total_Sales = sum(Sales),
    .groups = "drop"
  )
# Display monthly sales
monthly_sales
# Monthly Sales Graph
ggplot(
  monthly_sales,
  aes(
    x = Month_Name,
    y = Total_Sales,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly E-Commerce Sales",
    x = "Month",
    y = "Total Sales"
  ) +
  theme_minimal()
# STEP 8: Top 10 Products by Sales
product_sales <- data %>%
  group_by(Description) %>%
  summarise(
    Total_Sales = sum(Sales),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Sales))
# Display top 10 products
head(product_sales, 10)
# Top 10 Products Graph
top_products <- product_sales %>%
  slice_head(n = 10)
ggplot(
  top_products,
  aes(
    x = reorder(Description, Total_Sales),
    y = Total_Sales
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Products by Sales",
    x = "Product",
    y = "Total Sales"
  ) +
  theme_minimal()
# STEP 9: Country-wise Sales Analysis
country_sales <- data %>%
  group_by(Country) %>%
  summarise(
    Total_Sales = sum(Sales),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Sales))
# Display top 10 countries
head(country_sales, 10)
# Top 10 Countries by Sales
top_countries <- country_sales %>%
  slice_head(n = 10)
ggplot(
  top_countries,
  aes(
    x = reorder(Country, Total_Sales),
    y = Total_Sales
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Countries by E-Commerce Sales",
    x = "Country",
    y = "Total Sales"
  ) +
  theme_minimal()
# STEP 10: Quantity vs Sales Analysis
ggplot(
  data,
  aes(
    x = Quantity,
    y = Sales
  )
) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Quantity vs Sales",
    x = "Quantity",
    y = "Sales"
  ) +
  theme_minimal()
library(ggplot2)
p <- ggplot(data, aes(x = Quantity, y = Sales)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Quantity vs Sales",
    x = "Quantity",
    y = "Sales"
  ) +
  theme_minimal()
print(p)
sales_median <- median(data$Sales)
data <- data %>%
  mutate(
    Sales_Level = ifelse(
      Sales >= sales_median,
      "High",
      "Low"
    )
  )
data$Sales_Level <- as.factor(data$Sales_Level)
table(data$Sales_Level)
ml_data <- data %>%
  select(
    Quantity,
    UnitPrice,
    Month,
    Day,
    Hour,
    Sales_Level
  )
head(ml_data)
set.seed(123)
train_index <- createDataPartition(
  ml_data$Sales_Level,
  p = 0.80,
  list = FALSE
)
train_data <- ml_data[train_index, ]
test_data <- ml_data[-train_index, ]
dim(train_data)
dim(test_data)
data <- data %>%
  filter(!is.na(Description)) %>%
  filter(!grepl("^C", InvoiceNo)) %>%
  filter(Quantity > 0, UnitPrice > 0)
data <- data %>%
  mutate(
    Sales = Quantity * UnitPrice,
    Year = year(InvoiceDate),
    Month = month(InvoiceDate),
    Month_Name = month(InvoiceDate, label = TRUE),
    Day = day(InvoiceDate),
    Hour = hour(InvoiceDate)
  )
sales_median <- median(data$Sales)
data <- data %>%
  mutate(
    Sales_Level = ifelse(
      Sales >= sales_median,
      "High",
      "Low"
    )
  )
data$Sales_Level <- as.factor(data$Sales_Level)
ml_data <- data %>%
  select(
    Quantity,
    UnitPrice,
    Month,
    Day,
    Hour,
    Sales_Level
  )
set.seed(123)
train_index <- createDataPartition(
  ml_data$Sales_Level,
  p = 0.80,
  list = FALSE
)
train_data <- ml_data[train_index, ]
test_data <- ml_data[-train_index, ]
dim(train_data)
dim(test_data)
decision_tree <- rpart(
  Sales_Level ~ Quantity +
    UnitPrice +
    Month +
    Day +
    Hour,
  data = train_data,
  method = "class"
)
rpart.plot(decision_tree)
tree_prediction <- predict(
  decision_tree,
  test_data,
  type = "class"
)
head(tree_prediction)
tree_cm <- confusionMatrix(
  tree_prediction,
  test_data$Sales_Level,
  positive = "High"
)
tree_cm
set.seed(123)
random_forest <- randomForest(
  Sales_Level ~ Quantity +
    UnitPrice +
    Month +
    Day +
    Hour,
  data = train_data,
  ntree = 100,
  importance = TRUE
)
rf_prediction <- predict(
  random_forest,
  test_data
)
head(rf_prediction)
rf_cm <- confusionMatrix(
  rf_prediction,
  test_data$Sales_Level,
  positive = "High"
)
rf_cm
rf_accuracy <- rf_cm$overall["Accuracy"]
rf_precision <- rf_cm$byClass["Pos Pred Value"]
rf_recall <- rf_cm$byClass["Sensitivity"]
rf_f1 <- rf_cm$byClass["F1"]
rf_accuracy
rf_precision
rf_recall
rf_f1
tree_accuracy <- tree_cm$overall["Accuracy"]
tree_precision <- tree_cm$byClass["Pos Pred Value"]
tree_recall <- tree_cm$byClass["Sensitivity"]
tree_f1 <- tree_cm$byClass["F1"]
comparison <- data.frame(
  Algorithm = c("Decision Tree", "Random Forest"),
  Accuracy = c(tree_accuracy, rf_accuracy),
  Precision = c(tree_precision, rf_precision),
  Recall = c(tree_recall, rf_recall),
  F1_Score = c(tree_f1, rf_f1)
)
comparison
comparison_long <- data.frame(
  Algorithm = rep(comparison$Algorithm, each = 4),
  Metric = rep(
    c("Accuracy", "Precision", "Recall", "F1 Score"),
    times = 2
  ),
  Score = c(
    comparison$Accuracy,
    comparison$Precision,
    comparison$Recall,
    comparison$F1_Score
  )
)

ggplot(
  comparison_long,
  aes(x = Algorithm, y = Score, fill = Metric)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Machine Learning Algorithm Comparison",
    x = "Algorithm",
    y = "Score"
  ) +
  theme_minimal()
data <- data %>%
  mutate(
    Sales = Quantity * UnitPrice,
    Year = year(InvoiceDate),
    Month = month(InvoiceDate),
    Month_Name = month(InvoiceDate, label = TRUE)
  )
monthly_sales <- data %>%
  group_by(Year, Month, Month_Name) %>%
  summarise(
    Total_Sales = sum(Sales),
    .groups = "drop"
  )
ggplot(
  monthly_sales,
  aes(x = Month_Name, y = Total_Sales, group = 1)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly E-Commerce Sales",
    x = "Month",
    y = "Total Sales"
  ) +
  theme_minimal()
product_sales <- data %>%
  group_by(Description) %>%
  summarise(
    Total_Sales = sum(Sales),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Sales))

top_products <- product_sales %>%
  slice_head(n = 10)

ggplot(
  top_products,
  aes(
    x = reorder(Description, Total_Sales),
    y = Total_Sales
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Products by Sales",
    x = "Product",
    y = "Total Sales"
  ) +
  theme_minimal()
country_sales <- data %>%
  group_by(Country) %>%
  summarise(
    Total_Sales = sum(Sales),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Sales))

top_countries <- country_sales %>%
  slice_head(n = 10)

ggplot(
  top_countries,
  aes(
    x = reorder(Country, Total_Sales),
    y = Total_Sales
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Countries by E-Commerce Sales",
    x = "Country",
    y = "Total Sales"
  ) +
  theme_minimal()
ggplot(
  data,
  aes(x = Quantity, y = Sales)
) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Quantity vs Sales",
    x = "Quantity",
    y = "Sales"
  ) +
  theme_minimal()
library(ggplot2)

p <- ggplot(data, aes(x = Quantity, y = Sales)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Quantity vs Sales",
    x = "Quantity",
    y = "Sales"
  ) +
  theme_minimal()

print(p)
sales_median <- median(data$Sales)

data <- data %>%
  mutate(
    Sales_Level = ifelse(
      Sales >= sales_median,
      "High",
      "Low"
    )
  )

data$Sales_Level <- as.factor(data$Sales_Level)

ml_data <- data %>%
  select(
    Quantity,
    UnitPrice,
    Month,
    Day,
    Hour,
    Sales_Level
  )

set.seed(123)

train_index <- createDataPartition(
  ml_data$Sales_Level,
  p = 0.80,
  list = FALSE
)

train_data <- ml_data[train_index, ]
test_data <- ml_data[-train_index, ]

dim(train_data)
dim(test_data)
library(lubridate)

data <- data %>%
  mutate(
    Year = year(InvoiceDate),
    Month = month(InvoiceDate),
    Month_Name = month(InvoiceDate, label = TRUE),
    Day = day(InvoiceDate),
    Hour = hour(InvoiceDate)
  )
ml_data <- data %>%
  select(
    Quantity,
    UnitPrice,
    Month,
    Day,
    Hour,
    Sales_Level
  )
set.seed(123)

train_index <- createDataPartition(
  ml_data$Sales_Level,
  p = 0.80,
  list = FALSE
)

train_data <- ml_data[train_index, ]
test_data <- ml_data[-train_index, ]decision_tree <- rpart(
  Sales_Level ~ Quantity +
    UnitPrice +
    Month +
    Day +
    Hour,
  data = train_data,
  method = "class"
)
decision_tree <- rpart(
  Sales_Level ~ Quantity +
    UnitPrice +
    Month +
    Day +
    Hour,
  data = train_data,
  method = "class"
)
tree_prediction <- predict(
  decision_tree,
  test_data,
  type = "class"
)
tree_cm <- confusionMatrix(
  tree_prediction,
  test_data$Sales_Level,
  positive = "High"
)
set.seed(123)
random_forest <- randomForest(
  Sales_Level ~ Quantity +
    UnitPrice +
    Month +
    Day +
    Hour,
  data = train_data,
  ntree = 100,
  importance = TRUE
)
print(random_forest)
rf_prediction <- predict(
  random_forest,
  test_data
)
rf_cm <- confusionMatrix(
  rf_prediction,
  test_data$Sales_Level,
  positive = "High"
)
rf_cm
comparison <- data.frame(
  Algorithm = c("Decision Tree", "Random Forest"),
  Accuracy = c(0.9727, 0.9999),
  Precision = c(0.9726, 0.9998),
  Recall = c(0.9731, 0.9999),
  F1_Score = c(0.9729, 0.9999)
)
comparison
comparison_long <- data.frame(
  Algorithm = rep(comparison$Algorithm, each = 4),
  Metric = rep(
    c("Accuracy", "Precision", "Recall", "F1 Score"),
    times = 2
  ),
  Score = c(
    comparison$Accuracy,
    comparison$Precision,
    comparison$Recall,
    comparison$F1_Score
  )
)

ggplot(
  comparison_long,
  aes(x = Algorithm, y = Score, fill = Metric)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Machine Learning Algorithm Comparison",
    x = "Algorithm",
    y = "Score"
  ) +
  theme_minimal()