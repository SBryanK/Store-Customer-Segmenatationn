#DATA WRANGLING AND CLEANING for Supervised Learning (Import Library for data wrangling and cleaning)
library(tidyverse)

#data is Walmart Data set (Supervised Learning)
data <- read.csv("1. Walmart.csv")
str(data) #displaying the data lists of content 

#Check any missing value in each column
missing_count = sum (is.na(data))
total_missing1 <- colSums(is.na(data))
print(total_missing1)

#Check any duplicated value in each rows 
unique_rows1 <- data[duplicated(data), ]
print(unique_rows1)

#DATA WRANGLING AND CLEANING foR Unsupervised Learning (Import Library for data wrangling and cleaning)
library(tidyverse)
#data2 is Online Retail Data set-----------------------------------------------------------------
data2 = read.csv("2. Online Retail.csv")
str(data2)

#Check any missing value in each column
missing_count = sum (is.na(data2))
total_missing2 <- colSums(is.na(data2))
print(total_missing2)

#Check any duplicated value in each rows 
data2 <- data2[data2$Quantity > 0 & data2$UnitPrice > 0, ]
print(unique_rows2)

#check any 0 values in Quantity and UnitPrice
data2 <- data2[data2$Quantity > 0 & data2$UnitPrice > 0, ]
# Drop rows where either 'Quantity' or 'UnitPrice' (or both) are zero
data2 <- data2[!(data2$Quantity <= 0 | data2$UnitPrice <= 0), ]
print(data2)
### SUPERVISED LEARNING

# CORRELATION PLOT ---------------------------------------------------------
# Load necessary libraries
library(ggplot2)
library(reshape2)
library(ggcorrplot)

# Read the data from CSV
data <- read.csv('1. Walmart.csv')

# Convert 'Date' to Date type and create 'Year', 'Month', and 'Day' columns
data$Date <- as.Date(data$Date, format="%d-%m-%Y")
data$Year <- format(as.Date(data$Date), "%Y")
data$Month <- format(as.Date(data$Date), "%m")
data$Day <- format(as.Date(data$Date), "%d")

# Calculate the correlation matrix
correlation_matrix <- cor(data[sapply(data, is.numeric)])

# Create the correlation plot
ggcorrplot(correlation_matrix, method="circle", type = "lower", 
           lab = TRUE, lab_size = 3, tl.cex = 10, tl.srt = 45, 
           title="Correlation plot for Walmart Dataset", 
           ggtheme=theme_minimal())



# WWEKLY SALES DISTRIBUTION - SINGLE BOXPLOT-----------------------------------------------------------------------------------
# Load necessary library for plotting
library(ggplot2) 

# Read the data from CSV
data <- read.csv('1. Walmart.csv')

# Create a box plot of Weekly Sales
ggplot(data, aes(y = Weekly_Sales)) +
  geom_boxplot(horizontal = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Distribution of Weekly Sales", y = "Weekly Sales") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)
  )

# DISTRIBUTION OF STORES WEEEKLY SALES - MULTIPLE BOXPLOTS --------------------------------------------------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data from CSV
data <- read.csv('1. Walmart.csv')

# Calculate the mean weekly sales for each store
    mean_sales <- data %>% 
      group_by(Store) %>%
      summarise(Mean_Weekly_Sales = mean(Weekly_Sales)) %>%
      arrange(Mean_Weekly_Sales)

# Join the mean sales back to the original data
data <- data %>% 
  left_join(mean_sales, by = "Store")

# Order the factor levels of Store based on the Mean_Weekly_Sales
data$Store <- factor(data$Store, levels = mean_sales$Store)

# Create the boxplot
ggplot(data, aes(x = Store, y = Weekly_Sales)) +
  geom_boxplot(aes(fill = Mean_Weekly_Sales)) +
  scale_fill_gradient(low = "red", high = "green") +
  coord_flip() + # Flip coordinates to make the boxplot horizontal
  theme_minimal() +
  labs(title = "Mean of Weekly Sales Across Different Store", x = "Mean of Weekly Sales", y = "Store Number") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) # Hide the legend for the fill scale

# HOLIDAY VS NON HOLIDAY COMPARISON - BOXPLOTS  ------------------------------------------
# Load necessary library
library(ggplot2)

# Read the data from CSV
data <- read.csv('1. Walmart.csv')

# Convert the Holiday_Flag to a factor with readable levels
data$Holiday_Flag <- factor(data$Holiday_Flag, levels = c(0, 1), labels = c("Non-Holidays", "Holidays"))

# Create the boxplot
ggplot(data, aes(x = Holiday_Flag, y = Weekly_Sales, fill = Holiday_Flag)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Comparison of Weekly Sales ", 
       x = "", 
       y = "Weekly Sales") +
  scale_fill_manual(values = c("red", "cyan")) + # Specify the colors you want to use
  theme(legend.position = "none") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))



# Average Weekly sales by Starting Date - Line plot------------------------------------------
# Load necessary libraries (Use modified data set from Walmart named : 1. Average_Weekly_Sales.csv (Group by Weekly sales for every week))
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)

# Read the data from CSV
average_sales <- read.csv('1. Average_Weekly_Sales.csv')

# Convert the 'Date' column to Date object
average_sales$Date <- as.Date(average_sales$Date, "%Y-%m-%d")

# Create a new variable for color based on the condition
average_sales$Color <- ifelse(average_sales$Weekly_Sales > 1200000, "green", "pink")

# Create the time series line plot with color-coded points
ggplot(average_sales, aes(x = Date, y = Weekly_Sales)) +
  geom_line(color = "black") +
  geom_point(aes(color = Color)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%d-%m-%Y", minor_breaks = NULL) +
  scale_color_identity() +
  labs(title = "Average Weekly Sales by Date",
       x = "Date", 
       y = "Average Weekly Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), # Rotate x labels for readability
        legend.position = "none") + # Hide the legend
theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Check the max and min Weekly Sales
max(average_sales[ , 'Weekly_Sales'])
min(average_sales[ , 'Weekly_Sales'])
mean(average_sales[ , "Weekly_Sales"])


# THE ALTERNATIVE------------------------------------------------------------------------------------- 
# library(ggplot2)
# library(dplyr)
# library(readr)
# library(scales) # For date formatting
# 
# # Read the data
# average_sales_data <- read_csv("1. Average_Weekly_Sales.csv")
# 
# # Convert the 'Date' column to Date object
# average_sales_data$Date <- as.Date(average_sales_data$Date, "%Y-%m-%d")
# 
# # Create a new column for color based on condition
# average_sales_data <- average_sales_data %>%
#   mutate(Color = ifelse(Weekly_Sales > 1200000, "green", "red"))
# 
# # Generate a sequence of dates every three months within the range of your data
# date_breaks <- seq(min(average_sales_data$Date),
#                    max(average_sales_data$Date),
#                    by = "3 months")
# 
# # Generate the line chart with points
# ggplot(average_sales_data, aes(x=Date, y=Weekly_Sales)) +
#   geom_line(color="blue") +
#   geom_point(aes(color=Color), size=2) +  # Adds the points with conditional coloring
#   scale_color_identity() +  # Tells ggplot to use the actual colors specified in the 'Color' column
#   labs(title="Average Weekly Sales by Date",
#        x="Date", 
#        y="Average Weekly Sales") +
#   theme_minimal() +
#   scale_x_date(breaks = date_breaks, labels = date_format("%b %Y"))  # Set x-axis dates for every three months


# REGRESSION
# DIAGNOSIS PLOT-------------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(lubridate)

# Read the data
data <- read_csv("1. Walmart.csv")

# Convert Date to a date object and extract the month
data$Date <- as.Date(data$Date, format="%d-%m-%Y")
data$Month <- month(data$Date)

# Fit the linear model with the specified predictors
lm_model <- lm(Weekly_Sales ~ Store + Temperature + CPI + Unemployment + Month, data = data)

par(mfrow = c(2, 2))  # Set up the plotting area to have 2 rows and 2 columns
plot(lm_model)  # Generate diagnostic plots

# Get a summary of the model to extract R-squared and other statistics
summary_model <- summary(lm_model)

# Calculate RMSE
rmse <- sqrt(mean(summary_model$residuals^2))

# Output the R-squared and RMSE
cat("R-squared: ", summary_model$r.squared, "\n")
cat("RMSE: £", rmse, "\n")

# You can also access the coefficients directly if needed
coefficients(lm_model)

#The  most important variables are CPI, Store, and Unemployment

#LINEAR REGRESSION EQUATION--------------------------------------------------------------------------------------------
  library(readr)
  library(dplyr)
  library(lubridate)
  
  # Read the data
  data <- read_csv("1. Walmart.csv")
  
  # Convert Date to a date object and extract the month
  data$Month <- month(data$Date)
  
  # Fit the linear model with the specified predictors
  lm_model <- lm(Weekly_Sales ~ Store + Temperature + CPI + Unemployment + Date, data = data)
  
  # Get a summary of the model to extract R-squared and other statistics
  summary_model <- summary(lm_model)
  
  # Calculate RMSE
  rmse <- sqrt(mean(summary_model$residuals^2))
  
  # Output the R-squared and RMSE
  cat("R-squared: ", summary_model$r.squared, "\n")
  cat("RMSE: £", rmse, "\n")
  
  # You can also access the coefficients directly if needed
  coefficients(lm_model)

#LASSO AND RIDGE REGRESSION ---------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(lubridate)
library(glmnet)

# Read the data
data <- read_csv("1. Walmart.csv")

# Convert Date to a date object and extract the month
data$Month <- month(data$Date)

# Prepare the data for glmnet
predictors <- model.matrix(Weekly_Sales ~ Store + Temperature + CPI + Unemployment + Month - 1, data = data)
response <- data$Weekly_Sales

# Standardize predictors for penalized regression
predictors_standardized <- scale(predictors)

# Fit the Ridge regression model
ridge_model <- glmnet(predictors_standardized, response, alpha = 0)

# Fit the Lasso regression model
lasso_model <- glmnet(predictors_standardized, response, alpha = 1)

# Use cross-validation to find the optimal lambda
set.seed(2024) # For reproducibility
cv_ridge <- cv.glmnet(predictors_standardized, response, alpha = 0)
cv_lasso <- cv.glmnet(predictors_standardized, response, alpha = 1)

# Calculate RMSE for Ridge regression
ridge_pred <- predict(ridge_model, s = cv_ridge$lambda.min, newx = predictors_standardized)
ridge_rmse <- sqrt(mean((response - ridge_pred)^2))

# Calculate RMSE for Lasso regression
lasso_pred <- predict(lasso_model, s = cv_lasso$lambda.min, newx = predictors_standardized)
lasso_rmse <- sqrt(mean((response - lasso_pred)^2))

# Output RMSE for both models
cat("Ridge RMSE: £", ridge_rmse, "\n")
cat("Lasso RMSE: £", lasso_rmse, "\n")

# DECISION TREE  ------------------------------------------------------------------------------------
# Load the 'caret' package
library(caret)
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)

# Read the data
data <- read_csv("1. Walmart.csv")

# Convert Date to a date object and extract the month
data$Month <- month(data$Date)

# Fit the decision tree model with a complexity penalty
# Adjust the 'cp' parameter to penalize complexity. Lower values allow more complex trees.
dt_model <- rpart(Weekly_Sales ~ Store + Temperature + CPI + Unemployment + Month,
                  data = data,
                  method = "anova",
                  control = rpart.control(cp = 0.006)) # Example complexity parameter

# Plot the decision tree
pfit <- prune(dt_model, cp = dt_model$cptable[which.min(dt_model$cptable[,"xerror"]),"CP"])
rpart.plot(pfit, type = 4, main = "Decision Tree Model for Regression", cex = 0.5, # Larger text in the nodes
           cex.main = 1.5, under = FALSE, faclen = 1)

# Calculate variable importance
var_importance <- as.data.frame(varImp(dt_model))

# Calculate RMSE of the decision tree
predictions <- predict(dt_model, data)
rmse <- sqrt(mean((data$Weekly_Sales - predictions)^2))

# Output the variable importance and RMSE
print(var_importance)
cat("RMSE of Decision Tree: £", formatC(rmse, format = "f", digits = 1))

#RANDOM FOREST ---------------------------------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(lubridate)
library(randomForest)

# Read the data
data <- read_csv("1. Walmart.csv")

# Convert Date to a date object and extract the month
data$Month <- month(data$Date)

# Prepare the data for the randomForest model
# We select only the variables of interest
data <- data %>% 
  select(Weekly_Sales, Store, CPI, Unemployment, Month)

# Create the Random Forest model
# ntrees is set to 500 and mtry is set to 3
# mtry is the number of variables randomly sampled as candidates at each split
rf_model <- randomForest(Weekly_Sales ~ ., data = data, 
                         ntree = 500, mtry = 3, importance = TRUE)

# Calculate the RMSE of the Random Forest model
rf_pred <- predict(rf_model, data)
rf_rmse <- sqrt(mean((data$Weekly_Sales - rf_pred)^2))

# Output the RMSE
cat("Random Forest RMSE: £", rf_rmse, "\n")

# Get variable importance and print the top 3 variables
importance(rf_model)
varImpPlot(rf_model, n.var = 3, main = "Top 3 Variable Importance in Random Forest")

#CLASSIFICATION -----------------------------------------------------------------------------------
#LOGISTIC REGRESSION--------------------------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(lubridate)
library(glm2)

# Read the data
data <- read_csv("1. Walmart.csv") 
# Convert Date to a date object and extract the month
data$Date <- as.Date(data$Date, format="%d-%m-%Y")
data$Month <- month(data$Date)

# Create a binary outcome variable for the logistic regression
# Let's assume '1' means Weekly_Sales are above the median for this example
data$Sales_Class <- as.factor(ifelse(data$Weekly_Sales > median(data$Weekly_Sales),1,0))
# Fit the logistic regression model
logit_model <- glm(Sales_Class ~ Store + Temperature + CPI + Unemployment + Month, 
                   data = data, 
                   family = binomial(link = "logit"))

# Summary of the model to get coefficients
summary_logit <- summary(logit_model)

# Calculate accuracy
fitted.results <- ifelse(predict(logit_model, type='response') >= 0.5, 1, 0)
actual.results <- data$Sales_Class
accuracy <- mean(fitted.results == actual.results)
cat("Accuracy: ", accuracy*100, "%", "\n")

# Output the coefficients
print(summary_logit$coefficients)

# Output the odds ratios
odds_ratios <- coef(logit_model)
print(odds_ratios)

#SUPPORT VECTOR MACHINE ---------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(lubridate)
library(e1071)

# Read the data
data <- read_csv("1. Walmart.csv")

# Convert Date to a date object and extract the month

data$Month <- month(data$Date)

# Assuming 'Sales_Class' is the binary outcome variable you have created
# For example:
data$Sales_Class <- as.factor(ifelse(data$Weekly_Sales > median(data$Weekly_Sales), 'high', 'low'))

# Fit the SVM model with a radial basis function kernel
svm_model <- svm(Sales_Class ~ Store + Temperature + CPI + Unemployment + Month, 
                 data = data, 
                 kernel = "radial", 
                 cost = 1,  # the cost of constraints violation (it has to be tuned, e.g., using cross-validation)
                 gamma = 1) # the parameter of the radial basis function (to be tuned as well)

# Predict using the SVM model
svm_pred <- predict(svm_model, data)

# Calculate the accuracy of the SVM model
accuracy <- mean(svm_pred == data$Sales_Class)
cat("SVM Model Accuracy: ", accuracy * 100, "%", "\n")


#DECISION TREE---------------------------------------------------------
library(readr)
library(dplyr)
library(lubridate)
library(rpart)
library(rpart.plot)

# Read the data
data <- read_csv("1. Walmart.csv")

# Convert the 'Date' column to Date object and extract the month
data$Date <- as.Date(data$Date, format="%d-%m-%Y")
data$Month <- month(data$Date)

# Assuming that we have a binary target variable 'Target' (need to be created or defined)
# Here's a placeholder line for creating a binary target based on some criterion (e.g., sales above median)
data$Target <- as.factor(ifelse(data$Weekly_Sales > median(data$Weekly_Sales), 'Low', 'High'))

# Fit the decision tree model
tree_model <- rpart(Target ~ Store + Unemployment + CPI, 
                    data = data, 
                    method = "class",
)

# Plot the decision tree
rpart.plot(tree_model, main = "Decision Tree Model for Classification", under = TRUE, faclen = 0,
           cex = 0.5,       
           cex.main = 1.2)

# Calculate the model's accuracy
predictions <- predict(tree_model, data, type = "class")
accuracy <- sum(predictions == data$Target) / nrow(data)
cat("Model Accuracy: ", accuracy * 100, "%", "\n")

# Extract variable importance
importance <- as.data.frame(tree_model$variable.importance)
importance$variables <- rownames(importance)
rownames(importance) <- NULL  # Reset rownames to avoid confusion in ordering

# Order by importance value
importance <- importance[order(-importance[,1]), ]

# Now importance is ordered correctly
print(importance)

#KNN-------------------------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(lubridate)
library(class)

# Read the data
data <- read_csv("1. Walmart.csv")

# Convert Date to a date object and extract the month
data$Date <- as.Date(data$Date, format="%d-%m-%Y")
data$Month <- month(data$Date)

# Assuming that we have a binary target variable 'Target' (need to be created or defined)
# For example:
data$Target <- as.factor(ifelse(data$Weekly_Sales > median(data$Weekly_Sales), 1, 0))

# Normalize the predictors
scale_data <- scale(data[, c("Store", "CPI", "Unemployment")])

# Define the value of k as the square root of the number of observations
k <- round(sqrt(nrow(data)))

# Split data into training and testing sets (Here we assume a 70/30 split)
set.seed(2024) # for reproducibility of random results
index <- sample(1:nrow(data), round(0.7*nrow(data)))
train_data <- scale_data[index, ]
test_data <- scale_data[-index, ]
train_target <- data$Target[index]
test_target <- data$Target[-index]

# Fit the KNN model
knn_pred <- knn(train_data, test_data, train_target, k = 8)

# Calculate the accuracy of the KNN model
accuracy <- sum(test_target == knn_pred) / length(knn_pred)
cat("KNN Model Accuracy: ", accuracy * 100, "%", "\n")

#RANDOM FOREST --------------------------------------------------------------------------------------- 
library(readr)
library(dplyr)
library(lubridate)
library(randomForest)

# Read the data
data <- read_csv("1. Walmart.csv")

# Convert Date to a date object and extract the month
data$Date <- as.Date(data$Date, format="%d-%m-%Y")
data$Month <- month(data$Date)

# Assuming that we have a binary target variable 'Target' (need to be created or defined)
# For example:
data$Target <- as.factor(ifelse(data$Weekly_Sales > median(data$Weekly_Sales), 1 , 0))

# Split data into training and testing sets (Here we assume a 70/30 split)
set.seed(123) # for reproducibility of random results
index <- sample(1:nrow(data), round(0.7*nrow(data)))
train_data <- data[index, ]
test_data <- data[-index, ]

# Fit the Random Forest model
rf_model <- randomForest(Target ~ Store + CPI + Unemployment, 
                         data = train_data, 
                         ntree = 500, 
                         mtry = 3)

# Predict using the Random Forest model
rf_pred <- predict(rf_model, test_data)

# Calculate the accuracy of the Random Forest model
accuracy <- sum(test_data$Target == rf_pred) / nrow(test_data)
cat("Random Forest Model Accuracy: ", accuracy * 100, "%", "\n")

# Get variable importance and print the top 3 variables
importance <- importance(rf_model)
importance_sorted <- sort(importance, decreasing = TRUE)
print(importance_sorted)


### UNSUPERVISED LEARNING------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------
# DATA WRANGLING  --------------------------------------------------------------------------
library(readr)
data2 <- read.csv("2. Online Retail.csv")
str(data2)

#Missing Value-----------------------------------------------------------------------------------------
missing_count <- sum (is.na(data2))
total_missing1 <- colSums(is.na(data2))
print(total_missing1)

#Alternative to see duplicate Value----------------------------------------------------------------------
dup_indices <- which(duplicated(data2))
dup_data <- data2[duplicated(data2) | duplicated(data2, fromLast = TRUE), ]
print (dup_data)
# Remove rows with missing values from the entire data frame
new_data2 <- na.omit(data2)


# Make new data set for aggregation Load necessary libraries
library(dplyr)

# Count the transactions by country
transaction_counts <- total_missing1 %>%
  count(Country, name = "TransactionCount") %>%
  rename(`Invoice Date` = Country)

# Save the aggregated data to a CSV
aggregated_data_path <- 'AggregatedTransactionCounts.csv'
write_csv(transaction_counts, aggregated_data_path)

# Display the top of the aggregated data
head(transaction_counts)

# Transaction Count by Country Bar Plot---------------------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(plotly)
# Read the aggregated data
transaction_data <- read.csv('2. AggregatedTransactionCounts.csv')

# Create the bar plot
p <- ggplot(transaction_data, aes(x=reorder(Country, -TransactionCount), y=TransactionCount)) +
  geom_bar(stat='identity', fill='steelblue') +
  theme_minimal() +
  labs(x='', y='Transaction Count', title='Transaction Count by Country') +
  scale_y_continuous(breaks = c(50000, 100000, 150000,  200000, 250000, 300000, 350000, 400000),
                     labels = c("50K", "100K", "150K","200K", "250K", "300K","350K", "400K")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 40, vjust = 0.7) 
  )
ggplotly(p)
ggplot(transaction_data)

#Check transaction data
transaction_data

# Read the aggregated transaction counts data
transaction_counts <- read.csv('2. AggregatedTransactionCounts.csv')

# Calculate the total number of transactions
total_transactions <- sum(transaction_counts$TransactionCount)

# Calculate the proportion of each country's transactions in percentage
transaction_counts$Percentage <- (transaction_counts$TransactionCount / total_transactions) * 100

# You can round the percentages to a certain number of decimal places if you want
transaction_counts$Percentage <- round(transaction_counts$Percentage, 2)

# Display the data frame with the percentage column
print(transaction_counts)


#Make New Dataset Revenue by Date------------------------------------------------------------------
library(readr)
library(dplyr)
library(lubridate)

# Load the dataset
file_path <- '2. Online Retail.csv'
online_retail <- read_csv(file_path)

# Drop rows with missing values
online_retail <- drop_na(online_retail)

# Drop rows with negative quantity and negative UnitPrice
online_retail <- online_retail %>% 
  filter(Quantity > 0, UnitPrice > 0)

# Calculate revenue
online_retail <- online_retail %>%
  mutate(Revenue = Quantity * UnitPrice)

# Convert 'InvoiceDate' to datetime format
online_retail$InvoiceDate <- dmy_hm(online_retail$InvoiceDate)

# Aggregate revenue by invoice date
online_retail <- online_retail %>%
  group_by(InvoiceDate) %>%
  summarise(Revenue = sum(Revenue)) %>%
  ungroup()

# Write the aggregated data to a new CSV file
output_file_path <- 'Revenue by Date.csv'
write_csv(online_retail, output_file_path)


#TOTAL REVENUE BY WEEK - LINE PLOT ------------------------------------------------
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(readr)
# Load the dataset
retail_data <- read_csv("Revenue by Date.csv")

# Convert the InvoiceDate to a date object and calculate the revenue
retail_data$InvoiceDate <- as.Date(retail_data$InvoiceDate, format="%d/%m/%Y %H:%M")

# Add a WeekYear column to group by week
retail_data$Week <- floor_date(retail_data$InvoiceDate, unit="day")

# Aggregate revenue by week
weekly_revenue <- retail_data %>%
  group_by(Week) %>%
  summarise(TotalRevenue = sum(Revenue))

# Plot the data
p <- ggplot(weekly_revenue, aes(x=Week, y=TotalRevenue)) +
  geom_line(color= 'lightblue') +
  labs(title='Revenue by Date', x='Date', y='Total Sales') +
  scale_y_continuous(breaks = c(50000, 100000, 150000,  200000, 250000, 300000, 350000, 400000),
                     labels = c("50K", "100K", "150K","200K", "250K", "300K","350K", "400K")) # label each y axis element+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()
ggplotly(p)

# ALternative 1--------------------------------------------
# library(dplyr)
# library(readr)
# library(lubridate)
# 
# # Load the dataset
# revenue_by_date <- read_csv("Revenue by Date.csv")
# 
# # Ensure 'InvoiceDate' is in Date format
# revenue_by_date$InvoiceDate <- as.Date(revenue_by_date$InvoiceDate, "%Y-%m-%d")
# 
# # Add week, day, month, and weekday columns
# revenue_by_date$WeekOfYear <- isoweek(revenue_by_date$InvoiceDate)
# revenue_by_date$Day <- as.Date(revenue_by_date$InvoiceDate)
# revenue_by_date$Month <- month(revenue_by_date$InvoiceDate)
# revenue_by_date$Year <- year(revenue_by_date$InvoiceDate)
# revenue_by_date$Weekday <- wday(revenue_by_date$InvoiceDate, label = TRUE)
# 
# # Grouping by week and day to sum up revenue
# weekly_revenue <- revenue_by_date %>%
#   group_by(WeekOfYear) %>%
#   summarise(TotalRevenue = sum(Revenue))
# 
# daily_revenue <- revenue_by_date %>%
#   group_by(Day) %>%
#   summarise(TotalRevenue = sum(Revenue))
# 
# # Grouping by weekday to sum up revenue
# weekday_revenue <- revenue_by_date %>%
#   group_by(Weekday) %>%
#   summarise(TotalRevenue = sum(Revenue))
# 
# # Identifying the weekday with the highest and lowest total sales
# highest_sales_weekday <- weekday_revenue[which.max(weekday_revenue$TotalRevenue), ]
# lowest_sales_weekday <- weekday_revenue[which.min(weekday_revenue$TotalRevenue), ]
# 
# # Additionally, group by month to sum up revenue
# monthly_revenue <- revenue_by_date %>%
#   group_by(Year, Month) %>%
#   summarise(TotalRevenue = sum(Revenue))
# 
# # Displaying the results
# cat("Weekday with the Highest Sales:")
# print(highest_sales_weekday)
# cat("Weekday with the Lowest Sales:")
# print(lowest_sales_weekday)
# 
# cat("Total Revenue for Each Month:")
# print(monthly_revenue)


# Alternative 2-----------------------------------------------------------------------------------------------------------

# library(ggplot2)
# library(plotly)
# library(dplyr)
# library(lubridate)
# library(readr) # Assuming you're using read_csv from the readr package
# 
# # Load the dataset
# retail_data <- read_csv("2. Revenue by Date.csv")
# 
# # Convert the InvoiceDate to a date object
# retail_data$InvoiceDate <- as.Date(retail_data$InvoiceDate, format="%d/%m/%Y %H:%M")
# 
# # Add a MonthYear column to group by month
# retail_data$MonthYear <- floor_date(retail_data$InvoiceDate, unit="month")
# 
# # Aggregate revenue to find max, min, and total revenue by month
# monthly_revenue_stats <- retail_data %>%
#   group_by(MonthYear) %>%
#   summarise(MaxRevenue = max(Revenue),
#             MinRevenue = min(Revenue),
#             TotalRevenue = sum(Revenue))
# 
# # Plot the data
# p <- ggplot(monthly_revenue_stats, aes(x=MonthYear)) +
#   geom_line(aes(y=TotalRevenue), color='lightblue') +
#   geom_point(aes(y=MaxRevenue), color='red') +
#   geom_point(aes(y=MinRevenue), color='green') +
#   labs(title='Monthly Revenue Stats', x='Date', y='Revenue') +
#   scale_y_continuous(labels = scales::comma) + # Use comma for better readability
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_minimal()
# ggplotly(p)

# Alternative 3 --------------------------------------------------------------
# # Required libraries
# library(ggplot2)
# library(dplyr)
# library(lubridate)
# library(readr) # Assuming read_csv is from the readr package
# 
# # Load the dataset
# retail_data <- read_csv("2. Revenue by Date.csv")
# 
# # Prepare the data
# retail_data$InvoiceDate <- as.Date(retail_data$InvoiceDate, format="%d/%m/%Y %H:%M")
# retail_data$Week <- floor_date(retail_data$InvoiceDate, unit="week")
# 
# # Aggregate revenue by week
# weekly_revenue <- retail_data %>%
#   group_by(Week) %>%
#   summarise(TotalRevenue = sum(Revenue))
# 
# # Plot the data
# p <- ggplot(weekly_revenue, aes(x = Week, y = TotalRevenue)) +
#   geom_line(color = 'lightblue') +
#   labs(title = 'Weekly Total Sales', x = 'Date', y = 'Total Sales') +
#   scale_y_continuous(labels = scales::label_number()) +
#   theme_minimal()
# 
# # Display the plot
# print(p)

# 3 BOX PLOTS FREQUENCY RECENCY MONETARY-------------------------------------------------------------------
# Make New Data set-----------------------------------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(readr)

# Load the dataset
file_path <- '2. Online Retail.csv'
data <- read_csv(file_path)

# Drop rows with missing values, particularly in 'CustomerID' and 'Quantity'
data <- data %>%
  filter(!is.na(CustomerID) & !is.na(Quantity))

# Drop rows with negative quantity
data <- data %>%
  filter(Quantity > 0)

# Convert 'InvoiceDate' to datetime format and calculate 'Revenue'
data$InvoiceDate <- dmy_hm(data$InvoiceDate)
data$Revenue <- data$Quantity * data$UnitPrice

# Aggregate revenue by InvoiceDate
online_retail_aggregated <- data %>%
  group_by(InvoiceDate) %>%
  summarise(Revenue = sum(Revenue)) %>%
  ungroup()

# Write the aggregated data to a new CSV file
output_aggregated_file_path <- 'Revenue_by_Date.csv'
write_csv(online_retail_aggregated, output_aggregated_file_path)

# Find the most recent purchase date for RFM calculation
snapshot_date <- as.Date('2011-12-09')

# Calculate RFM metrics
rfm <- data %>%
  group_by(CustomerID) %>%
  summarise(Recency = as.numeric(snapshot_date - max(InvoiceDate)),
            Frequency = n_distinct(InvoiceNo),
            Monetary = sum(Revenue)) %>%
  filter(Monetary > 0)

# Save the RFM metrics to another CSV file
output_rfm_file_path <- 'rfm_data.csv'
write_csv(rfm, output_rfm_file_path)

# RFM BOXPLOT ----------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(patchwork) # for arranging plots

# Load the RFM dataset
rfm_data <- read_csv("rfm_data.csv")

# Create boxplot for Recency with customized x-axis limits
p_recency <- ggplot(rfm_data, aes(x = factor(1), y = Recency)) +
  geom_boxplot(fill = "steelblue") +
  coord_cartesian(ylim = c(0, 300)) +
  labs(title = "Recency", y = "Recency", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Create boxplot for Frequency with customized x-axis limits
p_frequency <- ggplot(rfm_data, aes(x = factor(1), y = Frequency)) +
  geom_boxplot(fill = "orange") +
  coord_cartesian(ylim = c(0, 250)) +
  labs(title = "Frequency", y = "Frequency", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Create boxplot for Monetary with customized x-axis limits
p_monetary <- ggplot(rfm_data, aes(x = factor(1), y = Monetary)) +
  geom_boxplot(fill = "green") +
  labs(title = "Monetary", y = "Monetary", x = "") +
  coord_cartesian(ylim = c(0, 5000)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Combine the plots into one figure with a common legend
p_combined <- p_recency + p_frequency + p_monetary + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'top', legend.justification="right")

# Print the combined plot
print(p_combined)

# Calculate and print the mean for each RFM category
mean_recency <- mean(rfm_data$Recency, na.rm = TRUE)
mean_frequency <- mean(rfm_data$Frequency, na.rm = TRUE)
mean_monetary <- mean(rfm_data$Monetary, na.rm = TRUE)

print(paste("Mean Recency:", mean_recency))
print(paste("Mean Frequency:", mean_frequency))
print(paste("Mean Monetary:", mean_monetary))


# SILHOUETTE, ELBOW METHOD, K-MEANS-------------------------------------------------------------------------------
library(ggplot2)
library(factoextra)
library(dplyr)
library(readr)
library(ggfortify)


# Read the RFM data
rfm_data <- read_csv("rfm_data.csv")

# Normalize the RFM data
rfm_scaled <- scale(rfm_data[, c('Recency', 'Frequency', 'Monetary')])

# Perform PCA
pca_res <- prcomp(rfm_scaled, scale. = TRUE)

# Find the explained variance by the first two principal components
pca_summary <- summary(pca_res)
print(pca_summary$importance[2, 1:2])

# Perform K-Means Clustering on PCA results
set.seed(2024) # For reproducibility
kmeans_result <- kmeans(pca_res$x[, 1:2], centers = 3)

# Determine optimal number of clusters using the elbow and silhouette methods
# These are exploratory and would typically be done before final clustering
fviz_nbclust(rfm_scaled, kmeans, method = "wss") # Elbow method
fviz_nbclust(rfm_scaled, kmeans, method = "silhouette") # Silhouette method

# Create a data frame for ggplot
cluster_data <- data.frame(pca_res$x[, 1:2], cluster = factor(kmeans_result$cluster))

p <- autoplot(pca_res, data = rfm_data, label = FALSE, frame = TRUE, 
                         frame.type = 'norm', scale = 0, pc.biplot = FALSE) +
  geom_point(aes(color = as.factor(kmeans_result$cluster)), size = 4, alpha = 0.6) +
  scale_color_manual(values = c("red", "green", "blue")) +
  labs(title = 'K-Means Clustering', color = 'Cluster') +
  theme_minimal()

# Calculate the convex hull for each cluster
hulls <- data.frame(x = numeric(0), y = numeric(0), cluster = factor())
for (i in levels(cluster_data$cluster)) {
  cluster_points <- cluster_data[cluster_data$cluster == i, ]
  hull_points <- cluster_points[chull(cluster_points$PC1, cluster_points$PC2), ]
  hulls <- rbind(hulls, data.frame(x = hull_points$PC1, y = hull_points$PC2, cluster = i))
}
p <- p + geom_polygon(data = hulls, aes(x = x, y = y, fill = cluster), alpha = 0.2)

# Add cluster assignments back to the data
rfm_data$Cluster <- kmeans_result$cluster

# Summarize the clusters
cluster_summary <- rfm_data %>%
  group_by(Cluster) %>%
  summarise(
    Size = n(),
    Average_Recency = mean(Recency),
    Average_Frequency = mean(Frequency),
    Average_Monetary = mean(Monetary)
  )

# Print the PCA clustering plot and summary table
print(p)
print(cluster_summary)


# # Alternative1-------------------------------------------------------------------
# 
# library(ggplot2)
# library(factoextra)
# library(dplyr)
# library(readr)
# install.packages("ggfortify")
# library(ggfortify)
# 
# 
# # Read the RFM data
# rfm_data <- read_csv("rfm_data.csv")
# 
# # Normalize the RFM data
# rfm_scaled <- scale(rfm_data[, c('Recency', 'Frequency', 'Monetary')])
# 
# # Perform PCA
# pca_res <- prcomp(rfm_scaled, scale. = TRUE)
# 
# # Find the explained variance by the first two principal components
# pca_summary <- summary(pca_res)
# print(pca_summary$importance[2, 1:2])
# 
# # Perform K-Means Clustering on PCA results
# set.seed(2024) # For reproducibility
# kmeans_result <- kmeans(pca_res$x[, 1:2], centers = 3)
# 
# # Create a data frame for ggplot
# cluster_data <- data.frame(pca_res$x[, 1:2], cluster = factor(kmeans_result$cluster))
# 
# pca_plot <- ggplot(cluster_data, aes(x = PC1, y = PC2, color = cluster)) +
#   geom_point(size = 4, alpha = 0.6) +
#   scale_color_manual(values = c("red", "green", "blue")) +
#   labs(title = 'K-Means Clustering on PCA Results', color = 'Cluster') +
#   theme_minimal()
# 
# # Plot the convex hull for each cluster
# # Assuming 'hulls' calculation is correct and not causing the error.
# pca_plot <- pca_plot + geom_polygon(data = hulls, aes(x = x, y = y, fill = cluster), alpha = 0.2)
# 
# # Since 'p' is not defined in your provided code, I'm assuming 'pca_plot' is what you intended to print.
# print(pca_plot)
# 
# # Ensure 'rfm_data' and 'cluster_summary' steps are correct, as previously outlined.
# # Summarize the clusters (assuming this is correct as provided).
# print(cluster_summary)


# HIERARCHICAL CLUSTERING -----------------------------------------------------------------------------------------
library(dendextend)
library(dplyr)

# Load your data
rfm_data <- read.csv('rfm_data.csv')

# Normalize the data
rfm_scaled <- scale(rfm_data[, c('Recency', 'Frequency', 'Monetary')])

# Compute the hierarchical clustering with Manhattan distance and complete linkage
hc <- hclust(dist(rfm_scaled, method = "euclidian"), method = "ward.D")

# Simplify the dendrogram by cutting at a height to get four clusters and then plot
dend <- as.dendrogram(hc)
dend <- color_branches(dend, k = 4)
dend <- hang.dendrogram(dend, hang = -1)

# Set the labels to none for a cleaner plot, similar to the one in the image provided
labels(dend) <- NULL

# Plot the simplified dendrogam
plot(dend, main = "Dendrogram using Manhattan Distance & Complete Linkage, k = 4")

# Cut the dendrogram into 4 clusters
clusters <- cutree(hc, k = 4)

# Add the cluster assignments back to the original rfm_data
rfm_data$Cluster <- clusters

# Create summary table for each cluster
cluster_summary <- rfm_data %>%
  group_by(Cluster) %>%
  summarise(
    Size = n(),
    Average_Recency = mean(Recency),
    Average_Frequency = mean(Frequency),
    Average_Monetary = mean(Monetary)
  ) %>%
  arrange(desc(Average_Monetary)) # Assuming you want to sort by Monetary value as in the image

# Print the summary table
print(cluster_summary)

# TABLE CONCLUSION-----------------------------------------------------------------
# REGRESSION TABLE-----------------------------------------------
library(readr)
library(caret)
library(randomForest)
library(rpart)
library(Metrics)
library(glmnet)

# Load the dataset
data <- read_csv("1. Walmart.csv")

# Assuming 'Weekly_Sales' is the target variable and 'Date' is not a predictor
# Remove non-numeric and Date columns
numeric_columns <- sapply(data, is.numeric)
data <- data[, numeric_columns]
target <- data$Weekly_Sales
predictors <- data[, !names(data) %in% c('Weekly_Sales', 'Date')]

# Split the data into training and testing sets
set.seed(2024)  # Set a seed for reproducibility
train_index <- createDataPartition(target, p = 0.7, list = FALSE)
train_predictors <- predictors[train_index, , drop = FALSE]
train_target <- target[train_index]
test_predictors <- predictors[-train_index, , drop = FALSE]
test_target <- target[-train_index]

# Scale the features
preProcValues <- preProcess(train_predictors, method = c("center", "scale"))
train_predictors_scaled <- predict(preProcValues, train_predictors)
test_predictors_scaled <- predict(preProcValues, test_predictors)

# Fit the models and predict
# Initialize models
models <- list(
  lm = lm(Weekly_Sales ~ Store + Unemployment + CPI , data = data[train_index, ]),
  rf = randomForest(Weekly_Sales ~ Store + Unemployment + CPI, data = data[train_index, ], ntree = 500)
)

# Add glmnet models separately due to their different prediction method
set.seed(2024)
ridge_model <- cv.glmnet(as.matrix(train_predictors_scaled), train_target, alpha = 0)
lasso_model <- cv.glmnet(as.matrix(train_predictors_scaled), train_target, alpha = 1)

# Predict and calculate RMSE for lm and rf
results <- list()
for (model_name in names(models)) {
  model <- models[[model_name]]
  predictions <- predict(model, newdata = data[-train_index, ])
  rmse <- rmse(test_target, predictions)
  results[[model_name]] <- c(RMSE = rmse, NormalizedRMSE = rmse / (max(target) - min(target)))
}

# Calculate RMSE for ridge and lasso
ridge_predictions <- predict(ridge_model, newx = as.matrix(test_predictors_scaled), s = "lambda.min")
results$ridge <- c(RMSE = rmse(test_target, ridge_predictions),
                   NormalizedRMSE = rmse(test_target, ridge_predictions) / (max(target) - min(target)))

lasso_predictions <- predict(lasso_model, newx = as.matrix(test_predictors_scaled), s = "lambda.min")
results$lasso <- c(RMSE = rmse(test_target, lasso_predictions),
                   NormalizedRMSE = rmse(test_target, lasso_predictions) / (max(target) - min(target)))

# Fit Decision Tree separately (doesn't require scaling)
dt_model <- rpart(Weekly_Sales ~ ., data = data[train_index, ])
dt_predictions <- predict(dt_model, newdata = data[-train_index, ])
results$decision_tree <- c(RMSE = rmse(test_target, dt_predictions),
                           NormalizedRMSE = rmse(test_target, dt_predictions) / (max(target) - min(target)))

# Convert results to a data frame
results_df <- as.data.frame(do.call(rbind, results))
results_df$Model <- rownames(results_df)
rownames(results_df) <- NULL
results_df <- results_df[, c("Model", "RMSE", "NormalizedRMSE")]

# Print results
print(results_df)



# FINAL CLASSIFICATION TABLE--------------------------------------------------------------
library(readr)
library(caret)
library(e1071) # For SVM
library(randomForest) # For Random Forest
library(klaR) # For KNN
library(glmnet)
library(rpart) # For Decision Tree

# Load the data
data <- read_csv("1. Walmart.csv")

# Creating a binary classification target
data$Sales_Class <- ifelse(data$Weekly_Sales > median(data$Weekly_Sales), 'High', 'Low')
data$Sales_Class <- as.factor(data$Sales_Class)

# Splitting the data set into training and testing sets
set.seed(2024) # for reproducibility
index <- createDataPartition(data$Sales_Class, p = 0.7, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

# Fit classification models
# Logistic Regression Model
logistic_model <- glm(Sales_Class ~ Store + CPI + Unemployment , data = train_data, family = binomial)

# SVM Model
svm_model <- svm(Sales_Class ~ Store + CPI + Unemployment, data = train_data, kernel = "radial")

# Random Forest Model
rf_model <- randomForest(Sales_Class ~ Store + CPI + Unemployment , data = train_data, ntree = 500)

# KNN Model
knn_model <- train(Sales_Class ~ Store + CPI + Unemployment , data = train_data, method = "knn", tuneLength = 10)

# Decision Tree Model
dt_model <- rpart(Sales_Class ~ Store + CPI + Unemployment, data = train_data, method = "class")

# Making predictions and computing confusion matrix for each model
pred_logistic <- predict(logistic_model, newdata = test_data, type = "response")
cm_logistic <- confusionMatrix(as.factor(ifelse(pred_logistic > 0.5, 'High', 'Low')), test_data$Sales_Class)

pred_svm <- predict(svm_model, newdata = test_data)
cm_svm <- confusionMatrix(pred_svm, test_data$Sales_Class)

pred_rf <- predict(rf_model, newdata = test_data)
cm_rf <- confusionMatrix(pred_rf, test_data$Sales_Class)

pred_knn <- predict(knn_model, newdata = test_data)
cm_knn <- confusionMatrix(pred_knn, test_data$Sales_Class)

pred_dt <- predict(dt_model, newdata = test_data, type = "class")
cm_dt <- confusionMatrix(pred_dt, test_data$Sales_Class)

# Collecting the metrics from confusion matrices
results <- data.frame(
  Model = c("Logistic Regression", "SVM", "Random Forest", "KNN", "Decision Tree"),
  Accuracy = c(cm_logistic$overall['Accuracy'], cm_svm$overall['Accuracy'], cm_rf$overall['Accuracy'], cm_knn$overall['Accuracy'], cm_dt$overall['Accuracy']),
  Sensitivity = c(cm_logistic$byClass['Sensitivity'], cm_svm$byClass['Sensitivity'], cm_rf$byClass['Sensitivity'], cm_knn$byClass['Sensitivity'], cm_dt$byClass['Sensitivity']),
  Specificity = c(cm_logistic$byClass['Specificity'], cm_svm$byClass['Specificity'], cm_rf$byClass['Specificity'], cm_knn$byClass['Specificity'], cm_dt$byClass['Specificity']),
  Precision = c(cm_logistic$byClass['Precision'], cm_svm$byClass['Precision'], cm_rf$byClass['Precision'], cm_knn$byClass['Precision'], cm_dt$byClass['Precision'])
)



