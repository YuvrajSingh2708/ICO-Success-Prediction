rm(list=ls())


# Install and load the necessary packages

library(readr)

library(ggplot2)
library(dplyr)
library(cowplot)
library(zoo)
install.packages("corrplot")
library(corrplot)
library(reshape2)
library(corrgram)

install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
install.packages("VIM",dependencies = T)
library("VIM")

library(caret)
library(pROC)

library(randomForest)
library(stringr)



raw_data <- read_csv("LUBS5990M_courseworkData_2324.csv")

raw_data1 <- raw_data
# check the first six 
head(raw_data)
# View the structure of the dataset
str(raw_data)

# Summary statistics
summary(raw_data)



#Dimensions
dim(raw_data)
#the dataset has 2,767 rows and 15 columns
#data on 2,767 ICOs with 15 features




numerical_data <- raw_data[sapply(raw_data, is.numeric)]
# Categorical variables
categorical_vars <- c("success", "brandSlogan", "countryRegion", "startDate", "endDate", "platform", "hasGithub", "hasReddit")
# Numerical variables
numerical_vars <- c( "hasVideo", "rating", "priceUSD", "teamSize", "coinNum", "minInvestment", "distributedPercentage")
# Subset the dataset based on the defined classes
categorical_data <- raw_data[categorical_vars]
numerical_data <- raw_data[numerical_vars]

# Check the structure of the categorical and numerical data
str(categorical_data)
str(numerical_data)



# Calculate the number of missing values for each variable
missing_counts <- colSums(is.na(raw_data))

# Calculate the percentage of missing values for each variable
missing_percentage <- missing_counts / nrow(raw_data) * 100

# Create a dataframe with variable names, missing value counts, and percentages
missing_data <- data.frame(variable = names(missing_counts), 
                           missing_count = missing_counts,
                           missing_percentage = missing_percentage)

# Print the table
print(missing_data)

# Filter the dataframe to include only variables with missing values
missing_variables <- missing_data %>% 
  filter(missing_counts > 0)

# Print the table
print(missing_variables)

sum(complete.cases(raw_data))
sum(!complete.cases(raw_data))
aggr(raw_data,numbers = T, prop = F,cex.lab = 1.5,cex.axis=1.1,combined=T,cex.numbers=1.1)





# Define a vector of colors for each histogram
hist_colors <- c("skyblue", "lightgreen", "orange", "pink", "lightgray", "salmon", "lightblue", "tan")

# Plotting all numerical variables in one graph using facets
num_plots <- lapply(seq_along(names(numerical_data)), function(i) {
  ggplot(raw_data, aes_string(x = names(numerical_data)[i])) +
    geom_histogram(fill = hist_colors[i], color = "black", bins = 40) +
    labs(title = paste("Dist. of", names(numerical_data)[i])) +
    theme(plot.title = element_text(hjust = 0.5))  # Center plot titles
})
# Combine and print the plots
multiplot <- plot_grid(plotlist = num_plots, ncol = 3)
print(multiplot)







#Drop the ID column
raw_data <- raw_data[,-1]


####### Defining the Target Variable---------------------------------------------------------------
#Success field summary
frequency_table <- table(raw_data$success)
# Calculate the percentages
percentage_table <- prop.table(frequency_table) * 100
# Combine the frequency and percentage tables
summary_table <- cbind(frequency_table, percentage_table)
print(summary_table)
#1739 ICOs did not reach the funding target
#1028 ICOs reached their funding target

#transofrming it to a factor
raw_data$success <- as.factor(raw_data$success)
str(raw_data$success)



# Replace missing values in priceUSD and teamSize with the mean-------------------------------------
raw_data$priceUSD <- na.aggregate(raw_data$priceUSD, FUN = median)
raw_data$teamSize <- na.aggregate(raw_data$teamSize, FUN = median)
raw_data$countryRegion <- ifelse(is.na(raw_data$countryRegion) | raw_data$countryRegion == "", "unknown", raw_data$countryRegion)
table(raw_data$countryRegion)
# Impute missing values in platform with the most common value
raw_data$platform[is.na(raw_data$platform)] <- "Ethereum"






# INT - HAS VIDEO COLUMN ----------------------------------------------------------------------
#Summary of the has video column
table(raw_data$hasVideo)
#758 did not share a video on their campaign page
#2009 shared a video on their campaign page


# INT - HAS GITHUB COLUMN ---------------------------------------------------------------------
#Summary of the has github column
table(raw_data$hasGithub)
#1168 did not share their github page on the campaign page
#1599 shared their github page on the campaign page


# INT - HAS REDDIT COLUMN ---------------------------------------------------------------------
#Summary of the has reddit column
table(raw_data$hasReddit)
#1016 did not share their reddit page on the campaign page
#1751 shared their reddit page on the campaign page



# INT - MIN INVESTMENT COLUMN -----------------------------------------------------------------
#Summary of the minimum investment column
table(raw_data$minInvestment)
#1513 did not set a minimum investment
#1254 set a minimum investment


# CHR - COUNTRY/REGION COLUMN -----------------------------------------------------------------
#Summary of the country/region column
table(raw_data$countryRegion)

#Viewing the unique values
unique(raw_data$countryRegion)

#converting the country region values to upper case for error correction
raw_data$countryRegion <- toupper(raw_data$countryRegion)

#viewing the unique values again
unique(raw_data$countryRegion)

#summary of the column
table(raw_data$countryRegion)
#replacing the empty values with an NA
raw_data$countryRegion <- ifelse(is.na(raw_data$countryRegion) | raw_data$countryRegion == "", "unknown", raw_data$countryRegion)
table(raw_data$countryRegion)
#correcting the errors in Mexico value
raw_data$countryRegion <- str_replace(raw_data$countryRegion,"MÉXICO","MEXICO")

#correcting the error in curacao
raw_data$countryRegion <- str_replace(raw_data$countryRegion,"CURAÇAO","CURACAO")

#correcting error in south africa
raw_data$countryRegion <- str_replace(raw_data$countryRegion,"SOUTHAFRICA","SOUTH AFRICA")

#sorting the column of unique values
raw_data$countryRegion <- toupper(raw_data$countryRegion)
unique_country_list <- as.data.frame(sort(unique(raw_data$countryRegion)))
unique_country_list

# CHR - START/END DATE COLUMNS ----------------------------------------------------------------
# Summary of the start/end date columns

# Converting the start date to a date
raw_data$startDate <- as.Date(raw_data$startDate, "%d/%m/%Y")
str(raw_data$startDate)
sum(is.na(raw_data$startDate))
min(raw_data$startDate)
max(raw_data$startDate)

# Converting the end date to a date
raw_data$endDate <- as.Date(raw_data$endDate, "%d/%m/%Y")
str(raw_data$endDate)
sum(is.na(raw_data$endDate))
min(raw_data$endDate)
max(raw_data$endDate)

# Finding the duration of the ICOs in days
# Adding the duration column to the dataset
head(raw_data)
indices <- which(raw_data$startDate > raw_data$endDate)
starts <- raw_data[indices, ]$startDate
ends <- raw_data[indices, ]$endDate
raw_data[indices, ]$startDate <- ends
raw_data[indices, ]$endDate <- starts
duration <- raw_data$endDate - raw_data$startDate
raw_data <- raw_data %>% mutate(duration)
raw_data$duration <- as.numeric(raw_data$duration)
summary(raw_data$duration)

# Plot histogram of duration before processing
hist(raw_data$duration, breaks = 20, main = "Histogram of ICO Duration (Before Processing)",
     xlab = "Duration (days)", ylab = "Frequency", col = "skyblue")

# Filter out extreme duration values
raw_data <- raw_data %>% filter(duration < 500)

# Plot histogram of duration after processing
hist(raw_data$duration, breaks = 20, main = "Histogram of ICO Duration (After Processing)",
     xlab = "Duration (days)", ylab = "Frequency", col = "skyblue")
dim(raw_data)
summary(raw_data$duration)

# Extract the year, month, and day values from the date columns
raw_data$startYear <- as.numeric(format(raw_data$startDate, '%Y'))
raw_data$endYear <- as.numeric(format(raw_data$endDate, '%Y'))
raw_data$startMonth <- as.numeric(format(raw_data$startDate, '%m'))
raw_data$endMonth <- as.numeric(format(raw_data$endDate, '%m'))
raw_data$startDay <- as.numeric(format(raw_data$startDate, '%d'))
raw_data$endDay <- as.numeric(format(raw_data$endDate, '%d'))

# CHR - PLATFORM COLUMN -----------------------------------------------------------------------
#Summary of the platform column
table(raw_data$platform)
length(unique(raw_data$platform))
unique_platform_list <- as.data.frame(sort(unique(raw_data$platform)))
unique_platform_list
# 'Stellar' is the same as 'Stellar Protocol'
#Separate blockchain the same as Separate Blockchain
#X11 blockchain the same as X11
#Ethereum, Waves??
#TRON same as Tron
#There is a "" value
#WAVES same as Waves
#Ethereum instead of Ether
#ERC20 under Ethereum platform


raw_data$platform <- str_trim(raw_data$platform)
raw_data$platform <- toupper(raw_data$platform)
unique(raw_data$platform)
raw_data$platform <- str_replace(raw_data$platform,"ETHERERUM","ETHEREUM")
raw_data$platform <- str_replace(raw_data$platform,"X11 BLOCKCHAIN","X11")
unique(raw_data$platform)
raw_data$platform <- str_replace(raw_data$platform,"ERC20","ETHEREUM")
raw_data$platform <- str_replace(raw_data$platform,"STELLAR PROTOCOL","STELLAR")
raw_data$platform <- str_replace(raw_data$platform,"ETHERUM","ETHEREUM")
raw_data$platform <- str_replace(raw_data$platform,"CRYPTONOTE-BASED BLOCKCHAIN","CRYPTONOTE")
raw_data$platform <- str_replace(raw_data$platform,"BTC","BITCOIN")
raw_data$platform <- str_replace(raw_data$platform,"ISL-","ISL")
raw_data$platform <- str_replace(raw_data$platform,"POS,POW","POW-POS")
raw_data$platform <- gsub("BLOCKCHAIN","",raw_data$platform)
raw_data$platform <- gsub("PLATFORM","",raw_data$platform)
raw_data$platform[raw_data$platform == "POS + POW"] <- "POW-POS"
raw_data$platform[raw_data$platform == "POW/POS"] <- "POW-POS"
sort(unique(raw_data$platform))
raw_data$platform <- str_trim(raw_data$platform)
raw_data$platform[raw_data$platform == ""] <- "UNKNOWN"
raw_data$platform[raw_data$platform == "ETHEREUM, WAVES"] <- "ETHEREUM-WAVES"
raw_data$platform[raw_data$platform == "ETH"] <- "ETHEREUM"

sort(unique(raw_data$platform))
length(unique(raw_data$platform))


# Summary of the priceUSD column--------------------------------------------------------------------------
summary(raw_data$priceUSD)
# Plot histogram before processing
hist(raw_data$priceUSD, breaks = 20, ylab = "Frequency", xlab = "Price (USD)", 
     main = "priceUSD (Before Processing)", col = "lightgreen")


# Calculate 1% and 99% quantiles of priceUSD
q1priceUSD <- quantile(raw_data$priceUSD, 0.08)
q99priceUSD <- quantile(raw_data$priceUSD, 0.97)

# Replace outliers with quantile values
raw_data$priceUSD[raw_data$priceUSD < q1priceUSD] <- q1priceUSD
raw_data$priceUSD[raw_data$priceUSD > q99priceUSD] <- q99priceUSD


num_zeros <- sum(raw_data$priceUSD >4)

# Print the count of zero values
print(num_zeros)

# Plot histogram after processing
hist(raw_data$priceUSD, breaks = 20, main = "Histogram of priceUSD (After Processing)",
     xlab = "Price (USD)", ylab = "Frequency", col = "lightgreen")

# Summary of priceUSD after processing
summary(raw_data$priceUSD)


# Calculate 1% and 99% quantiles of coinNum
q1 <- quantile(raw_data$coinNum, 0.01)
q99 <- quantile(raw_data$coinNum, 0.99)

# Replace outliers with quantile values
raw_data$coinNum[raw_data$coinNum < q1] <- q1
raw_data$coinNum[raw_data$coinNum > q99] <- q99

# Take the logarithm of the modified coinNum variable
raw_data$log_coinNum <- log(raw_data$coinNum)
# Plot the adjusted coinNum variable
ggplot(raw_data, aes(x = log_coinNum)) +
  geom_histogram(binwidth = 0.3, fill = "tan", color = "black") +
  labs(title = "Distribution of log(coinNum)") +
  theme_minimal()

# INT - TEAM SIZE COLUMN ----------------------------------------------------------------------
# Summary of the team size column
summary(raw_data$teamSize)

# Plot histogram before processing
hist(raw_data$teamSize, breaks = 30, ylab = "Frequency", xlab = "Team Size",
     main = "ICO Fundraising Team Sizes (Before Processing)", col = "tan")

# Remove outliers (assuming you've already identified and filtered outliers)
q199 <- quantile(raw_data$teamSize, 0.99)
raw_data <- dplyr::filter(raw_data, raw_data$teamSize < 50 | is.na(raw_data$teamSize))

# Plot histogram after processing
hist(raw_data$teamSize, breaks = 20, ylab = "Frequency", xlab = "Team Size",
     main = "ICO Fundraising Team Sizes (After Processing)", col = "lightgreen")

# Summary of teamSize after processing
summary(raw_data$teamSize)




# NUM - DISTRIBUTED PERCENTAGE COLUMN ---------------------------------------------------------
# Summary of the Distributed Percentage column
summary(raw_data$distributedPercentage)

# Plot histogram before processing
hist(raw_data$distributedPercentage, breaks = 20, 
     ylab = "Frequency", xlab = "Distributed Percentage",
     main = "Percentage of ICO Coins Distributed (Before Processing)", col = "lightblue")

# Filtering out values > 1 (outliers)
outliers <- sum(raw_data$distributedPercentage > 1)
print(paste("Number of outliers:", outliers))

raw_data <- raw_data %>% filter(distributedPercentage <= 1)

# Plot histogram after processing
hist(raw_data$distributedPercentage, breaks = 35, 
     ylab = "Frequency", xlab = "Distributed Percentage",
     main = "Percentage of ICO Coins Distributed (After Processing)", col = "seagreen")

# Summary of distributedPercentage after processing
summary(raw_data$distributedPercentage)




# RECAP OF THE DATA --------------------------------------------------------------------
dim(raw_data)
unique(raw_data$success)
unique(raw_data$hasVideo)
unique(raw_data$rating)
unique(raw_data$priceUSD)
unique(raw_data$countryRegion)
unique(raw_data$teamSize)
unique(raw_data$hasGithub)
unique(raw_data$hasReddit)
unique(raw_data$platform)
unique(raw_data$minInvestment)
unique(raw_data$distributedPercentage)


# CHECKING MISSING VALUES -----------------------------------------------------------------------
dim(raw_data)
sum(is.na(raw_data$priceUSD))
sum(is.na(raw_data$teamSize))
sum(is.na(raw_data$platform))
sort(unique(raw_data$platform))
sum(is.na(raw_data$platform))
sum(is.na(raw_data))
str(raw_data)
sum(complete.cases(raw_data))
sum(!complete.cases(raw_data))




###### Investigating the Relationships between Predictors------------------------------------------
# Compute correlation matrix for numerical variables
numerical_vars <- c("hasGithub", "hasReddit", "hasVideo", "rating", "teamSize", "minInvestment")
correlation_matrix <- cor(raw_data[, numerical_vars])
# Plot correlogram with correlation coefficients
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "orange")







# Filter data for countryRegion variable, remove rows with missing values, and calculate count
country_data <- raw_data %>%
  filter(!is.na(countryRegion)) %>%
  count(countryRegion) %>%
  arrange(desc(n)) %>%  # Sort data by count in decreasing order
  slice(1:15)  # Select only the top 15 countries

# Plot bar plot for top 15 countryRegion with different colors for each country
country_plot <- ggplot(country_data, aes(x = reorder(countryRegion, -n), y = n, fill = countryRegion)) +
  geom_bar(stat = "identity", color = "black") +  # Add black border to bars
  geom_text(aes(label = n), vjust = -0.3, size = 3, color = "black") +  # Add data labels above bars
  labs(title = "Top 15 Country/Region by Count", x = "Country/Region", y = "Count") +
  theme_minimal() +  # Use minimal theme for cleaner appearance
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # Rotate x-axis labels and adjust font size
        axis.title = element_text(size = 10),  # Adjust axis title font size
        plot.title = element_text(size = 12, face = "bold")) +  # Adjust plot title font size and weight
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5")) +  # Set custom colors
  guides(fill = "none")  # Remove legend

# Display the plot
print(country_plot)





























# Assuming your data frame is named 'data'
# Assuming 'platform' contains the platform names
# Assuming 'countryRegion' contains the country/region names

# Create a table of platform names and their frequencies
platform_freq <- table(raw_data$platform)

# Convert the table to a data frame
platform_df <- as.data.frame(platform_freq)

# Set column names
names(platform_df) <- c("Platform", "Frequency")

# Sort the data frame by frequency (optional)
platform_df <- platform_df[order(-platform_df$Frequency), ]

# Generate the word cloud for platform names
wordcloud_platform <- wordcloud(words = platform_df$Platform, freq = platform_df$Frequency, min.freq = 1,
                                max.words = 80, random.order = FALSE, colors = brewer.pal(8, "Dark2"),
                                scale = c(3, 0.5), rot.per = 0.3, random.color = TRUE,
                                family = "sans", bg = "white",
                                title.size = 1.5, title.color = "darkblue", title.bold = TRUE,
                                random.seed = 42)

# Add informative title for platform word cloud
title(main = "Word Cloud of Platform Names", col.main = "darkblue", font.main = 4)

# Create a table of country/region names and their frequencies
country_freq <- table(raw_data$countryRegion)

# Convert the table to a data frame
country_df <- as.data.frame(country_freq)

# Set column names
names(country_df) <- c("CountryRegion", "Frequency")

# Sort the data frame by frequency (optional)
country_df <- country_df[order(-country_df$Frequency), ]

# Generate the word cloud for country/region names
wordcloud_country <- wordcloud(words = country_df$CountryRegion, freq = country_df$Frequency, min.freq = 1,
                               max.words = 80, random.order = FALSE, colors = brewer.pal(8, "Dark2"),
                               scale = c(3, 0.5), rot.per = 0.3, random.color = TRUE,
                               family = "sans", bg = "white",
                               title.size = 1.5, title.color = "darkred", title.bold = TRUE,
                               random.seed = 42)

# Add informative title for country/region word cloud
title(main = "Word Cloud of Country/Region Names", col.main = "darkred", font.main = 4)

# Combine both word clouds into one plot
par(mfrow = c(1, 2))
wordcloud_platform
wordcloud_country











##############################-------------------------------------------------------------
#feature selection 
#Feature “startDate”, “endDate” and “brandSlogan” are deleted
data <- raw_data[, -c(2, 6, 7, 8, 12, 23)]
str(data)


# Load required packages
library(caret)
library(pROC)
library(ggplot2)
library(rpart)
library(randomForest)
library(class)
library(e1071)

# Set seed for reproducibility
set.seed(42)

# Create training and testing sets
train_size <- round(0.7 * nrow(data))
train_indices <- sample(seq_len(nrow(data)), size = train_size)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Ensure consistent factor levels for the target variable
train_data$success <- factor(train_data$success)
test_data$success <- factor(test_data$success, levels = levels(train_data$success))

# --- Decision Tree Model ---
decision_tree_model <- train(success ~ ., data = train_data, method = "rpart", trControl = trainControl(method = "cv", number = 10))
predictions_dt_prob <- predict(decision_tree_model, test_data, type = "prob")
positive_class_dt <- "Y"  # Update if necessary
predictions_dt <- as.numeric(predictions_dt_prob[, positive_class_dt])
conf_matrix_dt <- confusionMatrix(predict(decision_tree_model, test_data), test_data$success)

# --- Random Forest Model ---
rf_model <- train(success ~ ., data = train_data, method = "rf", trControl = trainControl(method = "cv", number = 10))
predictions_rf_prob <- predict(rf_model, test_data, type = "prob")
positive_class_rf <- "Y"  # Update if necessary
predictions_rf <- as.numeric(predictions_rf_prob[, positive_class_rf])
conf_matrix_rf <- confusionMatrix(predict(rf_model, test_data), test_data$success)

# --- KNN Model ---
# Preprocess to scale numerical features
preProc <- preProcess(train_data[, -which(names(train_data) == "success")], method = c("center", "scale"))
train_data_scaled <- predict(preProc, train_data)
test_data_scaled <- predict(preProc, test_data)

# Find the optimal k value for KNN
k_values <- seq(1, 30, by = 2)
train_and_evaluate_knn <- function(k, train_data, test_data) {
  model <- knn(train_data[, -which(names(train_data) == "success")], test_data[, -which(names(test_data) == "success")], train_data$success, k = k)
  error_rate <- mean(model != test_data$success)
  return(error_rate)
}
optimal_k <- k_values[which.min(sapply(k_values, function(k) train_and_evaluate_knn(k, train_data_scaled, test_data_scaled)))]
knn_model <- knn(train_data_scaled[, -which(names(train_data_scaled) == "success")], test_data_scaled[, -which(names(test_data_scaled) == "success")], train_data_scaled$success, k = optimal_k)
knn_predictions <- factor(knn_model, levels = levels(train_data$success))
conf_matrix_knn <- confusionMatrix(knn_predictions, test_data$success)

# --- Logistic Regression Model ---
logistic_model <- glm(success ~ ., data = train_data, family = binomial)
predictions_logistic <- predict(logistic_model, test_data, type = "response")
binary_predictions <- factor(ifelse(predictions_logistic > 0.5, "Y", "N"), levels = levels(train_data$success))
conf_matrix_logistic <- confusionMatrix(binary_predictions, test_data$success)

# --- Combined ROC Plot ---
combined_roc_plot <- function() {
  roc_dt <- roc(test_data$success, predictions_dt)
  roc_rf <- roc(test_data$success, predictions_rf)
  roc_knn <- roc(test_data$success, as.numeric(knn_predictions == "Y"))
  roc_logistic <- roc(test_data$success, predictions_logistic)
  
  auc_dt <- round(auc(roc_dt), 2)
  auc_rf <- round(auc(roc_rf), 2)
  auc_knn <- round(auc(roc_knn), 2)
  auc_logistic <- round(auc(roc_logistic), 2)
  
  # Create individual data frames
  df_dt <- data.frame(FPR = 1 - roc_dt$specificities, TPR = roc_dt$sensitivities, Model = "Decision Tree")
  df_rf <- data.frame(FPR = 1 - roc_rf$specificities, TPR = roc_rf$sensitivities, Model = "Random Forest")
  df_knn <- data.frame(FPR = 1 - roc_knn$specificities, TPR = roc_knn$sensitivities, Model = "KNN")
  df_logistic <- data.frame(FPR = 1 - roc_logistic$specificities, TPR = roc_logistic$sensitivities, Model = "Logistic Regression")
  
  roc_combined <- rbind(df_dt, df_rf, df_knn, df_logistic)
  
  auc_labels <- data.frame(
    Model = c("Decision Tree", "Random Forest", "KNN", "Logistic Regression"),
    AUC = c(auc_dt, auc_rf, auc_knn, auc_logistic),
    X = c(0.8, 0.8, 0.8, 0.8),
    Y = c(0.2, 0.25, 0.3, 0.35)
  )
  
  plot <- ggplot(roc_combined, aes(x = FPR, y = TPR, color = Model, linetype = Model)) +
    geom_line(size = 1) +
    labs(title = "Combined ROC Curves for All Models", x = "False Positive Rate", y = "True Positive Rate") +
    geom_text(data = auc_labels, aes(x = X, y = Y, label = paste(Model, ": AUC =", AUC)), color = "black") +
    theme_minimal()
  
  print(plot)
}

# Call the ROC plot function
combined_roc_plot()

# --- Combined Confusion Matrix Plot ---
confusionMatrixToDataFrame <- function(conf_matrix, model_name, colors) {
  df <- as.data.frame(as.table(conf_matrix))
  colnames(df) <- c("Predicted", "Actual", "Count")
  df$Model <- model_name
  df$low_color <- colors[1]
  df$high_color <- colors[2]
  return(df)
}

colors_list <- list(
  c("lightblue", "blue"),    # Decision Tree
  c("lightgreen", "green"),  # Random Forest
  c("salmon", "red"),        # KNN
  c("lightgrey", "tan")      # Logistic Regression
)

df_dt <- confusionMatrixToDataFrame(conf_matrix_dt, "Decision Tree", colors_list[[1]])
df_rf <- confusionMatrixToDataFrame(conf_matrix_rf, "Random Forest", colors_list[[2]])
df_knn <- confusionMatrixToDataFrame(conf_matrix_knn, "KNN", colors_list[[3]])
df_logistic <- confusionMatrixToDataFrame(conf_matrix_logistic, "Logistic Regression", colors_list[[4]])

conf_combined <- rbind(df_dt, df_rf, df_knn, df_logistic)

combined_confusion_plot <- ggplot(conf_combined, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(aes(fill = Count), color = "white") +
  geom_text(aes(label = Count), vjust = 1, fontface = "bold") +
  facet_wrap(~Model, scales = "free", ncol = 2) +
  labs(title = "Confusion Matrices for All Models", x = "Actual Class", y = "Predicted Class") +
  theme_minimal() +
  scale_fill_gradientn(colours = c("#FFFFFF", conf_combined$low_color, conf_combined$high_color)) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 11),
    axis.title.y = element_text(face = "bold", size = 11),
    axis.text = element_text(face = "bold", size = 10)
  )

# Print the combined confusion matrix plot
print(combined_confusion_plot)

# --- Model Evaluation (Accuracy and F1 Score) ---
calculate_scores <- function(conf_matrix) {
  accuracy <- conf_matrix$overall['Accuracy']
  precision <- conf_matrix$byClass['Pos Pred Value']
  recall <- conf_matrix$byClass['Sensitivity']
  f1_score <- 2 * (precision * recall) / (precision + recall)
  return(list(accuracy = accuracy, f1_score = f1_score))
}

# Decision Tree Scores
scores_dt <- calculate_scores(conf_matrix_dt)
print(paste("Decision Tree Accuracy:", scores_dt$accuracy))
print(paste("Decision Tree F1 Score:", scores_dt$f1_score))

# Random Forest Scores
scores_rf <- calculate_scores(conf_matrix_rf)
print(paste("Random Forest Accuracy:", scores_rf$accuracy))
print(paste("Random Forest F1 Score:", scores_rf$f1_score))

# KNN Scores
scores_knn <- calculate_scores(conf_matrix_knn)
print(paste("KNN Accuracy:", scores_knn$accuracy))
print(paste("KNN F1 Score:", scores_knn$f1_score))

# Logistic Regression Scores
scores_logistic <- calculate_scores(conf_matrix_logistic)
print(paste("Logistic Regression Accuracy:", scores_logistic$accuracy))
print(paste("Logistic Regression F1 Score:", scores_logistic$f1_score))
