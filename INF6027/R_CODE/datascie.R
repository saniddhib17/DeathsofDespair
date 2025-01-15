library(dplyr)
library(stringr)
library(readxl)
install.packages("tidyr")
library(tidyr)
library(caret)
install.packages("caret")
library(rpart)
install.packages("rpart")
library(rpart.plot)
install.packages("rpart.plot")
library(ggplot2)
install.packages("pheatmap")
library(pheatmap)

Suicide <- read_excel("C:/Users/SANIDDHI/University Of Sheffield/R programming/assignment/coastaldeaths/coastaldeaths.xlsx", 
                      sheet = "Table 1_Suicide", col_names = TRUE, skip = 4)

Drug_poisoning <- read_excel("C:/Users/SANIDDHI/University Of Sheffield/R programming/assignment/coastaldeaths/coastaldeaths.xlsx", 
                             sheet = "Table 2_Drug-poisoning", col_names = TRUE, skip = 4)

Alcohol_specific <- read_excel("C:/Users/SANIDDHI/University Of Sheffield/R programming/assignment/coastaldeaths/coastaldeaths.xlsx", 
                               sheet = "Table 3_Alcohol-specific", col_names = TRUE, skip = 4)



# Rename columns for consistency
rename_columns <- function(data) {
  data %>%
    rename(
      Year = Year,
      Coastal_Classification = `COASTAL CLASSIFICATION`,
      Deaths_Male = `Deaths...3`,
      Mortalityrate_Male = `Age-standardised mortality rate...4`,
      LCL_Male = `Lower confidence limit...5`,
      UCL_Male = `Upper confidence limit...6`,
      Deaths_Female = `Deaths...9`,
      Mortalityrate_Female = `Age-standardised mortality rate...10`,
      LCL_Female = `Lower confidence limit...11`,
      UCL_Female = `Upper confidence limit...12`
    )
}

Suicide <- rename_columns(Suicide)
Drug_poisoning <- rename_columns(Drug_poisoning)
Alcohol_specific <- rename_columns(Alcohol_specific)

# Remove unnecessary rows
Suicide <- Suicide[1:64, ]
Drug_poisoning <- Drug_poisoning[1:64, ]
Alcohol_specific <- Alcohol_specific[1:64, ]

#checking for missing values 
any(is.na(Suicide))
any(is.na(Drug_poisoning))
any(is.na(Alcohol_specific))


# Clean and round specified columns
clean_columns <- function(data, columns, decimal_places = 1) {
  data %>%
    mutate(across(
      all_of(columns),
      ~ as.numeric(str_replace_all(., ":", NA_character_)) %>% round(decimal_places)
    ))
}

columns_to_clean <- c("UCL_Male", "UCL_Female", "Mortalityrate_Male", "Mortalityrate_Female", "LCL_Male", "LCL_Female", "Year")

Suicide <- clean_columns(Suicide, columns_to_clean)
Drug_poisoning <- clean_columns(Drug_poisoning, columns_to_clean)
Alcohol_specific <- clean_columns(Alcohol_specific, columns_to_clean)

# Handle unreliable data for both UCL_Male and UCL_Female
# Check if the columns exist in the dataset
process_unreliable_data <- function(data, numeric_cols, flag_cols) {
  for (i in seq_along(numeric_cols)) {
    numeric_col <- numeric_cols[i]
    flag_col <- flag_cols[i]
    
    if (!(numeric_col %in% colnames(data)) || !(flag_col %in% colnames(data))) {
      stop(paste("Error: Missing column(s) -", numeric_col, "or", flag_col))
    }
    
    data <- data %>%
      mutate(
        # Safely replace ":" with NA in numeric column and convert to numeric
        {{ numeric_col }} := as.numeric(str_replace_all(.data[[numeric_col]], ":", NA_character_)),
        
        # Ensure flag column is trimmed and valid
        {{ flag_col }} := str_trim(as.character(.data[[flag_col]])),
        
        # Add unreliable flag and combined columns with u
        !!paste0("Unreliable_", numeric_col) := ifelse(!is.na(.data[[flag_col]]) & .data[[flag_col]] == "u", TRUE, FALSE),
        !!paste0("Combined_", numeric_col) := ifelse(
          .data[[paste0("Unreliable_", numeric_col)]] == TRUE,
          paste0(.data[[numeric_col]], "u"),
          as.character(.data[[numeric_col]])
        ),
        !!paste0("Clean_", numeric_col) := as.numeric(str_replace(.data[[paste0("Combined_", numeric_col)]], "u", ""))
      )
  }
  
  return(data)
}

# Apply the function to both UCL_Male and UCL_Female
Suicide <- process_unreliable_data(Suicide, c("UCL_Male", "UCL_Female"), c("...7", "...13"))
Drug_poisoning <- process_unreliable_data(Drug_poisoning, c("UCL_Male", "UCL_Female"), c("...7", "...13"))
Alcohol_specific <- process_unreliable_data(Alcohol_specific, c("UCL_Male", "UCL_Female"), c("...7", "...13"))

# Rearrange columns
rearrange_columns <- function(data, column_order) {
  if (!all(column_order %in% colnames(data))) {
    stop("Error: Column order contains missing columns in dataset.")
  }
  data %>% select(all_of(column_order))
}

column_order <- c(
  "Year", "Coastal_Classification", "Deaths_Male", "Mortalityrate_Male", "LCL_Male", "UCL_Male", 
  "Combined_UCL_Male", "Unreliable_UCL_Male", "Clean_UCL_Male",
  "Deaths_Female", "Mortalityrate_Female", "LCL_Female", "UCL_Female", 
  "Combined_UCL_Female", "Unreliable_UCL_Female", "Clean_UCL_Female"
)

Suicide <- rearrange_columns(Suicide, column_order)
Drug_poisoning <- rearrange_columns(Drug_poisoning, column_order)
Alcohol_specific <- rearrange_columns(Alcohol_specific, column_order)

# View datasets
View(Suicide)
View(Drug_poisoning)
View(Alcohol_specific)

cause_data <- bind_rows(
  Suicide %>% mutate(Cause_of_death = "Suicide"),
  Drug_poisoning %>% mutate(Cause_of_death = "Drug-poisoning"),
  Alcohol_specific %>% mutate(Cause_of_death = "Alcohol-specific"),
)
# Move the columns
cause_data <- cause_data %>% relocate(Cause_of_death, .after = Coastal_Classification)
cause_data <- cause_data %>% select(-c(UCL_Male,UCL_Female)) 

summary(cause_data)
summary(cause_data$Deaths_Male)
summary(cause_data$Deaths_Female)

#EXPLORATORY DATA ANALYSIS :

library(tidyverse)

# Pivot and summarize data
Line_chart <- pivot_longer(
  cause_data,
  cols = c(Deaths_Male, Deaths_Female), # Adjust column names if they differ
  names_to = "Gender",
  values_to = "Death_Rate"
) %>%
  group_by(Year, Cause_of_death, Gender) %>%
  summarize(
    Death_Rate = mean(Death_Rate, na.rm = TRUE)
  ) %>%
  ungroup()

# Check structure of the resulting data
str(Line_chart)

# Plot the line chart
ggplot(Line_chart, aes(x = Year, y = Death_Rate, color = Cause_of_death)) +
  geom_line(linewidth = 1.5, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "black", color = "black") +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_y_continuous(labels = scales::comma, limits = c(0, max(Line_chart$Death_Rate) * 1.1)) +
  scale_x_continuous(breaks = seq(min(Line_chart$Year), max(Line_chart$Year), by = 1)) + # Ensure all years are shown
  labs(
    title = "Death Rates Over Time by Gender (2011-2018)",
    subtitle = "Analysis of Suicide, Drug Poisoning, and Alcohol-related Deaths",
    x = "Year",
    y = "Average Death Rate",
    color = "Causes of Death"
  ) +
  facet_wrap(~ Gender) + # Separate facets by gender
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#02062e"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "blue"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "#566573"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray20", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#dce9fa", color = NA),
    plot.background = element_rect(fill = "#FDFEFE", color = "black", size = 1)
  )


# Boxplot comparing mortality rates by cause of death and gender
ggplot(cause_data, aes(x = Cause_of_death, y = Deaths_Male, fill = "Male")) +
  geom_boxplot() +
  geom_boxplot(data = cause_data, aes(y = Deaths_Female, fill = "Female"), alpha = 0.5,outlier.colour = "Black") +
  labs(title = "Comparison of Deaths Rates by Gender", x = "Cause of Death", y = "Mortality Rate") +
  scale_fill_manual(values = c("Male" = "orange", "Female" = "yellow")) +
  theme_grey()

# Calculate averages
all_data_summary <- cause_data %>%
  group_by(Cause_of_death) %>%
  summarize(
    Avg_Deaths_Male = mean(Deaths_Male, na.rm = TRUE),
    Avg_Deaths_Female = mean(Deaths_Female, na.rm = TRUE),
    Avg_Mortalityrates_Male = mean(Mortalityrate_Male , na.rm = TRUE),
    Avg_Mortalityrates_Female = mean(Mortalityrate_Female, na.rm = TRUE)
  )

# Reshape data to long format for plotting
gender_summary <- all_data_summary %>%
  pivot_longer(
    cols = c(Avg_Deaths_Male, Avg_Deaths_Female),
    names_to = "Gender",
    values_to = "Avg_Deaths"
  ) %>%
  mutate(Gender = recode(Gender,
                         "Avg_Deaths_Male" = "Male",
                         "Avg_Deaths_Female" = "Female"))

# Plotting
library(ggplot2)

ggplot(gender_summary, aes(x = Cause_of_death, y = Avg_Deaths, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Death Rates by Gender and Cause of Death",
    subtitle = "Comparison Between Male and Female",
    x = "Cause of Death",
    y = "Average Death Rates",
    fill = "Gender"
  ) +
  scale_fill_manual(
    values = c("Female" = "#87CEEB", "Male" = "#6A5ACD"), 
    labels = c("Female", "Male")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    
  )

# Aggregate data for heatmap
heatmap_data <- cause_data %>%
  group_by(Coastal_Classification, Cause_of_death) %>%
  summarize(
    Avg_MortalityMale = mean(Mortalityrate_Male, na.rm = TRUE),
    Avg_MortalityFemale = mean(Mortalityrate_Female, na.rm = TRUE),
    .groups = "drop" # Drop the grouping after summarization
  ) %>%
  pivot_longer(
    cols = c(Avg_MortalityMale, Avg_MortalityFemale),
    names_to = "Gender",
    values_to = "Mortality_Rate"
  )

heatmap_data <- heatmap_data %>%
  unite("Cause_Gender", Cause_of_death, Gender, sep = "_") %>% # Combine Cause of Death and Gender
  pivot_wider(
    names_from = Cause_Gender,
    values_from = Mortality_Rate
  )

colnames(heatmap_data) <- colnames(heatmap_data) %>%
  gsub("Alcohol-specific", "Alcohol", .) %>%
  gsub("Drug-poisoning", "Drugs", .) %>%
  gsub("_Avg_MortalityMale", "_Male", .) %>%
  gsub("_Avg_MortalityFemale", "_Female", .)

# Convert data to matrix format
heatmap_matrix <- as.matrix(heatmap_data[,-1])
rownames(heatmap_matrix) <- tools::toTitleCase(tolower(heatmap_data$Coastal_Classification))

# Plot the heatmap
pheatmap(
  heatmap_matrix,
  main = "Heatmap of Mortality Rates by Coastal Area and Cause of Death",
  Colv = NA,
  Rowv = NA,
  col = colorRampPalette(c("lightblue", "darkblue"))(50),
  display_numbers = TRUE,
  fontsize = 9, # Font size for readability
  fontsize_row = 10, # Font size for row labels
  fontsize_col = 10, # Font size for column labels
  border_color = "grey90", # Border color
  legend = TRUE,
  number_color = "white",
  cluster_rows = FALSE, # Disable row clustering for cleaner layout
  cluster_cols = FALSE, # Disable column clustering
  cellwidth = 40, # Adjust cell width
  cellheight = 25 # Adjust cell height
  )


#Age_Female and Age_Male having unreliable data and outliers :
#Identify outliers: Observed rates outside the confidence interval
# Identify outliers: Observed rates outside the confidence interval

# Install and load the outliers package
install.packages("outliers")
library(outliers)

# Load the dataset
# Function to detect outliers for all numeric columns
detect_outliers_all <- function(cause_data, prob = 0.95) {
  # Select only numeric columns
  numeric_columns <- cause_data %>%
    select(where(is.numeric))
  
  # Apply z-score method to each column
  outlier_results <- lapply(numeric_columns, function(column) {
    scores(column, type = "z", prob = prob)
  })
  
  # Combine results into a data frame
  names(outlier_results) <- colnames(numeric_columns)
  return(outlier_results)
}

# Detect outliers in the dataset
outliers_all <- detect_outliers_all(cause_data, prob = 0.95)

# Print outliers for each column
for (col_name in names(outliers_all)) {
  cat("\nOutliers in column:", col_name, "\n")
  print(outliers_all[[col_name]])
}


# Function to compute frequency distribution for categorical variables
frequency_distribution <- function(cause_data) {
  # Select categorical columns
  categorical_vars <- names(cause_data)[sapply(cause_data, is.factor) | sapply(cause_data, is.character)]
  
  # Generate frequency tables for each categorical variable
  freq_list <- lapply(categorical_vars, function(var) {
    freq_table <- cause_data %>%
      group_by(.data[[var]]) %>%
      summarise(Frequency = n()) %>%
      arrange(desc(Frequency))
    
    return(freq_table)
  })
  
  names(freq_list) <- categorical_vars
  return(freq_list)
}

# Perform frequency distribution
freq_tables <- frequency_distribution(cause_data)

# Example: Print frequency table for a specific variable (e.g., "Category")
print(freq_tables[["Cause_of_death"]])  # Replace "Category" with your column name
print(freq_tables[["Coastal_Classification"]])

# Load required libraries
library(dplyr)
library(ggplot2)
library(caret)

# Data Preparation
cause_data$Cause_of_death <- as.factor(cause_data$Cause_of_death)
cause_data$Coastal_Classification <- as.factor(cause_data$Coastal_Classification)

# Combine Male and Female Mortality Rates for Analysis
linear_regression_data <- cause_data %>%
  mutate(Mortality_Combined = (Mortalityrate_Male + Mortalityrate_Female) / 2)

# Remove rows with missing values
linear_regression_data <- na.omit(linear_regression_data)

# Inspect the structure of the dataset
str(linear_regression_data)

# Train-Test Split
set.seed(123)  # For reproducibility
split_index <- createDataPartition(linear_regression_data$Mortality_Combined, p = 0.7, list = FALSE)
train_data <- linear_regression_data[split_index, ]
test_data <- linear_regression_data[-split_index, ]

# Linear Regression Model
lm_model <- lm(Mortality_Combined ~ Cause_of_death + Coastal_Classification, data = train_data)

# Model Summary
summary(lm_model)

# Make Predictions on the Test Data
lm_predictions <- predict(lm_model, newdata = test_data)

# Evaluate Model Performance
# Calculate RMSE
lm_rmse <- sqrt(mean((lm_predictions - test_data$Mortality_Combined)^2))
print(paste("Linear Regression RMSE: ", round(lm_rmse, 2)))

# Accuracy Evaluation (Within 10% Tolerance)
accuracy_threshold <- 0.1  # 10% tolerance
accuracy <- mean(abs(lm_predictions - test_data$Mortality_Combined) / test_data$Mortality_Combined < accuracy_threshold)
print(paste("Accuracy (within 10% of actual): ", round(accuracy * 100, 2), "%"))

# Plot Actual vs Predicted with Enhanced Visualization
comparison_lm <- data.frame(
  Actual = test_data$Mortality_Combined, 
  Predicted = lm_predictions,
  Coastal_Classification = test_data$Coastal_Classification
)

ggplot(comparison_lm, aes(x = Actual, y = Predicted, color = Coastal_Classification)) +
  geom_point(size = 3, alpha = 0.8) +  # Scatter points with transparency
  geom_smooth(method = "lm", col = "black", linetype = "dashed") +  # Add regression line
  labs(
    title = "Linear Regression: Actual vs Predicted Mortality Rates",
    x = "Actual Mortality Rate",
    y = "Predicted Mortality Rate",
    color = "Coastal Classification"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
  )

#RANDOM FOREST 
# Convert Cause_of_death to a factor
library(randomForest)
cause_data$Cause_of_death <- as.factor(cause_data$Cause_of_death)

# Step 1: Prepare the Random Forest Data
randomforest_data <- cause_data %>%
  mutate(
    Mortality_Combined = (Mortalityrate_Male + Mortalityrate_Female) / 2,
    Cause_of_death = as.factor(Cause_of_death),
    Coastal_Classification = as.factor(Coastal_Classification)
  )

# Ensure the structure reflects proper factor conversion
str(randomforest_data)

# Remove missing values
randomforest_data <- na.omit(randomforest_data)

# Step 2: Split the Data into Training and Testing Sets
set.seed(123)  # For reproducibility
split_index <- createDataPartition(randomforest_data$Mortality_Combined, p = 0.7, list = FALSE)
train_data1 <- randomforest_data[split_index, ]
test_data1 <- randomforest_data[-split_index, ]

# Step 3: Extract Predictors and Response Variables
train_x1 <- train_data1[, c("Cause_of_death", "Coastal_Classification")]
train_y1 <- train_data1$Mortality_Combined
test_x1 <- test_data1[, c("Cause_of_death", "Coastal_Classification")]
test_y1 <- test_data1$Mortality_Combined

# Step 4: Train the Random Forest Regression Model
rf_model <- randomForest(Mortality_Combined ~ Cause_of_death + Coastal_Classification,
                         data = train_data1, ntree = 100)

# Step 5: Make Predictions on the Test Data
rf_predictions <- predict(rf_model, test_x1)

# Step 6: Evaluate Model Performance
# Calculate RMSE
rf_rmse <- sqrt(mean((rf_predictions - test_y1)^2))
print(paste("Random Forest RMSE: ", round(rf_rmse, 2)))

# Calculate Accuracy (within 10% of actual)
accuracy_threshold <- 0.1  # 10% tolerance
accuracy <- mean(abs(rf_predictions - test_y1) / test_y1 < accuracy_threshold)
print(paste("Accuracy (within 10% of actual): ", round(accuracy * 100, 2), "%"))


# Step 7: Scatter Plot with Coastal/Non-Coastal Differentiation
comparison_rf <- data.frame(
  Actual = test_data1$Mortality_Combined,
  Predicted = rf_predictions,
  Coastal_Classification = test_data1$Coastal_Classification
)

ggplot(comparison_rf, aes(x = Actual, y = Predicted, color = Coastal_Classification)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", col = "black", linetype = "dashed") +
  labs(
    title = "Random Forest: Actual vs Predicted Mortality Rates",
    x = "Actual Mortality Rate",
    y = "Predicted Mortality Rate",
    color = "Coastal Classification"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )

