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
      ~ if_else(is.character(.), 
                as.numeric(str_replace_all(., ":", NA_character_)), 
                as.numeric(.)) %>%
        round(decimal_places)
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





Age_Males <- read_excel("C:/Users/SANIDDHI/University Of Sheffield/R programming/assignment/coastaldeaths/coastaldeaths.xlsx", sheet = "Table 4_Age_Males",
                        col_names = TRUE, skip = 4)

Age_Females <- read_excel("C:/Users/SANIDDHI/University Of Sheffield/R programming/assignment/coastaldeaths/coastaldeaths.xlsx", sheet = "Table 5_Age_Females",
                          col_names = TRUE, skip = 4)

Age_Females <- Age_Females %>% rename(Cause_of_death = `Cause of death`,
                                      Coastal_Classification = `COASTAL CLASSIFICATION`,
                                      Age24_and_under_Deaths_Females = `Deaths...3`,
                                      Mortalityrate_Females_24_and_under = `Rate...4`,
                                      LCL_Females_24_and_under = `LCL...5`,
                                      UCL_Females_24_and_Under = `UCL...6`,
                                      
                                      Age25_44_Deaths_Females = `Deaths...8`,
                                      Mortalityrate_Females_25_44 = `Rate...9`,
                                      LCL_Females_25_44 = `LCL...10`,
                                      UCL_Females_25_44 = `UCL...11`,
                                      
                                      Age45_64_Death_Females = Deaths...13,
                                      Mortalityrate_Females_45_64 = Rate...14,
                                      LCL_Females_45_64 = LCL...15,
                                      UCL_Females_45_64 = UCL...16,
                                      
                                      Age65_74_Death_Females = Deaths...18,
                                      Mortalityrate_Females_65_74 = Rate...19,
                                      LCL_Females_65_74 = LCL...20,
                                      UCL_Females_65_74 = UCL...21,
                                      
                                      Age75_Death_Females = Deaths...23,
                                      Mortalityrate_Females_75 = Rate...24,
                                      LCL_Females_75 = LCL...25,
                                      UCL_Females_75 = UCL...26,
) 


Age_Males <- Age_Males %>% rename(Cause_of_death = `Cause of death`,
                                  Coastal_Classification = `COASTAL CLASSIFICATION`,
                                  Age24_and_under_Deaths_Males = `Deaths...3`,
                                  Mortalityrate_Males_24_and_under = `Rate...4`,
                                  LCL_Males_24_and_under = `LCL...5`,
                                  UCL_Males_24_and_Under = `UCL...6`,
                                  
                                  Age25_44_Deaths_Males = `Deaths...8`,
                                  Mortalityrate_Males_25_44 = `Rate...9`,
                                  LCL_Males_25_44 = `LCL...10`,
                                  UCL_Males_25_44 = `UCL...11`,
                                  
                                  Age45_64_Death_Males = Deaths...13,
                                  Mortalityrate_Males_45_64 = Rate...14,
                                  LCL_Males_45_64 = LCL...15,
                                  UCL_Males_45_64 = UCL...16,
                                  
                                  Age65_74_Death_Males = Deaths...18,
                                  Mortalityrate_Males_65_74 = Rate...19,
                                  LCL_Males_65_74 = LCL...20,
                                  UCL_Males_65_74 = UCL...21,
                                  
                                  Age75_Death_Males = Deaths...23,
                                  Mortalityrate_Males_75 = Rate...24,
                                  LCL_Males_75 = LCL...25,
                                  UCL_Males_75 = UCL...26,
) 


Age_Females <- Age_Females [1:24, ]
Age_Males <- Age_Males [1:24, ]

# Load necessary library
library(dplyr)
library(stringr)

# Function to clean and round specified columns
clean_columns1 <- function(data, columns, decimal_places = 1) {
  # Identify missing columns
  missing_columns <- setdiff(columns, colnames(data))
  
  if (length(missing_columns) > 0) {
    warning(paste("The following columns are missing in the dataset:", 
                  paste(missing_columns, collapse = ", ")))
  }
  
  # Proceed with cleaning and rounding for columns that exist
  data %>%
    mutate(across(
      all_of(intersect(columns, colnames(data))),  # Only process existing columns
      ~ as.numeric(str_replace_all(., ":", NA_character_)) %>% round(decimal_places)
    ))
}

# Columns to clean
columns_to_clean1 <- c(
  # Female columns
  "Age24_and_under_Deaths_Females",
  "Mortalityrate_Females_24_and_under", 
  "LCL_Females_24_and_under",
  "UCL_Females_24_and_Under", 
  "Age25_44_Deaths_Females", 
  "Mortalityrate_Females_25_44",
  "LCL_Females_25_44",
  "UCL_Females_25_44",
  "Age45_64_Death_Females",
  "Mortalityrate_Females_45_64",
  "LCL_Females_45_64",
  "UCL_Females_45_64",
  "Age65_74_Death_Females",
  "Mortalityrate_Females_65_74",
  "LCL_Females_65_74",
  "UCL_Females_65_74",
  "Age75_Death_Females",
  "Mortalityrate_Females_75",
  "LCL_Females_75",
  "UCL_Females_75",
  
  # Male columns
  "Age24_and_under_Deaths_Males",
  "Mortalityrate_Males_24_and_under",
  "LCL_Males_24_and_under",
  "UCL_Males_24_and_Under",
  "Age25_44_Deaths_Males",
  "Mortalityrate_Males_25_44",
  "LCL_Males_25_44",
  "UCL_Males_25_44",
  "Age45_64_Death_Males",
  "Mortalityrate_Males_45_64",
  "LCL_Males_45_64",
  "UCL_Males_45_64",
  "Age65_74_Death_Males",
  "Mortalityrate_Males_65_74",
  "LCL_Males_65_74",
  "UCL_Males_65_74",
  "Age75_Death_Males",
  "Mortalityrate_Males_75",
  "LCL_Males_75",
  "UCL_Males_75"
)

# Function to clean both datasets
clean_both <- function(females, males, columns) {
  cleaned_females <- clean_columns1(females, columns)
  cleaned_males <- clean_columns1(males, columns)
  
  # Check columns missing from both datasets
  missing_from_both <- setdiff(columns, union(colnames(females), colnames(males)))
  if (length(missing_from_both) > 0) {
    warning(paste("The following columns are missing from both datasets:", 
                  paste(missing_from_both, collapse = ", ")))
  }
  
  list(
    Age_Females = cleaned_females,
    Age_Males = cleaned_males
  )
}

# Apply the function to clean the datasets
result <- clean_both(Age_Females, Age_Males, columns_to_clean1)

# Extract the cleaned datasets
Age_Females <- result$Age_Females
Age_Males <- result$Age_Males


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


Age_Males <- process_unreliable_data(Age_Males, c("UCL_Males_24_and_Under", "UCL_Males_25_44",  "UCL_Males_45_64",
                                                  "UCL_Males_65_74",  "UCL_Males_75"),c("...7","...12","...17","...22","...27"))

Age_Females <- process_unreliable_data(Age_Females, c("UCL_Females_24_and_Under","UCL_Females_25_44","UCL_Females_45_64","UCL_Females_65_74","UCL_Females_75"),c("...7","...12","...17","...22","...27"))


# Delete specific columns by column name
Age_Males <- Age_Males %>% select(-...7,-...12,-...17,-...22,-...27)
Age_Females <- Age_Females %>% select(-...7,-...12,-...17,-...22,-...27)

# Function to rearrange columns
rearrange_columns <- function(data, column_order) {
  # Identify missing columns
  missing_columns <- setdiff(column_order, colnames(data))
  
  if (length(missing_columns) > 0) {
    message("The following columns are missing in the dataset and will be skipped: ",
            paste(missing_columns, collapse = ", "))
  }
  
  # Use only the columns that exist in the dataset
  existing_columns <- intersect(column_order, colnames(data))
  
  # Rearrange the columns in the specified order
  data %>% select(all_of(existing_columns), everything())
}

# Function to rearrange columns for multiple datasets
rearrange_datasets <- function(datasets, column_order) {
  lapply(datasets, rearrange_columns, column_order)
}
# Define the column order
column_order <- c(
  "Cause_of_death", "Coastal_Classification",
  "Age24_and_under_Deaths_Males", "Mortalityrate_Males_24_and_under",
  "LCL_Males_24_and_under", "UCL_Males_24_and_Under",
  "Combined_UCL_Males_24_and_Under", "Unreliable_UCL_Males_24_and_Under", "Clean_UCL_Males_24_and_Under",
  "Age25_44_Deaths_Males", "Mortalityrate_Males_25_44", "LCL_Males_25_44", "UCL_Males_25_44",
  "Combined_UCL_Males_25_44", "Unreliable_UCL_Males_25_44", "Clean_UCL_Males_25_44",
  "Age45_64_Death_Males", "Mortalityrate_Males_45_64", "LCL_Males_45_64", "UCL_Males_45_64",
  "Combined_UCL_Males_45_64", "Unreliable_UCL_Males_45_64", "Clean_UCL_Males_45_64",
  "Age65_74_Death_Males", "Mortalityrate_Males_65_74", "LCL_Males_65_74", "UCL_Males_65_74",
  "Combined_UCL_Males_65_74", "Unreliable_UCL_Males_65_74", "Clean_UCL_Males_65_74",
  "Age75_Death_Males", "Mortalityrate_Males_75", "LCL_Males_75", "UCL_Males_75",
  "Combined_UCL_Males_75", "Unreliable_UCL_Males_75", "Clean_UCL_Males_75",
  "Age24_and_under_Deaths_Females", "Mortalityrate_Females_24_and_under",
  "LCL_Females_24_and_under", "UCL_Females_24_and_Under",
  "Combined_UCL_Females_24_and_Under", "Unreliable_UCL_Females_24_and_Under", "Clean_UCL_Females_24_and_Under",
  "Age25_44_Deaths_Females", "Mortalityrate_Females_25_44", "LCL_Females_25_44", "UCL_Females_25_44",
  "Combined_UCL_Females_25_44", "Unreliable_UCL_Females_25_44", "Clean_UCL_Females_25_44",
  "Age45_64_Death_Females", "Mortalityrate_Females_45_64", "LCL_Females_45_64", "UCL_Females_45_64",
  "Combined_UCL_Females_45_64", "Unreliable_UCL_Females_45_64", "Clean_UCL_Females_45_64",
  "Age65_74_Death_Females", "Mortalityrate_Females_65_74", "LCL_Females_65_74", "UCL_Females_65_74",
  "Combined_UCL_Females_65_74", "Unreliable_UCL_Females_65_74", "Clean_UCL_Females_65_74",
  "Age75_Death_Females", "Mortalityrate_Females_75", "LCL_Females_75", "UCL_Females_75",
  "Combined_UCL_Females_75", "Unreliable_UCL_Females_75", "Clean_UCL_Females_75"
)

# Combine datasets in a list
datasets <- list(Age_Males = Age_Males, Age_Females = Age_Females)

# Rearrange columns for both datasets
rearranged_datasets <- rearrange_datasets(datasets, column_order)

# Extract the rearranged datasets
Age_Males <- rearranged_datasets$Age_Males
Age_Females <- rearranged_datasets$Age_Females

# Merging the two datasets without suffixes
merge_datasets <- function(Age_Females, Age_Males, by_columns) {
  merge(Age_Females, Age_Males, by = by_columns, all = TRUE)
}

# Specify the columns to merge by
common_columns <- c("Cause_of_death", "Coastal_Classification")

# Merge Age_Males and Age_Females
merged_data <- merge_datasets(Age_Males, Age_Females, by_columns = common_columns)

# View the merged dataset
head(merged_data)


# Filter rows for the year 2018
filter_2018 <- function(data) {
  data %>% 
    filter(Year == 2018) %>%  # Keep only rows where Year is 2018
    mutate(Year = 2018)       # Ensure the Year column is explicitly added
}

# Apply filtering to the dataset containing year 2018
filtered_2018 <- filter_2018(cause_data)


final_merged <- inner_join(filtered_2018,merged_data, by = common_columns )

unmatched_filtered <- anti_join(filtered_2018, merged_data, by = "Cause_of_death")


#timeseries-trendlines 
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

Line_chart <- pivot_longer(
  cause_data,
  cols = c(Deaths_Male, Deaths_Female),
  names_to = "Gender",
  values_to = "Death_Rate"
) %>%
  group_by(Year, Cause_of_death, Gender) %>%
  summarize(
    Death_Rate = mean(Death_Rate, na.rm = TRUE)
  ) %>%
  ungroup()


ggplot(Line_chart, aes(x = Year, y = Death_Rate, color = Cause_of_death)) +
  geom_line(linewidth = 1.5, alpha = 0.9) +
  geom_point(linewidth = 1.5, shape = 21, fill = "black", color = "black") +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_y_continuous(labels = scales::comma, limits = c(0, max(Line_chart$Death_Rate) * 1.1)) +
  scale_x_continuous(breaks = seq(min(Line_chart$Year), max(Line_chart$Year), by = 1)) +  
  labs(
    title = "Death Rates Over Time by Gender (2011-2018)",
    subtitle = "Analysis of Suicide, Drug Poisoning, and Alcohol-related Deaths",
    x = "Year",
    y = "Average Death Rate",
    color = "Causes of Death"
  ) +
  facet_wrap(~ Gender) +  # Separate facets by gender
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

# Reshape Data for Stacked Bar Chart
stacked_barchart <- merged_data %>%
  select(Cause_of_death, 
         Mortalityrate_Females_24_and_under, Mortalityrate_Males_24_and_under,
         Mortalityrate_Females_25_44, Mortalityrate_Males_25_44,
         Mortalityrate_Females_45_64, Mortalityrate_Males_45_64,
         Mortalityrate_Females_65_74, Mortalityrate_Males_65_74,
         Mortalityrate_Females_75, Mortalityrate_Males_75) %>%
  pivot_longer(
    cols = starts_with("Mortalityrate"), 
    names_to = "Group", 
    values_to = "Mortality_Rate"
  ) %>%
  separate(Group, into = c("Metric", "Gender", "Age_Group"), sep = "_", extra = "merge") %>%
  mutate(
    Age_Group = case_when(
      Age_Group == "24_and_under" ~ "24 and under",
      Age_Group == "25_44" ~ "25-44",
      Age_Group == "45_64" ~ "45-64",
      Age_Group == "65_74" ~ "65-74",
      Age_Group == "75" ~ "75 and above"
    ),
    Gender = ifelse(Gender == "Females", "Female", "Male")
  )

# Load Viridis Library for Colors
library(viridis)

# Create Stacked Bar Chart
ggplot(stacked_barchart, aes(x = Age_Group, y = Mortality_Rate, fill = Cause_of_death)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Gender, ncol = 1) +
  scale_fill_viridis_d(
    option = "D", 
    name = "Cause of Death"
  ) +
  labs(
    title = "Mortality Rates by Age Groups and Gender in 2018",
    subtitle = "For Causes of Death: Suicide, Drug Poisoning, and Alcohol-Specific",
    x = "Age Group",
    y = "Mortality Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10))
  )

library(ggplot2)
library(dplyr)

# Data preparation
line_data <- cause_data %>%
  filter(
    !is.na(Year),
    !is.na(Coastal_Classification),
    !is.na(Mortalityrate_Male),
    !is.na(Mortalityrate_Female),
    !is.na(Clean_UCL_Male),
    !is.na(LCL_Male),
    !is.na(Clean_UCL_Female),
    !is.na(LCL_Female)
  ) %>%
  mutate(
    Combined_UCL = (Clean_UCL_Male + Clean_UCL_Female) / 2,
    Combined_LCL = (LCL_Male + LCL_Female) / 2,
    Combined_Mortality_Rate = (Mortalityrate_Male + Mortalityrate_Female) / 2,
    Coastal_Group = case_when(
      Coastal_Classification %in% c("COASTAL CITY", "LARGER SEASIDE TOWN", "SMALLER SEASIDE TOWN", "LARGER OTHER COASTAL TOWN", "SMALLER OTHER COASTAL TOWN") ~ "Coastal Towns",
      TRUE ~ "Non-Coastal Towns"
    )
  ) %>%
  group_by(Coastal_Group) %>%
  summarise(
    Average_Mortality_Rate = mean(Combined_Mortality_Rate, na.rm = TRUE),
    Average_UCL = mean(Combined_UCL, na.rm = TRUE),
    Average_LCL = mean(Combined_LCL, na.rm = TRUE)
  )

# Bar Chart with UCL and LCL as Dashed Lines and Annotations
ggplot(line_data, aes(x = Coastal_Group, y = Average_Mortality_Rate, fill = Coastal_Group)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  # Add dashed line for UCL (only one per category)
  geom_hline(aes(yintercept = max(Average_UCL)), linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(x = 2, y = max(Average_UCL), label = "UCL", vjust = -0.5), color = "red") +
  # Add dashed line for LCL (only one per category)
  geom_hline(aes(yintercept = min(Average_LCL)), linetype = "dashed", color = "blue", size = 1) +
  geom_text(aes(x = 2, y = min(Average_LCL), label = "LCL", vjust = 1.5), color = "blue") +
  labs(
    title = "Average Mortality Rates by Coastal and Non-Coastal Towns",
    subtitle = "Dashed lines represent Upper (UCL) and Lower (LCL) Control Limits",
    x = "Coastal Group",
    y = "Average Mortality Rate",
    fill = "Coastal Group"
  ) +
  scale_fill_manual(
    values = c(
      "Coastal Towns" = "green",
      "Non-Coastal Towns" = "yellow"
    ),
    labels = c("Coastal Towns" = "Coastal Towns", "Non-Coastal Towns" = "Non-Coastal Towns")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "top"
  )

# Required Libraries
library(ggplot2)
library(dplyr)
install.packages("treemapify")
library(treemapify) # For creating treemaps

# Summarize the data for plotting
treemap_data <- cause_data %>%
  group_by(Coastal_Classification, Cause_of_death) %>%
  summarise(
    Total_Deaths = sum(Deaths_Male, Deaths_Female, na.rm = TRUE)
  ) %>%
  ungroup()

# Ensure proper ordering for the treemap
treemap_data <- treemap_data %>%
  mutate(
    Coastal_Classification = factor(Coastal_Classification, levels = unique(Coastal_Classification)),
    Cause_of_death = factor(Cause_of_death, levels = unique(Cause_of_death))
  )

# Create Treemap
ggplot(treemap_data, aes(
  area = Total_Deaths, 
  fill = Cause_of_death, 
  label = paste0(Cause_of_death, "\n", format(Total_Deaths, big.mark = ",")), 
  subgroup = Coastal_Classification
)) +
  geom_treemap() + 
  geom_treemap_subgroup_border(color = "white", size = 1) + 
  geom_treemap_text(
    fontface = "bold", 
    color = "white", 
    place = "centre", 
    size = 1.5, 
    grow = TRUE
  ) + 
  geom_treemap_subgroup_text(
    fontface = "bold", 
    color = "black", 
    place = "bottom", 
    size = 6
  ) + 
  scale_fill_brewer(palette = "Reds", name = "Cause of Death") + 
  labs(
    title = "Treemap of Deaths Rates by Coastal Classification",
    subtitle = "For Causes: Suicide, Drug Poisoning, Alcohol-Related Deaths"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 14, hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid = element_blank()
  )

# Ensure all components are ggplot objects
Composite_Visualization <- (Line_chart + stacked_barchart) /
  (treemap_data + line_plot) + 
  plot_layout(guides = "collect") + 
  plot_annotation(
    title = "Composite Visualization",
    subtitle = "Combined Visualizations of Different Data Representations",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 16, hjust = 0.5)
    )
  )

# Print the composite plot
print(Composite_Visualization)
