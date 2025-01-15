# DeathsofDespair
Deaths Due to Suicide, Drug Poisoning, or Alcohol-Specific Causes in English Coastal Areas (2011–2018)

## Project Overview
Analysis of mortality trends related to suicide, drug poisoning, and alcohol-specific deaths across English coastal regions, examining gender disparities, geographical patterns, and temporal trends.

## Aim
To investigate trends in deaths of despair (DoD) in English coastal towns and analyze geographic variations to identify areas with concerning mortality rates, while exploring gender-specific patterns to inform public health interventions.

## Objectives

1. Analyze gender-specific variations in mortality rates
2. Identify high-risk coastal areas through geographical analysis
3. Examine temporal trends in mortality rates from 2011-2018
4. Develop predictive models for mortality rate estimation

## Research Questions 

1.	How do suicide, drug poisoning, and alcohol-related death rates vary by gender?

2.	Are there specific coastal areas in England with higher mortality rates of suicide, drug poisoning, or alcohol-related deaths compared to others?

3.	How have the suicide, drug-poisoning, and alcohol-specific mortality rates changed over time in coastal areas from 2011-2018 ? 

## Methodology
Data Source

- Office of National Statistics (ONS) data. <br>
- Age-standardized mortality rates (ASRs) per 100,000 population. <br>
- ICD-10 coded mortality data for suicide, drug poisoning, and alcohol-specific deaths <br>

## Analysis Pipeline

- Data Preprocessing <br>
- Standardization of column names <br>
- Handling of unreliable data points <br>
- Missing value treatment <br>
- Outlier detection using IQR <br>

## Statistical Analysis

- Descriptive statistics <br>
- Gender-specific mortality analysis <br>
- Geographical distribution analysis <br>
- Time series trend analysis <br>

## Predictive Modeling

- Multiple Linear Regression <br>
- Random Forest Regression <br>
- 70/30 train-test split <br>
- Hyperparameter optimization <br>

## Key Results

- Significant gender disparities with higher male mortality rates <br>
- Geographical clustering of high-risk coastal areas identified <br>
- Increasing temporal trends in drug-related deaths <br>
- Superior predictive accuracy achieved with Random Forest model <br>
- Clear patterns of regional variation in mortality rates <br>

## Tools & Libraries

- R Statistical Software <br>
- caret & randomForest for ML <br>
- pheatmap for visualization <br>
- outliers package for anomaly detection <br>
