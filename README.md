# Audience Satisfaction & Repeat Viewing Prediction

## Statistical Modeling & Machine Learning Project

A statistical modeling project conducted for the Theory and Practices in Statistical Modeling (IT3011) module at SLIIT.

The project investigates an important question:

> Does audience satisfaction influence repeat viewing and listening behavior?

Rather than relying on assumptions, the study uses primary survey data and combines statistical analysis with machine learning to identify relationships and predict repeat viewing behavior.

---

## Project Objective

The main objective of this study was to investigate whether audience satisfaction is associated with repeat viewing behavior and to determine whether satisfaction and other audience characteristics can be used to predict repeat viewing.

---

## Dataset

Primary data was collected through a Google Forms survey.

- Sample size: 489 participants
- Sampling method: Quota sampling
- Data source: Primary survey data
- Target variable: Repeat viewing behavior

Quota sampling was used to obtain representation across different age groups.

### Data Collection Considerations

The study also considered potential limitations associated with primary survey data, including:

- Selection bias from collecting responses through public locations and online platforms
- Response bias from self-reported information
- Minor deviations from the intended quota distribution

---

## Methodology

The analysis was conducted in three main stages.

### 1. Descriptive Analysis

Descriptive statistics were used to understand the overall characteristics and patterns within the collected data.

### 2. Inferential Analysis

Statistical tests were used to investigate relationships between variables, including:

- Independent samples t-test
- Chi-square test
- ANOVA

These analyses helped determine whether observed differences and relationships were statistically significant.

### 3. Predictive Modeling

Three machine learning models were developed and compared:

- Logistic Regression
- Decision Tree
- Random Forest

The models were evaluated to determine their ability to predict repeat viewing behavior.

---

## Machine Learning Models

| Model | Purpose |
|---|---|
| Logistic Regression | Baseline classification model |
| Decision Tree | Interpretable classification model |
| Random Forest | Ensemble classification model |

Model performance was evaluated using classification performance measures, including ROC-AUC.

---

## Key Findings

The analysis produced several important findings:

- Higher audience satisfaction was strongly associated with repeat viewing behavior.
- Approximately 77.1% of participants were repeat viewers.
- Repeat viewers demonstrated significantly higher satisfaction levels.
- Age showed noticeable differences in repeat viewing behavior.
- Random Forest achieved the best predictive performance with an AUC of 0.818.

The findings were consistent across the statistical analysis and predictive modeling stages, providing evidence that audience satisfaction is an important factor associated with repeat viewing behavior.

---

## Data Limitations

An important part of the project was evaluating the limitations of the collected data.

Potential sources of bias included:

- Selection bias caused by the combination of online and public-location data collection
- Response bias resulting from self-reported responses
- Minor deviations from the intended quota distribution

These limitations were considered when interpreting the statistical and machine learning results.

---

## Technologies & Methods

- R
- Statistical Modeling
- Machine Learning
- Data Cleaning and Preprocessing
- Descriptive Statistics
- Inferential Statistics
- Logistic Regression
- Decision Trees
- Random Forest
- ROC-AUC
- Data Visualization

---
