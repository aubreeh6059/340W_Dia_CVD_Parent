# Cardiovascular Disease Classification Using NHANES

## DS 340W Midterm Project

**Team Members:** Riya Merchant and Aubree Huntley

This project examines cardiovascular disease classification using data from the National Health and Nutrition Examination Survey (NHANES). Our work builds on the methodology of our parent paper and focuses on how class-balancing and feature-selection methods affect model performance.

## Research Question

How do class-balancing methods and feature-selection techniques affect CVD classification performance, and does their effectiveness vary across age groups?

## Proposed Methodology

We compare different class-balancing approaches, including no resampling, ROSE, SMOTE, and random oversampling. For feature selection, we consider no feature selection, RFE, and ANOVA.

The models considered include Logistic Regression, Random Forest, SVM, XGBoost, and LightGBM. Performance is evaluated using F1-score, recall, AUROC, and accuracy. We also plan to compare performance across age groups and use SHAP for model interpretation.

## Repository Contents

- `00_setup.R` – package setup and project configuration
- `01_load_merge_nhanes.R` – loads and merges NHANES data
- `02_missingness_outcome_impute.R` – handles missing data and prepares the outcome
- `03_split_rfe_balance.R` – data splitting, feature selection, and class balancing
- `04_train_models.R` – model training
- `05_explainability.R` – model interpretation and explainability
- `data_clean.csv` – cleaned NHANES dataset used for the project

## Current Status

This repository contains our current code and dataset for the DS 340W midterm project. The analysis is still in development and will continue to be updated as we test the proposed methods.
