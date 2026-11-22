# Cardiovascular Disease Classification Using NHANES

## DS 340W Midterm Project

**Team Members:** Riya Merchant and Aubree Huntley

This project examines cardiovascular disease classification using data from the National Health and Nutrition Examination Survey (NHANES). Our work builds on the methodology of our parent paper and focuses on how class-balancing and feature-selection methods affect model performance.

## Research Question

How do class-balancing methods and feature-selection techniques affect CVD classification performance, and does their effectiveness vary across age groups?

## Proposed Methodology

We compare different class-balancing approaches, including no resampling, ROSE, SMOTE, and random oversampling. For feature selection, we consider no feature selection, RFE, and ANOVA.

The models considered include Logistic Regression, Random Forest, SVM, XGBoost, and LightGBM. Performance is evaluated using F1-score, recall, AUROC, and accuracy. We also plan to compare performance across age groups and use SHAP for model interpretation.

## Preprocessing comparison

Run `Rscript 08_preprocessing_comparison.R` from the repository root to compare all 60 combinations of four resampling methods (none, ROSE, SMOTE, and random oversampling), three feature-selection choices (none, RFE, and ANOVA), and five classifiers. The script makes a stratified 60/20/20 train/validation/test split. Feature selection and resampling are fit using training data only. The validation set ranks combinations by F1-score, with AUROC as the tie-breaker; the held-out test set is reserved for final reporting.

The script writes `preprocessing_comparison_validation.csv` and `preprocessing_comparison_test.csv` under `output/tables/`. Both include accuracy, F1-score, AUROC, and recall. It also saves the selected features and, when age is available, test metrics for age groups 18–39, 40–59, and 60+. Install `caret`, `ROSE`, `smotefamily`, `randomForest`, `e1071`, `xgboost`, `lightgbm`, and `pROC` before running.

Classifier settings follow the parent-paper replication: logistic regression uses a binomial GLM; random forest uses 500 trees and `mtry = 5`; SVM uses an RBF kernel, cost 1, and gamma 1/30; XGBoost uses depth 8, eta 0.1, 0.8 row and column subsampling, minimum child weight 1, and up to 1,000 rounds with 50-round early stopping; LightGBM uses learning rate 0.01, 31 leaves, and up to 1,000 rounds with 50-round early stopping. Boosting early stopping uses a separate 15% inner holdout from the training partition, then refits on the full outer training partition at the selected number of rounds. The RFE option uses 30 features and repeated five-fold cross-validation (five repeats). No hyperparameter grid search is run.

After the comparison finishes, run `source("09_make_results_figures.R")` from the repository root to create heatmaps for all four metrics on both validation and test data, plus age-group figures when the age results CSV is available. Figures are saved as PNG and PDF files under `figures/results/`. This step requires `ggplot2`, `dplyr`, and `tidyr`.

## Repository Contents

- `00_setup.R` – package setup and project configuration
- `01_load_merge_nhanes.R` – loads and merges NHANES data
- `02_missingness_outcome_impute.R` – handles missing data and prepares the outcome
- `03_split_rfe_balance.R` – data splitting, feature selection, and class balancing
- `04_train_models.R` – model training
- `05_explainability.R` – model interpretation and explainability
- `data_clean.csv` – cleaned NHANES dataset used for the project

The repository includes the cleaned NHANES dataset used for the analysis. The original NHANES `.xpt` source files are not included.

## Current Status

This repository contains our current code and dataset for the DS 340W midterm project. The analysis is still in development and will continue to be updated as we test the proposed methods.


## Parent Paper Replication Progress

We conducted an initial replication of the parent paper using the cleaned NHANES 2017–2023 dataset (12,382 participants).

### Methods
- **Class distribution:** 87.74% without CVD and 12.26% with CVD
- **Train/test split:** 80/20, stratified by outcome
- **Feature selection:** RFE with random forests, using 30 selected predictors
- **Class balancing:** ROSE applied to the model training data
- **Models:** Logistic Regression, Random Forest, SVM, XGBoost, and LightGBM

### Initial Results

| Model | Accuracy | Recall (CVD) | AUROC |
|---|---|---|---|
| Logistic Regression | 68.61% | 77.23% | 0.8014 |
| Random Forest | 66.91% | 78.22% | 0.7905 |
| SVM | 69.05% | 74.59% | 0.7878 |
| XGBoost | 67.80% | 76.24% | 0.7875 |
| LightGBM | 66.38% | 78.55% | 0.7889 |

These are preliminary results from a parent-paper-style replication attempt. Additional work is needed to verify that the preprocessing, feature selection, and model settings match the published methodology.

### Files
- `06_parent_paper_baseline.R` — Initial baseline models
- `07_parent_paper_replication.R` — Updated five-model replication
- `output/tables/baseline_comparison.csv` — Baseline results
- `output/tables/parent_paper_replication.csv` — Updated model results
- `output/tables/replication_selected_features.csv` — Selected predictors
