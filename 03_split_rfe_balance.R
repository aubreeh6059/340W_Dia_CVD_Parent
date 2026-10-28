# ------------------------------------------------------------------
# 03_split_rfe_balance.R — Train/test split, RFE, ROSE balance, matrices
# ------------------------------------------------------------------
source("scripts/00_setup.R")

df <- read.csv("data/processed/data_clean.csv")
df$y <- as.factor(df$y)

# Split
set.seed(232)
idx <- caret::createDataPartition(df$y, p = 0.75, list = FALSE)
trainData <- df[idx, ]
testData  <- df[-idx, ]

# RFE (RF-based, 5-fold CV)
ctrl <- caret::rfeControl(
  functions = caret::rfFuncs,
  method = "cv",
  number = 5,
  verbose = TRUE
)

set.seed(285)
rfe_results <- caret::rfe(
  x = trainData[, -ncol(trainData)],
  y = trainData$y,
  sizes = c(10, 20, 30),
  rfeControl = ctrl
)
selected_features <- caret::predictors(rfe_results)
cat("Selected features (RFE):", paste(selected_features, collapse = ", "), "\n")

# Subset to selected + outcome
trainData <- trainData[, c(selected_features, "y")]
testData  <- testData[, c(selected_features, "y")]

# ROSE balance on training only
set.seed(123)
train_bal <- ROSE::ROSE(y ~ ., data = trainData, seed = 75)$data
cat("ROSE train counts:\n"); print(table(train_bal$y))

# Numeric matrices for xgb/lgbm
train_num <- train_bal; test_num <- testData
train_num$y <- as.numeric(as.character(train_num$y))
test_num$y  <- as.numeric(as.character(test_num$y))
X_train <- as(Matrix::sparse.model.matrix(y ~ . - 1, data = train_num), "dgCMatrix")
X_test  <- as(Matrix::sparse.model.matrix(y ~ . - 1, data = test_num), "dgCMatrix")
y_train <- train_num$y; y_test <- test_num$y

# Factor versions for RF/GLM/SVM
train_fac <- train_bal; train_fac$y <- as.factor(train_fac$y)
test_fac  <- testData;  test_fac$y  <- as.factor(test_fac$y)

# Save everything for reuse
saveRDS(list(
  rfe_results = rfe_results, selected_features = selected_features,
  train_num = train_num, test_num = test_num, X_train = X_train, X_test = X_test,
  y_train = y_train, y_test = y_test, train_fac = train_fac, test_fac = test_fac
), "data/processed/splits_rfe_rose.rds")
