# DS 340W — Parent Paper Replication
# NHANES 2017–2023
# RFE (30 features) + ROSE + 5 ML Models

required <- c(
  "caret", "ROSE", "randomForest", "e1071",
  "xgboost", "lightgbm", "pROC"
)

missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing)) {
  stop("Install missing packages: ", paste(missing, collapse = ", "))
}

# --------------------------------------------------
# 1. LOAD DATA
# --------------------------------------------------

set.seed(232)

df <- read.csv("data_clean.csv", check.names = TRUE)

stopifnot("y" %in% names(df))
stopifnot(all(na.omit(unique(df$y)) %in% c(0, 1)))

df <- df[complete.cases(df), , drop = FALSE]
df$y <- factor(df$y, levels = c(0, 1))

cat("Rows:", nrow(df), "| Predictors:", ncol(df) - 1, "\n")
cat("Original class distribution:\n")
print(table(df$y))

cat("Original class percentages:\n")
print(round(prop.table(table(df$y)) * 100, 2))

# --------------------------------------------------
# 2. TRAIN / TEST SPLIT (80/20)
# --------------------------------------------------

set.seed(232)

train_idx <- caret::createDataPartition(
  df$y,
  p = 0.80,
  list = FALSE
)

train <- df[train_idx, , drop = FALSE]
test <- df[-train_idx, , drop = FALSE]

cat("Train:", nrow(train), "| Test:", nrow(test), "\n")

# --------------------------------------------------
# 3. RECURSIVE FEATURE ELIMINATION
# --------------------------------------------------

cat("Running RFE (30 features, 5-fold CV repeated 5 times)...\n")

rfe_ctrl <- caret::rfeControl(
  functions = caret::rfFuncs,
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  verbose = FALSE
)

set.seed(285)

rfe_fit <- caret::rfe(
  x = train[, setdiff(names(train), "y"), drop = FALSE],
  y = train$y,
  sizes = 30,
  rfeControl = rfe_ctrl
)

# Select exactly 30 predictors from RFE's final model
features <- caret::predictors(rfe_fit)
stopifnot(length(features) == 30)
stopifnot(all(features %in% names(train)))

cat(
  "Selected", length(features), "features:\n",
  paste(features, collapse = ", "), "\n"
)

train <- train[, c(features, "y"), drop = FALSE]
test <- test[, c(features, "y"), drop = FALSE]

# --------------------------------------------------
# 4. INTERNAL VALIDATION SPLIT
# --------------------------------------------------

set.seed(232)

sub_idx <- caret::createDataPartition(
  train$y,
  p = 0.85,
  list = FALSE
)

subtrain <- train[sub_idx, , drop = FALSE]
valid <- train[-sub_idx, , drop = FALSE]

# --------------------------------------------------
# 5. ROSE CLASS BALANCING
# --------------------------------------------------

set.seed(75)

balanced_subtrain <- ROSE::ROSE(
  y ~ .,
  data = subtrain,
  seed = 75
)$data

balanced <- ROSE::ROSE(
  y ~ .,
  data = train,
  seed = 75
)$data

balanced_subtrain$y <- factor(
  balanced_subtrain$y, levels = c(0, 1)
)

balanced$y <- factor(balanced$y, levels = c(0, 1))

cat("ROSE training class counts:\n")
print(table(balanced$y))

# --------------------------------------------------
# 6. PREPARE MATRICES
# --------------------------------------------------

make_x <- function(d) {
  x <- as.matrix(d[, features, drop = FALSE])
  storage.mode(x) <- "double"
  return(x)
}

X_train <- make_x(balanced)
y_train <- as.integer(as.character(balanced$y))

X_subtrain <- make_x(balanced_subtrain)
y_subtrain <- as.integer(as.character(balanced_subtrain$y))

X_valid <- make_x(valid)
y_valid <- as.integer(as.character(valid$y))

X_test <- make_x(test)
y_test <- as.integer(as.character(test$y))

# --------------------------------------------------
# 7. EVALUATION FUNCTION
# --------------------------------------------------

score <- function(name, prob) {
  
  prob <- as.numeric(prob)
  
  if (length(prob) != length(y_test) ||
      any(!is.finite(prob))) {
    stop("Invalid predictions from ", name)
  }
  
  pred <- as.integer(prob >= 0.5)
  
  TP <- sum(pred == 1 & y_test == 1)
  TN <- sum(pred == 0 & y_test == 0)
  FP <- sum(pred == 1 & y_test == 0)
  FN <- sum(pred == 0 & y_test == 1)
  
  accuracy <- (TP + TN) / length(y_test)
  
  precision <- if (TP + FP > 0) {
    TP / (TP + FP)
  } else {
    NA_real_
  }
  
  recall <- if (TP + FN > 0) {
    TP / (TP + FN)
  } else {
    NA_real_
  }
  
  f1 <- if (
    !is.na(precision) &&
    !is.na(recall) &&
    precision + recall > 0
  ) {
    2 * precision * recall / (precision + recall)
  } else {
    NA_real_
  }
  
  auc <- as.numeric(
    pROC::auc(
      pROC::roc(
        y_test,
        prob,
        levels = c(0, 1),
        direction = "<",
        quiet = TRUE
      )
    )
  )
  
  data.frame(
    Model = name,
    Accuracy = accuracy,
    Precision_Positive = precision,
    Recall_Positive = recall,
    F1_Positive = f1,
    AUROC = auc,
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN
  )
}

results <- list()

# --------------------------------------------------
# 8. LOGISTIC REGRESSION
# --------------------------------------------------

cat("Training Logistic Regression...\n")

glm_fit <- glm(
  y ~ .,
  data = balanced,
  family = binomial()
)

glm_prob <- predict(
  glm_fit,
  newdata = test,
  type = "response"
)

results[[1]] <- score("Logistic Regression", glm_prob)

# --------------------------------------------------
# 9. RANDOM FOREST
# --------------------------------------------------

cat("Training Random Forest...\n")

set.seed(123)

rf_fit <- randomForest::randomForest(
  y ~ .,
  data = balanced,
  ntree = 500
)

rf_prob <- predict(
  rf_fit,
  newdata = test,
  type = "prob"
)[, "1"]

results[[2]] <- score("Random Forest", rf_prob)

# --------------------------------------------------
# 10. SUPPORT VECTOR MACHINE
# --------------------------------------------------

cat("Training SVM...\n")

set.seed(895)

svm_fit <- e1071::svm(
  y ~ .,
  data = balanced,
  probability = TRUE
)

svm_pred <- predict(
  svm_fit,
  newdata = test,
  probability = TRUE
)

svm_prob <- attr(svm_pred, "probabilities")[, "1"]

results[[3]] <- score("SVM", svm_prob)

# --------------------------------------------------
# 11. XGBOOST
# --------------------------------------------------

cat("Training XGBoost...\n")

dtrain <- xgboost::xgb.DMatrix(
  X_subtrain,
  label = y_subtrain
)

dvalid <- xgboost::xgb.DMatrix(
  X_valid,
  label = y_valid
)

dtest <- xgboost::xgb.DMatrix(X_test)

set.seed(123)

xgb_fit <- xgboost::xgb.train(
  params = list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 8,
    eta = 0.1,
    colsample_bytree = 0.8,
    subsample = 0.8,
    min_child_weight = 1
  ),
  data = dtrain,
  nrounds = 1000,
  evals = list(
    train = dtrain,
    validation = dvalid
  ),
  early_stopping_rounds = 50,
  verbose = 0
)

best_rounds <- as.integer(xgboost::xgb.attributes(xgb_fit)$best_iteration)

if (is.null(best_rounds) || best_rounds < 1) {
  stop("XGBoost did not return a valid best iteration.")
}

dfull <- xgboost::xgb.DMatrix(
  X_train,
  label = y_train
)

set.seed(123)

xgb_fit <- xgboost::xgb.train(
  params = list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 8,
    eta = 0.1,
    colsample_bytree = 0.8,
    subsample = 0.8,
    min_child_weight = 1
  ),
  data = dfull,
  nrounds = best_rounds,
  verbose = 0
)

cat("XGBoost final training completed.\n")

xgb_prob <- predict(xgb_fit, dtest)

results[[4]] <- score("XGBoost", xgb_prob)

# --------------------------------------------------
# 12. LIGHTGBM
# --------------------------------------------------

cat("Training LightGBM...\n")

lgb_train <- lightgbm::lgb.Dataset(
  X_subtrain,
  label = y_subtrain
)

lgb_valid <- lightgbm::lgb.Dataset(
  X_valid,
  label = y_valid
)

set.seed(123)

lgb_fit <- lightgbm::lgb.train(
  params = list(
    objective = "binary",
    metric = "auc",
    learning_rate = 0.01,
    num_leaves = 31,
    scale_pos_weight = 1,
    verbosity = -1
  ),
  data = lgb_train,
  nrounds = 1000,
  valids = list(validation = lgb_valid),
  early_stopping_rounds = 50,
  verbose = -1
)

best_lgb_rounds <- lgb_fit$best_iter

if (is.null(best_lgb_rounds) || best_lgb_rounds < 1) {
  stop("LightGBM did not return a valid best iteration.")
}

lgb_full <- lightgbm::lgb.Dataset(
  X_train,
  label = y_train
)

set.seed(123)

lgb_fit <- lightgbm::lgb.train(
  params = list(
    objective = "binary",
    metric = "auc",
    learning_rate = 0.01,
    num_leaves = 31,
    scale_pos_weight = 1,
    verbosity = -1
  ),
  data = lgb_full,
  nrounds = best_lgb_rounds,
  verbose = -1
)

lgb_prob <- predict(lgb_fit, X_test)

results[[5]] <- score("LightGBM", lgb_prob)

# --------------------------------------------------
# 13. SAVE RESULTS
# --------------------------------------------------

metrics <- do.call(rbind, results)

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  metrics,
  "output/tables/parent_paper_replication.csv",
  row.names = FALSE
)

write.csv(
  data.frame(feature = features),
  "output/tables/replication_selected_features.csv",
  row.names = FALSE
)

cat("\nFINAL MODEL RESULTS:\n")

print(
  metrics,
  row.names = FALSE,
  digits = 4
)

cat("\nSaved: output/tables/parent_paper_replication.csv\n")
cat("Saved: output/tables/replication_selected_features.csv\n")
cat("Replication run completed!\n")