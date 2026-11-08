# DS 340W -- Reproducible NHANES CVD baseline

set.seed(232)
required <- c("caret", "ROSE", "randomForest", "e1071", "xgboost", "pROC")
missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Install required packages first: install.packages(c(",
       paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "))")
}

# Locate the cleaned dataset in either of the locations used in the repository.
candidates <- c("data_clean.csv", "data/processed/data_clean.csv")
path <- candidates[file.exists(candidates)][1]
if (is.na(path)) stop("Cannot find data_clean.csv. Run from the repository root folder.")
df <- read.csv(path, check.names = TRUE)
if (!"y" %in% names(df)) stop("The dataset needs a binary outcome column named y.")
if (!all(na.omit(df$y) %in% c(0, 1))) stop("Expected y coded as 0 or 1.")
df <- df[!is.na(df$y), , drop = FALSE]
df$y <- factor(df$y, levels = c(0, 1))
feature_cols <- setdiff(names(df), "y")
if (!all(vapply(df[feature_cols], is.numeric, logical(1)))) {
  stop("This script expects numeric input predictors. Check the CSV columns.")
}

# Preserve an untouched held-out test set. ~70% train, ~15% validation, ~15% test.
set.seed(232)
train_ids <- caret::createDataPartition(df$y, p = 0.70, list = FALSE)
train <- df[train_ids, , drop = FALSE]
remaining <- df[-train_ids, , drop = FALSE]
val_ids <- caret::createDataPartition(remaining$y, p = 0.5, list = FALSE)
valid <- remaining[val_ids, , drop = FALSE]
test <- remaining[-val_ids, , drop = FALSE]

# Fit preprocessing on TRAIN only, so validation/test information does not leak.
medians <- vapply(train[feature_cols], function(x) median(x, na.rm = TRUE), numeric(1))
medians[!is.finite(medians)] <- 0
impute_numeric <- function(dat) {
  for (col in feature_cols) {
    dat[[col]][!is.finite(dat[[col]])] <- NA_real_
    dat[[col]][is.na(dat[[col]])] <- medians[[col]]
  }
  dat
}
train <- impute_numeric(train)
valid <- impute_numeric(valid)
test <- impute_numeric(test)

cat("Data rows:", nrow(df), "| Predictors:", length(feature_cols), "\n")
cat("Train:", nrow(train), "Validation:", nrow(valid), "Test:", nrow(test), "\n")
cat("Outcome counts (entire dataset):\n")
print(table(df$y))

# ROSE is applied ONLY to training data. Validation/test remain unbalanced.
set.seed(232)
train_bal <- ROSE::ROSE(y ~ ., data = train, N = nrow(train), seed = 232)$data
train_bal$y <- factor(train_bal$y, levels = c("0", "1"))
cat("ROSE training class counts:\n")
print(table(train_bal$y))

# Test matrix uses exactly the same predictor columns and column ordering.
xmat <- function(d) as.matrix(d[, feature_cols, drop = FALSE])
y01 <- function(d) as.integer(as.character(d$y))

# Fit the four parent-paper model families available in the repository.
# (The parent paper's ANN is not implemented in the current repository.)
models <- list()
predictions <- list()

cat("Training logistic regression...\n")
models$Logistic_Regression <- stats::glm(y ~ ., data = train_bal, family = binomial())
predictions$Logistic_Regression <- as.numeric(
  stats::predict(models$Logistic_Regression, newdata = test, type = "response"))

cat("Training random forest...\n")
set.seed(232)
models$Random_Forest <- randomForest::randomForest(y ~ ., data = train_bal, ntree = 250)
predictions$Random_Forest <- as.numeric(
  stats::predict(models$Random_Forest, newdata = test, type = "prob")[, "1"])

cat("Training SVM...\n")
set.seed(232)
models$SVM <- e1071::svm(y ~ ., data = train_bal, kernel = "radial", probability = TRUE)
svm_pred <- stats::predict(models$SVM, newdata = test, probability = TRUE)
predictions$SVM <- as.numeric(attr(svm_pred, "probabilities")[, "1"])

cat("Training XGBoost, with early stopping on VALIDATION (not test)...\n")
dtrain <- xgboost::xgb.DMatrix(xmat(train_bal), label = y01(train_bal))
dvalid <- xgboost::xgb.DMatrix(xmat(valid), label = y01(valid))
dtest <- xgboost::xgb.DMatrix(xmat(test), label = y01(test))
set.seed(232)
models$XGBoost <- xgboost::xgb.train(
  params = list(objective = "binary:logistic", eval_metric = "auc",
                max_depth = 5, eta = 0.05, subsample = 0.8,
                colsample_bytree = 0.8, nthread = 4),
  data = dtrain, nrounds = 600,
  watchlist = list(train = dtrain, validation = dvalid),
  early_stopping_rounds = 30, verbose = 0
)
predictions$XGBoost <- as.numeric(stats::predict(models$XGBoost, dtest))
cat("XGBoost best iteration:", models$XGBoost$best_iteration, "\n")

# Explicitly evaluate minority/CVD-positive class ('1'). A paper's F1 score
# might be a different averaging convention -- check before comparing.
score <- function(model_name, p, truth = test$y) {
  if (any(!is.finite(p))) stop("Non-finite predictions from ", model_name)
  p <- pmin(1, pmax(0, p))
  observed <- as.character(truth)
  predicted <- ifelse(p >= 0.5, "1", "0")
  TP <- sum(predicted == "1" & observed == "1")
  TN <- sum(predicted == "0" & observed == "0")
  FP <- sum(predicted == "1" & observed == "0")
  FN <- sum(predicted == "0" & observed == "1")
  div <- function(a, b) if (b == 0) NA_real_ else a / b
  precision <- div(TP, TP + FP)
  recall <- div(TP, TP + FN)
  f1 <- if (is.na(precision) || is.na(recall) || precision + recall == 0) {
    NA_real_
  } else 2 * precision * recall / (precision + recall)
  auc <- as.numeric(pROC::auc(pROC::roc(
    response = factor(observed, levels = c("0", "1")),
    predictor = p, levels = c("0", "1"), direction = "<", quiet = TRUE)))
  data.frame(Model = model_name, Accuracy = (TP + TN) / length(truth),
             Precision_Positive = precision, Recall_Positive = recall,
             F1_Positive = f1, AUROC = auc,
             TP = TP, TN = TN, FP = FP, FN = FN)
}
metrics <- do.call(rbind, lapply(names(predictions), function(name) {
  score(name, predictions[[name]])
}))
rownames(metrics) <- NULL
out_dir <- file.path("output", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
outfile <- file.path(out_dir, "baseline_comparison.csv")
write.csv(metrics, outfile, row.names = FALSE)
print(metrics, row.names = FALSE, digits = 4)
cat("Saved: ", outfile, "\n", sep = "")

# Save models and split information to make these results reproducible.
dir.create("models", showWarnings = FALSE)
saveRDS(list(models = models, feature_cols = feature_cols,
             train_row_count = nrow(train), validation_row_count = nrow(valid),
             test_row_count = nrow(test), seed = 232),
        "models/baseline_models.rds")
