# ------------------------------------------------------------------
# 08_preprocessing_comparison.R
# Compare 4 resampling methods x 3 feature-selection methods x 5 models.
# Run from the repository root: Rscript 08_preprocessing_comparison.R
# ------------------------------------------------------------------

options(stringsAsFactors = FALSE)
set.seed(232)

required <- c("caret", "ROSE", "smotefamily", "randomForest", "e1071",
              "xgboost", "lightgbm", "pROC")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install these packages before running: ", paste(missing, collapse = ", "))
}

input_file <- "data_clean.csv"
if (!file.exists(input_file)) stop("Could not find data_clean.csv in the project root.")
output_dir <- file.path("output", "tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

data <- read.csv(input_file, check.names = TRUE)
if (!"y" %in% names(data)) stop("The input data must contain the binary outcome column 'y'.")
data <- data[complete.cases(data), , drop = FALSE]
if (!all(data$y %in% c(0, 1))) stop("Outcome y must contain only 0 and 1.")
data$y <- factor(data$y, levels = c(0, 1))
predictor_names <- setdiff(names(data), "y")
if (!all(vapply(data[predictor_names], is.numeric, logical(1)))) {
  stop("All predictors must be numeric. Encode categorical predictors before running.")
}

# Stratified 60/20/20 split. The held-out test set is not used for feature
# selection, resampling, model fitting, or choosing the winning combination.
set.seed(232)
train_idx <- caret::createDataPartition(data$y, p = 0.60, list = FALSE)
train <- data[train_idx, , drop = FALSE]
remainder <- data[-train_idx, , drop = FALSE]
set.seed(233)
valid_idx <- caret::createDataPartition(remainder$y, p = 0.50, list = FALSE)
valid <- remainder[valid_idx, , drop = FALSE]
test <- remainder[-valid_idx, , drop = FALSE]
cat("Split sizes (train / validation / test):", nrow(train), "/", nrow(valid), "/", nrow(test), "\n")
cat("Training outcome counts:\n"); print(table(train$y))

# Fit feature selection once on the original training data. No validation or
# test observations enter either selector. Select 30 predictors when possible.
selection_k <- min(30L, length(predictor_names))
feature_sets <- list(All_features = predictor_names)

set.seed(285)
rfe_control <- caret::rfeControl(functions = caret::rfFuncs, method = "cv",
                                 number = 5, verbose = FALSE)
rfe_fit <- caret::rfe(x = train[, predictor_names, drop = FALSE], y = train$y,
                      sizes = selection_k, rfeControl = rfe_control)
feature_sets$RFE <- caret::predictors(rfe_fit)

anova_scores <- vapply(predictor_names, function(feature) {
  fit <- stats::aov(train[[feature]] ~ train$y)
  summary(fit)[[1]][["F value"]][1]
}, numeric(1))
anova_scores[!is.finite(anova_scores)] <- -Inf
feature_sets$ANOVA <- names(sort(anova_scores, decreasing = TRUE))[seq_len(selection_k)]

# Resampling is applied only to the training partition after feature selection.
resample_training <- function(frame, method) {
  frame$y <- factor(frame$y, levels = c(0, 1))
  if (method == "None") return(frame)
  if (method == "ROSE") {
    set.seed(300)
    return(ROSE::ROSE(y ~ ., data = frame, seed = 300)$data)
  }
  if (method == "SMOTE") {
    set.seed(301)
    x <- frame[, setdiff(names(frame), "y"), drop = FALSE]
    smote <- smotefamily::SMOTE(X = x, target = frame$y, K = 5, dup_size = 0)
    out <- as.data.frame(smote$data)
    names(out)[ncol(out)] <- "y"
    out$y <- factor(as.character(out$y), levels = c("0", "1"))
    return(out)
  }
  if (method == "Random_oversampling") {
    set.seed(302)
    counts <- table(frame$y)
    majority_n <- max(counts)
    pieces <- lapply(levels(frame$y), function(label) {
      rows <- frame[frame$y == label, , drop = FALSE]
      rows[sample.int(nrow(rows), majority_n, replace = nrow(rows) < majority_n), , drop = FALSE]
    })
    out <- do.call(rbind, pieces)
    out$y <- factor(out$y, levels = c(0, 1))
    out[sample.int(nrow(out)), , drop = FALSE]
  } else stop("Unknown resampling method: ", method)
}

get_probability <- function(model_name, model, frame) {
  x <- as.matrix(frame[, setdiff(names(frame), "y"), drop = FALSE])
  if (model_name == "Logistic_Regression") {
    return(as.numeric(stats::predict(model, newdata = frame, type = "response")))
  }
  if (model_name == "Random_Forest") {
    return(as.numeric(stats::predict(model, newdata = frame, type = "prob")[, "1"]))
  }
  if (model_name == "SVM") {
    prediction <- stats::predict(model, newdata = frame, probability = TRUE)
    return(as.numeric(attr(prediction, "probabilities")[, "1"]))
  }
  if (model_name == "XGBoost") {
    return(as.numeric(stats::predict(model, xgboost::xgb.DMatrix(x))))
  }
  if (model_name == "LightGBM") return(as.numeric(stats::predict(model, x)))
  stop("Unknown model: ", model_name)
}

score_predictions <- function(frame, probability) {
  truth <- as.integer(as.character(frame$y))
  predicted <- as.integer(probability >= 0.5)
  tp <- sum(predicted == 1 & truth == 1)
  fp <- sum(predicted == 1 & truth == 0)
  fn <- sum(predicted == 0 & truth == 1)
  precision <- if (tp + fp == 0) 0 else tp / (tp + fp)
  recall <- if (tp + fn == 0) 0 else tp / (tp + fn)
  f1 <- if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)
  auc <- as.numeric(pROC::auc(pROC::roc(truth, probability, levels = c(0, 1),
                                        direction = "<", quiet = TRUE)))
  c(Accuracy = mean(predicted == truth), F1 = f1, AUROC = auc, Recall = recall)
}

fit_model <- function(model_name, training) {
  if (model_name == "Logistic_Regression") return(stats::glm(y ~ ., data = training, family = binomial()))
  if (model_name == "Random_Forest") {
    set.seed(401)
    return(randomForest::randomForest(y ~ ., data = training, ntree = 500))
  }
  if (model_name == "SVM") {
    set.seed(402)
    return(e1071::svm(y ~ ., data = training, kernel = "radial", probability = TRUE))
  }
  x <- as.matrix(training[, setdiff(names(training), "y"), drop = FALSE])
  label <- as.integer(as.character(training$y))
  if (model_name == "XGBoost") {
    set.seed(403)
    return(xgboost::xgb.train(
      params = list(objective = "binary:logistic", eval_metric = "auc", max_depth = 6,
                    eta = 0.05, subsample = 0.8, colsample_bytree = 0.8,
                    nthread = 1, verbosity = 0),
      data = xgboost::xgb.DMatrix(x, label = label), nrounds = 300, verbose = 0
    ))
  }
  if (model_name == "LightGBM") {
    set.seed(404)
    dataset <- lightgbm::lgb.Dataset(data = x, label = label)
    return(lightgbm::lgb.train(
      params = list(objective = "binary", metric = "auc", learning_rate = 0.05,
                    num_leaves = 31, verbosity = -1, num_threads = 1),
      data = dataset, nrounds = 300, verbose = -1
    ))
  }
  stop("Unknown model: ", model_name)
}

resampling_methods <- c("None", "ROSE", "SMOTE", "Random_oversampling")
model_names <- c("Logistic_Regression", "Random_Forest", "SVM", "XGBoost", "LightGBM")
validation_results <- list()
test_results <- list()
age_results <- list()
run <- 0L

for (feature_method in names(feature_sets)) {
  selected <- feature_sets[[feature_method]]
  for (resampling_method in resampling_methods) {
    train_subset <- train[, c(selected, "y"), drop = FALSE]
    valid_subset <- valid[, c(selected, "y"), drop = FALSE]
    test_subset <- test[, c(selected, "y"), drop = FALSE]
    sampled <- resample_training(train_subset, resampling_method)
    sampled$y <- factor(sampled$y, levels = c(0, 1))

    for (model_name in model_names) {
      run <- run + 1L
      cat(sprintf("[%02d/60] %s | %s | %s\n", run, feature_method, resampling_method, model_name))
      model <- fit_model(model_name, sampled)
      validation_probability <- get_probability(model_name, model, valid_subset)
      test_probability <- get_probability(model_name, model, test_subset)
      common <- data.frame(Model = model_name, Feature_Selection = feature_method,
                           Resampling = resampling_method, stringsAsFactors = FALSE)
      validation_results[[run]] <- cbind(common, as.data.frame(t(score_predictions(valid_subset, validation_probability))))
      test_results[[run]] <- cbind(common, as.data.frame(t(score_predictions(test_subset, test_probability))))

      # Age-specific test metrics follow the manuscript's subgroup analysis.
      if ("Age" %in% names(test_subset)) {
        age_group <- cut(test_subset$Age, breaks = c(-Inf, 39, 59, Inf),
                         labels = c("18-39", "40-59", "60+"), right = TRUE)
        for (group in levels(age_group)) {
          rows <- which(age_group == group)
          if (length(rows) && length(unique(test_subset$y[rows])) == 2) {
            age_results[[length(age_results) + 1L]] <- cbind(
              common, Age_Group = group,
              as.data.frame(t(score_predictions(test_subset[rows, , drop = FALSE], test_probability[rows])))
            )
          }
        }
      }
    }
  }
}

validation_table <- do.call(rbind, validation_results)
test_table <- do.call(rbind, test_results)
# Rank combinations on validation F1, with AUROC as the tie-breaker.
validation_table <- validation_table[order(-validation_table$F1, -validation_table$AUROC), ]
validation_table$Validation_Rank <- seq_len(nrow(validation_table))
test_table <- merge(test_table,
                    validation_table[, c("Model", "Feature_Selection", "Resampling", "Validation_Rank")],
                    by = c("Model", "Feature_Selection", "Resampling"), sort = FALSE)
test_table <- test_table[order(test_table$Validation_Rank), ]
age_table <- if (length(age_results)) do.call(rbind, age_results) else data.frame()

write.csv(validation_table, file.path(output_dir, "preprocessing_comparison_validation.csv"), row.names = FALSE)
write.csv(test_table, file.path(output_dir, "preprocessing_comparison_test.csv"), row.names = FALSE)
write.csv(data.frame(Feature_Selection = names(feature_sets),
                     Selected_Features = vapply(feature_sets, paste, collapse = "|", character(1))),
          file.path(output_dir, "preprocessing_selected_features.csv"), row.names = FALSE)
if (nrow(age_table)) write.csv(age_table, file.path(output_dir, "preprocessing_comparison_test_by_age.csv"), row.names = FALSE)

cat("\nTop 10 combinations by validation F1 (AUROC breaks ties):\n")
print(utils::head(validation_table[, c("Validation_Rank", "Model", "Feature_Selection", "Resampling", "Accuracy", "F1", "AUROC", "Recall")], 10), row.names = FALSE)
cat("\nSaved validation ranking and held-out test results under output/tables/.\n")
cat("Choose the combination using validation results; use the test table for final held-out reporting.\n")
