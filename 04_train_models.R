# ------------------------------------------------------------------
# 04_train_models.R — Train 5 models, evaluate, export metrics
# ------------------------------------------------------------------
source("scripts/00_setup.R")
obj <- readRDS("data/processed/splits_rfe_rose.rds")

X_train <- obj$X_train; X_test <- obj$X_test
y_train <- obj$y_train; y_test <- obj$y_test
train_fac <- obj$train_fac; test_fac <- obj$test_fac

# ---- XGBoost
dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgboost::xgb.DMatrix(data = X_test,  label = y_test)
xgb_params <- list(objective = "binary:logistic", eval_metric = "auc",
                   max_depth = 8, eta = 0.1, colsample_bytree = 0.8,
                   subsample = 0.8, min_child_weight = 1, scale_pos_weight = 1)
set.seed(123)
xgb_model <- xgboost::xgb.train(params = xgb_params, data = dtrain, nrounds = 1000,
                                watchlist = list(train = dtrain, test = dtest),
                                early_stopping_rounds = 50, verbose = 0)
xgb_p <- stats::predict(xgb_model, dtest)
xgb_cls <- as.integer(xgb_p > 0.5)
xgb_cm <- caret::confusionMatrix(factor(xgb_cls, levels = c(0,1)), factor(y_test, levels = c(0,1)))
xgb_auc <- as.numeric(pROC::auc(pROC::roc(y_test, xgb_p)))

# ---- Random Forest
set.seed(123)
rf_model <- randomForest::randomForest(y ~ ., data = train_fac, ntree = 500, importance = TRUE)
rf_p <- predict(rf_model, newdata = test_fac, type = "prob")[,2]
rf_cls <- as.integer(rf_p > 0.5)
rf_cm <- caret::confusionMatrix(factor(rf_cls, levels = c(0,1)), factor(test_fac$y, levels = c(0,1)))
rf_auc <- as.numeric(pROC::auc(pROC::roc(as.numeric(as.character(test_fac$y)), rf_p)))

# ---- Logistic Regression
log_model <- stats::glm(y ~ ., data = train_fac, family = binomial())
log_p <- predict(log_model, newdata = test_fac, type = "response")
log_cls <- as.integer(log_p > 0.5)
log_cm <- caret::confusionMatrix(factor(log_cls, levels = c(0,1)), factor(test_fac$y, levels = c(0,1)))
log_auc <- as.numeric(pROC::auc(pROC::roc(as.numeric(as.character(test_fac$y)), log_p)))

# ---- LightGBM
train_mat <- as.matrix(train_fac %>% dplyr::select(-y) %>% as.data.frame())
test_mat  <- as.matrix(test_fac  %>% dplyr::select(-y) %>% as.data.frame())
lgb_train <- lightgbm::lgb.Dataset(data = train_mat, label = y_train)
lgb_valid <- lightgbm::lgb.Dataset(data = test_mat,  label = y_test)
lgb_params <- list(objective = "binary", metric = "auc", learning_rate = 0.01, num_leaves = 31, scale_pos_weight = 1)
lgb_model <- lightgbm::lgb.train(params = lgb_params, data = lgb_train, nrounds = 100,
                                 valids = list(valid = lgb_valid), early_stopping_rounds = 50, verbose = -1)
lgb_p <- predict(lgb_model, test_mat)
lgb_cls <- as.integer(lgb_p > 0.5)
lgb_cm <- caret::confusionMatrix(factor(lgb_cls, levels = c(0,1)), factor(test_fac$y, levels = c(0,1)))
lgb_auc <- as.numeric(pROC::auc(pROC::roc(y_test, lgb_p)))

# ---- SVM (RBF)
set.seed(895)
svm_model <- e1071::svm(y ~ ., data = train_fac, probability = TRUE)  # RBF default, C=1, gamma=1/p
svm_pred  <- predict(svm_model, newdata = test_fac, probability = TRUE)
svm_p     <- attr(svm_pred, "probabilities")[, "1"]
svm_cls   <- as.integer(svm_p > 0.5)
svm_cm    <- caret::confusionMatrix(factor(svm_cls, levels = c(0,1)), factor(as.numeric(as.character(test_fac$y)), levels = c(0,1)))
svm_auc   <- as.numeric(pROC::auc(pROC::roc(as.numeric(as.character(test_fac$y)), svm_p)))

# ---- Metrics table (Accuracy/Recall/Specificity/Precision/F1/AUC)
f1 <- function(ppv, sen) 2 * (ppv * sen) / (ppv + sen)
metrics_df <- data.frame(
  Model        = c("XGBoost","Random Forest","Logistic Regression","LightGBM","SVM"),
  Accuracy     = c(xgb_cm$overall["Accuracy"], rf_cm$overall["Accuracy"], log_cm$overall["Accuracy"], lgb_cm$overall["Accuracy"], svm_cm$overall["Accuracy"]),
  Sensitivity  = c(xgb_cm$byClass["Sensitivity"], rf_cm$byClass["Sensitivity"], log_cm$byClass["Sensitivity"], lgb_cm$byClass["Sensitivity"], svm_cm$byClass["Sensitivity"]),
  Specificity  = c(xgb_cm$byClass["Specificity"], rf_cm$byClass["Specificity"], log_cm$byClass["Specificity"], lgb_cm$byClass["Specificity"], svm_cm$byClass["Specificity"]),
  Precision    = c(xgb_cm$byClass["Pos Pred Value"], rf_cm$byClass["Pos Pred Value"], log_cm$byClass["Pos Pred Value"], lgb_cm$byClass["Pos Pred Value"], svm_cm$byClass["Pos Pred Value"]),
  Recall       = c(xgb_cm$byClass["Sensitivity"], rf_cm$byClass["Sensitivity"], log_cm$byClass["Sensitivity"], lgb_cm$byClass["Sensitivity"], svm_cm$byClass["Sensitivity"]),
  F1_Score     = c(
    f1(xgb_cm$byClass["Pos Pred Value"], xgb_cm$byClass["Sensitivity"]),
    f1(rf_cm$byClass["Pos Pred Value"],  rf_cm$byClass["Sensitivity"]),
    f1(log_cm$byClass["Pos Pred Value"], log_cm$byClass["Sensitivity"]),
    f1(lgb_cm$byClass["Pos Pred Value"], lgb_cm$byClass["Sensitivity"]),
    f1(svm_cm$byClass["Pos Pred Value"], svm_cm$byClass["Sensitivity"])
  ),
  AUC          = c(xgb_auc, rf_auc, log_auc, lgb_auc, svm_auc),
  row.names = NULL
)
metrics_df[,-1] <- round(metrics_df[,-1], 4)
write.csv(metrics_df, "output/tables/metrics_test.csv", row.names = FALSE)
saveRDS(list(xgb_model=xgb_model, rf_model=rf_model, log_model=log_model, lgb_model=lgb_model, svm_model=svm_model),
        "models/models_rosetrain.rds")
print(metrics_df)
