# ------------------------------------------------------------------
# 05_explainability.R — SHAP (shapviz) and LIME (xgboost)
# ------------------------------------------------------------------
source("scripts/00_setup.R")
obj <- readRDS("data/processed/splits_rfe_rose.rds")
models <- readRDS("models/models_rosetrain.rds")

xgb_model <- models$xgb_model
X_train <- obj$X_train; X_test <- obj$X_test
y_test  <- obj$y_test
X_test_df <- as.data.frame(as.matrix(obj$test_num %>% dplyr::select(-y)))

# ---- SHAP (beeswarm)
X_train_dense <- as.matrix(X_train)
sv <- shapviz::shapviz(xgb_model, X = X_train_dense)
p_shap <- shapviz::sv_importance(sv, kind = "beeswarm") +
  ggplot2::labs(title = "SHAP Summary Plot", x = "SHAP value", y = NULL)
ggplot2::ggsave("figures/main/shap_beeswarm.png", p_shap, width = 7, height = 5, dpi = 300)

# ---- LIME for XGBoost
# Tell lime how to use xgboost
model_type.xgb.Booster <- function(x, ...) "classification"
predict_model.xgb.Booster <- function(x, newdata, type, ...) {
  preds <- stats::predict(x, as.matrix(newdata), ...)
  if (type == "raw") return(ifelse(preds > 0.5, "1", "0"))
  data.frame("0" = 1 - preds, "1" = preds)
}

explainer <- lime::lime(X_test_df, xgb_model)

# Pick borderline cases (prob ~ 0.5) to mirror your manuscript figures
p_test <- as.numeric(stats::predict(xgb_model, X_test))
ix <- order(abs(p_test - 0.5))[1:10]
explanation <- lime::explain(X_test_df[ix, ], explainer, n_features = 5, n_labels = 1)
png("figures/main/lime_borderline.png", width = 1200, height = 800, res = 150)
print(lime::plot_features(explanation))
dev.off()
