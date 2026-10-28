# ------------------------------------------------------------------
# 00_setup.R  —  Packages, options, and helper paths
# ------------------------------------------------------------------
options(stringsAsFactors = FALSE)
set.seed(232)

pkgs <- c(
  "dplyr","tidyr","purrr","caret","ROSE","xgboost","lightgbm","randomForest",
  "e1071","pROC","lime","shapviz","Matrix","ggplot2","mice","haven","foreign"
)
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

# Project folders (create if missing)
dirs <- c("data/raw","data/processed","figures/main","figures/supplement","output/tables","models")
for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
