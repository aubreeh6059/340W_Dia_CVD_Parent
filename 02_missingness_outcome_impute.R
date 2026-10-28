# ------------------------------------------------------------------
# 02_missingness_outcome_impute.R — Missingness, outcome y, imputation, save CSV
# ------------------------------------------------------------------
source("scripts/00_setup.R")

dat <- readRDS("data/processed/nhanes_combined_raw.rds")

# Nutrient columns
nutrient_cols <- c(
  "Protein","Carbohydrates","Sugars","Fiber","Saturated_Fat","Monounsaturated_Fat",
  "Polyunsaturated_Fat","Cholesterol","Beta_Carotene","Cryptoxanthin","Lutein_Zeaxanthin",
  "Thiamin","Riboflavin","Niacin","Vitamin_B6","Folic_Acid","Food_Folate","Iron","Choline",
  "Vitamin_B12","Vitamin_C","Vitamin_D","Vitamin_E","Vitamin_K","Calcium","Phosphorus",
  "Magnesium","Zinc","Copper","Sodium","Potassium","Selenium","Theobromine","Moisture"
)

# Remove rows with any missing nutrients (complete-case for nutrients)
dat_nut <- dat[complete.cases(dat[, nutrient_cols]), ]

# Drop "9" (unknown) in CVD components, then build binary outcome y
dat_nut <- dat_nut %>%
  dplyr::filter(
    Congestive   != 9,
    Coronary     != 9,
    Heart_attack != 9,
    Stroke       != 9,
    Angina       != 9
  ) %>%
  dplyr::mutate(
    Congestive_binary   = as.integer(Congestive == 1),
    Coronary_binary     = as.integer(Coronary == 1),
    Heart_attack_binary = as.integer(Heart_attack == 1),
    Stroke_binary       = as.integer(Stroke == 1),
    Angina_binary       = as.integer(Angina == 1),
    y = as.integer(Congestive_binary | Coronary_binary | Heart_attack_binary | Stroke_binary | Angina_binary)
  )

# Keep analysis columns; remove IDs and component flags + Theobromine (per your code)
data_clean <- dat_nut %>%
  dplyr::select(
    -SEQN, -Congestive_binary, -Coronary_binary, -Heart_attack_binary,
    -Stroke_binary, -Angina_binary, -Congestive, -Coronary, -Heart_attack,
    -Stroke, -Angina, -Theobromine
  )

# Impute non-nutrient clinical vars via MICE (others already complete after filtering)
non_nutrient_cols <- c("BMI","Waist_circ","Systolic_BP","Diastolic_BP","Total_Cholesterol","C_Reactive")
set.seed(123)
imp <- mice::mice(data_clean[, non_nutrient_cols], m = 5, maxit = 50, method = "pmm", seed = 123)
data_clean[, non_nutrient_cols] <- mice::complete(imp, 1)

# Save
write.csv(data_clean, "data/processed/data_clean.csv", row.names = FALSE)
saveRDS(list(data_clean = data_clean, nutrient_cols = nutrient_cols, non_nutrient_cols = non_nutrient_cols),
        "data/processed/clean_objects.rds")
cat("Clean data saved to data/processed/data_clean.csv\n")
