# ------------------------------------------------------------------
# 01_load_merge_nhanes.R  —  Read XPTs, harmonize, merge (2017–2020, 2021–2023)
# ------------------------------------------------------------------
source("00_setup.R")

# Helper to read XPT robustly
read_xpt <- function(path) {
  # prefer haven::read_xpt; fallback to foreign::read.xport
  if (file.exists(path)) {
    tryCatch(haven::read_xpt(path), error = function(e) foreign::read.xport(path))
  } else stop("File not found: ", path)
}

# ---------------- Pre-pandemic (2017–2020; suffix "P_") ----------------
diet_17_20 <- read_xpt("data/raw/P_DR1TOT.xpt") %>%
  dplyr::select(
    SEQN,
    Protein = DR1TPROT, Carbohydrates = DR1TCARB, Sugars = DR1TSUGR,
    Fiber = DR1TFIBE, Saturated_Fat = DR1TSFAT, Monounsaturated_Fat = DR1TMFAT,
    Polyunsaturated_Fat = DR1TPFAT, Cholesterol = DR1TCHOL, Beta_Carotene = DR1TBCAR,
    Cryptoxanthin = DR1TCRYP, Lutein_Zeaxanthin = DR1TLZ, Thiamin = DR1TVB1,
    Riboflavin = DR1TVB2, Niacin = DR1TNIAC, Vitamin_B6 = DR1TVB6,
    Folic_Acid = DR1TFA, Food_Folate = DR1TFF, Iron = DR1TFDFE, Choline = DR1TCHL,
    Vitamin_B12 = DR1TVB12, Vitamin_C = DR1TVC, Vitamin_D = DR1TVD,
    Vitamin_E = DR1TATOC, Vitamin_K = DR1TVK, Calcium = DR1TCALC,
    Phosphorus = DR1TPHOS, Magnesium = DR1TMAGN, Zinc = DR1TZINC,
    Copper = DR1TCOPP, Sodium = DR1TSODI, Potassium = DR1TPOTA,
    Selenium = DR1TSELE, Theobromine = DR1TTHEO, Moisture = DR1TMOIS
  )

heart_17_20 <- read_xpt("data/raw/P_MCQ.xpt") %>%
  dplyr::select(SEQN, Congestive = MCQ160B, Coronary = MCQ160C,
                Heart_attack = MCQ160D, Stroke = MCQ160F, Angina = MCQ160E)

demo_17_20  <- read_xpt("data/raw/P_DEMO.xpt") %>% dplyr::select(SEQN, Age = RIDAGEYR)

bmx_17_20 <- read_xpt("data/raw/P_BMX.xpt") %>%
  dplyr::select(SEQN, BMI = BMXBMI, Waist_circ = BMXWAIST)

bpx_17_20 <- read_xpt("data/raw/P_BPXO.xpt") %>%
  dplyr::select(SEQN, Systolic_BP = BPXOSY1, Diastolic_BP = BPXODI1)

tchol_17_20 <- read_xpt("data/raw/P_TCHOL.xpt") %>%
  dplyr::select(SEQN, Total_Cholesterol = LBXTC)

hscrp_17_20 <- read_xpt("data/raw/P_HSCRP.xpt") %>%
  dplyr::select(SEQN, C_Reactive = LBXHSCRP)

# Combine 2017–2020
merged_17_20 <- purrr::reduce(
  list(diet_17_20, heart_17_20, demo_17_20, bmx_17_20, bpx_17_20, tchol_17_20, hscrp_17_20),
  dplyr::full_join, by = "SEQN"
)

# ---------------- Post-period (2021–2023; suffix "_L") ----------------
diet_21_23 <- read_xpt("data/raw/DR1TOT_L.xpt") %>%
  dplyr::select(
    SEQN,
    Protein = DR1TPROT, Carbohydrates = DR1TCARB, Sugars = DR1TSUGR,
    Fiber = DR1TFIBE, Saturated_Fat = DR1TSFAT, Monounsaturated_Fat = DR1TMFAT,
    Polyunsaturated_Fat = DR1TPFAT, Cholesterol = DR1TCHOL, Beta_Carotene = DR1TBCAR,
    Cryptoxanthin = DR1TCRYP, Lutein_Zeaxanthin = DR1TLZ, Thiamin = DR1TVB1,
    Riboflavin = DR1TVB2, Niacin = DR1TNIAC, Vitamin_B6 = DR1TVB6,
    Folic_Acid = DR1TFA, Food_Folate = DR1TFF, Iron = DR1TFDFE, Choline = DR1TCHL,
    Vitamin_B12 = DR1TVB12, Vitamin_C = DR1TVC, Vitamin_D = DR1TVD,
    Vitamin_E = DR1TATOC, Vitamin_K = DR1TVK, Calcium = DR1TCALC,
    Phosphorus = DR1TPHOS, Magnesium = DR1TMAGN, Zinc = DR1TZINC,
    Copper = DR1TCOPP, Sodium = DR1TSODI, Potassium = DR1TPOTA,
    Selenium = DR1TSELE, Theobromine = DR1TTHEO, Moisture = DR1TMOIS
  )

heart_21_23 <- read_xpt("data/raw/MCQ_L.xpt") %>%
  dplyr::select(SEQN, Congestive = MCQ160B, Coronary = MCQ160C,
                Heart_attack = MCQ160D, Stroke = MCQ160F, Angina = MCQ160E)

demo_21_23  <- read_xpt("data/raw/DEMO_L.xpt") %>% dplyr::select(SEQN, Age = RIDAGEYR)

bmx_21_23 <- read_xpt("data/raw/BMX_L.xpt") %>%
  dplyr::select(SEQN, BMI = BMXBMI, Waist_circ = BMXWAIST)

bpx_21_23 <- read_xpt("data/raw/BPXO_L.xpt") %>%
  dplyr::select(SEQN, Systolic_BP = BPXOSY1, Diastolic_BP = BPXODI1)

tchol_21_23 <- read_xpt("data/raw/TCHOL_L.xpt") %>%
  dplyr::select(SEQN, Total_Cholesterol = LBXTC)

hscrp_21_23 <- read_xpt("data/raw/HSCRP_L.xpt") %>%
  dplyr::select(SEQN, C_Reactive = LBXHSCRP)

# Combine 2021–2023
merged_21_23 <- purrr::reduce(
  list(diet_21_23, heart_21_23, demo_21_23, bmx_21_23, bpx_21_23, tchol_21_23, hscrp_21_23),
  dplyr::full_join, by = "SEQN"
)

# Stack periods and inspect
nhanes_combined <- dplyr::bind_rows(merged_17_20, merged_21_23)
saveRDS(nhanes_combined, "data/processed/nhanes_combined_raw.rds")
cat("Combined dimensions:", paste(dim(nhanes_combined), collapse = " x "), "\n")
