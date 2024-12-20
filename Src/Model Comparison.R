# Load and clean the dataset
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRF_X8POsM2RAt2I7ogda-V8hiPBv5_csym2Tnoz051WmXUSqTlPVK8tcE5G1TaprtYU16R_aCh21bL/pub?gid=1853431303&single=true&output=csv"
causal_inference_data <- read.csv(url)
causal_inference_data <- na.omit(causal_inference_data)


# Ensure felony_laws is treated as a factor with consistent levels
causal_inference_data$felony_laws <- factor(
  causal_inference_data$felony_laws,
  levels = c("No restriction", "Prison only", "Prison, parole",
             "Prison, parole, probation", "Prison, parole, probation, post-sentence")
)

########## MODEL 2 ##########
# Full model with all covariates (Unmatched)
model2 <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) +
               white_perc + bach_perc,
             data = causal_inference_data)

########## MODEL 3 (OPTIMAL MATCHING) ##########
# Perform Optimal Matching
matched_outcome_optimal <- matchit(photo_id ~ as.factor(felony_laws) +
                                     white_perc + bach_perc,
                                   data = causal_inference_data, method = "optimal")
matched_df_optimal <- match.data(matched_outcome_optimal)

# Regression Model 3 on optimally matched data
model3 <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) +
               white_perc + bach_perc,
             data = matched_df_optimal)

########## MODEL 4 (FULL MATCHING) ##########
# Perform Full Matching
matched_outcome_full <- matchit(photo_id ~ as.factor(felony_laws) +
                                  white_perc + bach_perc,
                                data = causal_inference_data, method = "full")
matched_df_full <- match.data(matched_outcome_full)

# Regression Model 4 on fully matched data
model4 <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) +
               white_perc + bach_perc,
             data = matched_df_full, weights = weights)

########## EXPORT TABLE ##########
# Combine model summaries into a single table
stargazer(model2, model3, model4,
          type = "html",  # For console output, use "html" or "latex" for other formats
          title = "Summary Table of Models",
          align = TRUE,
          column.labels = c("Model 2: All Variables", "Model 3: Optimal Matching", "Model 4: Full Matching"),
          digits = 3,
          out = "model_summary.html")

# Cohen's f^2 calculation
calculate_f2 <- function(model) {
  r_squared <- summary(model)$r.squared
  f2 <- r_squared / (1 - r_squared)
  return(f2)
}

# Model Effect Sizes
f2_model2 <- calculate_f2(model2)
f2_model3 <- calculate_f2(model3)
f2_model4 <- calculate_f2(model4)

cat("Cohen's f^2 for Model 2 (Unmatched):", round(f2_model2, 3), "\n")
cat("Cohen's f^2 for Model 3 (Optimal Matching):", round(f2_model3, 3), "\n")
cat("Cohen's f^2 for Model 4 (Full Matching):", round(f2_model4, 3), "\n")

# Step 1: Extract coefficients for the treatment variable
treatment_effect_model2 <- coef(model2)["as.factor(photo_id)1"]
treatment_effect_model3 <- coef(model3)["as.factor(photo_id)1"]
treatment_effect_model4 <- coef(model4)["as.factor(photo_id)1"]

# Step 2: Compute pooled standard deviation for turnout
calculate_pooled_sd <- function(data) {
  treated_sd <- sd(data$turnout[data$photo_id == 1], na.rm = TRUE)
  control_sd <- sd(data$turnout[data$photo_id == 0], na.rm = TRUE)
  pooled_sd <- sqrt((treated_sd^2 + control_sd^2) / 2)
  return(pooled_sd)
}

pooled_sd_model2 <- calculate_pooled_sd(causal_inference_data)
pooled_sd_model3 <- calculate_pooled_sd(matched_df_optimal)
pooled_sd_model4 <- calculate_pooled_sd(matched_df_full)

# Step 3: Compute Cohen's d for each model
cohen_d_model2 <- treatment_effect_model2 / pooled_sd_model2
cohen_d_model3 <- treatment_effect_model3 / pooled_sd_model3
cohen_d_model4 <- treatment_effect_model4 / pooled_sd_model4

# Display results
cat("Cohen's d for Model 2 (Unmatched):", round(cohen_d_model2, 3), "\n")
cat("Cohen's d for Model 3 (Optimal Matching):", round(cohen_d_model3, 3), "\n")
cat("Cohen's d for Model 4 (Full Matching):", round(cohen_d_model4, 3), "\n")

