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


