url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRF_X8POsM2RAt2I7ogda-V8hiPBv5_csym2Tnoz051WmXUSqTlPVK8tcE5G1TaprtYU16R_aCh21bL/pub?gid=1853431303&single=true&output=csv"
causal_inference_data <- read.csv(url)
causal_inference_data <- na.omit(causal_inference_data)

# Perform Optimal Matching
matched_outcome_optimal <- matchit(photo_id ~ as.factor(felony_laws) +
                                     white_perc + bach_perc,
                                   data = causal_inference_data,
                                   method = "optimal")

# Extract matched data
matched_df_optimal <- match.data(matched_outcome_optimal)

# Define the regression model using matched data
model3 <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) +
               white_perc + bach_perc, data = matched_df_optimal)

# Calculate VIF (GVIF) for the optimal matching model
vif_optimal <- vif(model3)

# Convert GVIF output to a data frame
vif_table <- data.frame(
  GVIF = round(vif_optimal[, "GVIF"], 6),  # Extract GVIF
  Df = vif_optimal[, "Df"],               # Extract Degrees of Freedom
  `GVIF^(1/(2*Df))` = round(vif_optimal[, "GVIF^(1/(2*Df))"], 6)  # Transform GVIF
)

# Output VIF (GVIF) table using Stargazer
stargazer(
  vif_table,
  type = "html", # Output to HTML
  summary = FALSE,
  title = "Generalized Variance Inflation Factor (GVIF) Table - Optimal Matching Model",
  out = "gvif_optimal_matching.html" # Save table to an HTML file
)

# Inform user about success
cat("GVIF table successfully saved as 'gvif_optimal_matching.html'.\n")
