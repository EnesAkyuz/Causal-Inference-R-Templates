# Load and clean the dataset
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRF_X8POsM2RAt2I7ogda-V8hiPBv5_csym2Tnoz051WmXUSqTlPVK8tcE5G1TaprtYU16R_aCh21bL/pub?gid=1853431303&single=true&output=csv"
causal_inference_data <- read.csv(url)
causal_inference_data <- na.omit(causal_inference_data)

########### MODEL 1 ###########
# Minimal Model with photo_id and political_lean
model1_minimal <- lm(turnout ~ as.factor(photo_id) + political_lean,
                     data = causal_inference_data)

########## EXPORT SUMMARY ##########
# Export the model summary using stargazer
library(stargazer)
stargazer(
  model1_minimal,
  type = "html",
  title = "Regression Summary for Model 1 (Minimal Variables)",
  out = "model1_minimal_summary.html",
  covariate.labels = c("Photo ID", "Political Lean"),
  dep.var.labels = "Turnout",
  digits = 3
)
