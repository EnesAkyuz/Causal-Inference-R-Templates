# Load and clean the dataset
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRF_X8POsM2RAt2I7ogda-V8hiPBv5_csym2Tnoz051WmXUSqTlPVK8tcE5G1TaprtYU16R_aCh21bL/pub?gid=1853431303&single=true&output=csv"
causal_inference_data <- read.csv(url)
causal_inference_data <- na.omit(causal_inference_data)

# Filter numeric variables, excluding categorical ones like photo_id and county_fips
numeric_stats <- causal_inference_data %>%
  select(-county_fips, -photo_id, -felony_laws) %>%  # Exclude non-numeric or categorical columns
  select_if(is.numeric) %>%  # Ensure only numeric columns are selected
  psych::describe() %>%
  as.data.frame() %>%
  select(vars, n, mean, sd, min, max, median, range) %>%
  rename(
    Variable = vars, 
    N = n, 
    Mean = mean, 
    SD = sd, 
    Min = min, 
    Max = max, 
    Median = median, 
    Range = range
  )

# Export numeric descriptive statistics to HTML using stargazer
stargazer(
  numeric_stats,
  type = "html",
  title = "Descriptive Statistics for Quantitative Numeric Variables",
  summary = FALSE,
  digits = 3,
  out = "numeric_stats.html"
)
