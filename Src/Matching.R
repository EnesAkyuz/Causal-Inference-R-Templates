# Load and clean the dataset
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRF_X8POsM2RAt2I7ogda-V8hiPBv5_csym2Tnoz051WmXUSqTlPVK8tcE5G1TaprtYU16R_aCh21bL/pub?gid=1853431303&single=true&output=csv"
causal_inference_data <- read.csv(url)
causal_inference_data <- na.omit(causal_inference_data)

# Perform matching using Propensity Score, Full, and Optimal methods
formula <- photo_id ~ as.factor(felony_laws) + white_perc + bach_perc

matched_ps <- matchit(formula, data = causal_inference_data, method = "nearest")
matched_optimal <- matchit(formula, data = causal_inference_data, method = "optimal")
matched_full <- matchit(formula, data = causal_inference_data, method = "full")

# Extract balance data for adjusted differences
extract_adjusted_data <- function(matched_object, method_name) {
  bal.tab(matched_object, un = TRUE)$Balance %>%
    rownames_to_column("Covariate") %>%
    select(Covariate, Diff.Adj) %>%
    mutate(Method = method_name)
}

# Extract unadjusted balance (shared across methods)
unadjusted_balance <- bal.tab(matched_ps, un = TRUE, weights = NULL)$Balance %>%
  rownames_to_column("Covariate") %>%
  select(Covariate, Diff.Un) %>%
  mutate(Method = "Unadjusted")

# Combine adjusted balance data for all methods
adjusted_data <- bind_rows(
  extract_adjusted_data(matched_ps, "Propensity Score"),
  extract_adjusted_data(matched_optimal, "Optimal Matching"),
  extract_adjusted_data(matched_full, "Full Matching")
)

# Merge unadjusted and adjusted data, ensure "distance" is on top
combined_love_data <- bind_rows(
  unadjusted_balance %>%
    pivot_longer(cols = Diff.Un, names_to = "Type", values_to = "Difference"),
  adjusted_data %>%
    pivot_longer(cols = Diff.Adj, names_to = "Type", values_to = "Difference")
) %>%
  mutate(Covariate = factor(
    Covariate,
    levels = c("distance", setdiff(unique(Covariate), "distance"))
  ))

# Wrap long covariate names for readability
combined_love_data <- combined_love_data %>%
  mutate(Covariate = factor(Covariate, 
                            levels = levels(Covariate),
                            labels = str_wrap(levels(Covariate), width = 25)))  # Adjust width for wrapping

# Plot with improved styling and multi-row legend
ggplot(combined_love_data, aes(x = Difference, y = Covariate, color = Method)) +
  geom_point(data = filter(combined_love_data, Type == "Diff.Un"), 
             shape = 16, size = 4, alpha = 0.7) +  # Larger unadjusted points
  geom_point(data = filter(combined_love_data, Type == "Diff.Adj"), 
             shape = 18, size = 6, alpha = 0.7) +  # '+' symbol for adjusted points
  geom_hline(yintercept = 1.5, color = "black", linetype = "solid", size = 1.2) +  # Line above "distance"
  labs(
    title = "Covariate Balance",
    x = "Mean Difference",
    y = "Covariates",
    color = "Matching Method"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5),  # Larger centered title
    legend.position = "bottom",
    legend.text = element_text(size = 12),  # Adjusted legend text size
    legend.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10),  # Adjusted y-axis text size
    axis.text.x = element_text(size = 10),  # Adjusted x-axis text size
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.margin = margin(20, 20, 40, 20)  # Increase bottom margin for the legend
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +  # Split legend into 2 rows
  scale_x_continuous(expand = expansion(mult = 0.1), limits = c(-1, 2))

# Summarize Balance Scores for Each Method
summarize_balance <- function(matched_data, method_name) {
  balance_table <- bal.tab(matched_data, un = TRUE)$Balance
  data.frame(
    Method = method_name,
    Mean_SMD_Adjusted = mean(abs(balance_table$Diff.Adj), na.rm = TRUE),
    Prop_SMD_Below_0.1 = mean(abs(balance_table$Diff.Adj) < 0.1, na.rm = TRUE),
    Prop_SMD_Below_0.25 = mean(abs(balance_table$Diff.Adj) < 0.25, na.rm = TRUE)
  )
}

balance_scores <- bind_rows(
  summarize_balance(matched_ps, "Propensity Score"),
  summarize_balance(matched_optimal, "Optimal Matching"),
  summarize_balance(matched_full, "Full Matching")
)

# Export balance summary table to HTML with adjusted margins
html_table <- stargazer(
  balance_scores,
  summary = FALSE,
  type = "html",
  title = "Summary of Covariate Balance Across Matching Methods",
  out.header = FALSE
)

# Manually adjust column spacing in the HTML
html_table <- gsub("<table", "<table style='border-spacing: 15px; border-collapse: separate;'", html_table)

# Write the updated HTML table to a file
writeLines(html_table, "balance_summary_table.html")

# Perform regression on the matched dataset (Full Matching)
matched_data_full <- match.data(matched_full)
regression_model <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) + white_perc + bach_perc, 
                       data = matched_data_full)

# Export regression results to HTML with similar adjustments
regression_html <- stargazer(
  regression_model, 
  type = "html", 
  title = "Regression Results for Full Matching", 
  out.header = FALSE
)

# Adjust column spacing in the regression table
regression_html <- gsub("<table", "<table style='border-spacing: 15px; border-collapse: separate;'", regression_html)

# Write the updated regression table to a file
writeLines(regression_html, "regression_results.html")
