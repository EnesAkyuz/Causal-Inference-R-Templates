# Load and clean the dataset
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRF_X8POsM2RAt2I7ogda-V8hiPBv5_csym2Tnoz051WmXUSqTlPVK8tcE5G1TaprtYU16R_aCh21bL/pub?gid=1853431303&single=true&output=csv"
causal_inference_data <- read.csv(url)
causal_inference_data <- na.omit(causal_inference_data)

########### MODEL 1 ###########
# Normal Model (All Variables)
model1 <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) +
               white_perc + bach_perc,
             data = causal_inference_data)

# Add predicted values and residuals for Model 1
causal_inference_data$predicted1 <- predict(model1)
causal_inference_data$residuals1 <- residuals(model1)

# Perform Shapiro-Wilk test for Model 1 residuals
shapiro1 <- shapiro.test(causal_inference_data$residuals1)$p.value

# Residual Plot for Model 1
residual_plot1 <- ggplot(causal_inference_data, aes(x = predicted1, y = residuals1)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Model 2 Residual Plot (Base Model)",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal()

# Q-Q Plot for Residuals of Model 1
qq_plot1 <- ggplot(causal_inference_data, aes(sample = residuals1)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot (Model 2 - Base Model)",
       x = "Theoretical Quantiles",
       y = "Residuals") +
  theme_minimal()

########## MODEL 2 (OPTIMAL MATCHING) ##########
# Perform Optimal Matching
matched_outcome_optimal <- matchit(photo_id ~ as.factor(felony_laws) +
                                     white_perc + bach_perc,
                                   data = causal_inference_data,
                                   method = "optimal")

# Extract matched data
matched_df_optimal <- match.data(matched_outcome_optimal)

# Fit regression model on optimally matched data
model2 <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) +
               white_perc + bach_perc, 
             data = matched_df_optimal, 
             weights = weights)

# Add predicted values and residuals for Model 2
matched_df_optimal$predicted2 <- predict(model2)
matched_df_optimal$residuals2 <- residuals(model2)

# Perform Shapiro-Wilk test for Model 2 residuals
shapiro2 <- shapiro.test(matched_df_optimal$residuals2)$p.value

# Residual Plot for Model 2
residual_plot2 <- ggplot(matched_df_optimal, aes(x = predicted2, y = residuals2)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Model 3 Residual Plot (Optimal Matching)",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal()

# Q-Q Plot for Residuals of Model 2
qq_plot2 <- ggplot(matched_df_optimal, aes(sample = residuals2)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot (Model 3 - Optimal Matching)",
       x = "Theoretical Quantiles",
       y = "Residuals") +
  theme_minimal()

########## MODEL 3 (FULL MATCHING) ##########
# Perform Full Matching
matched_outcome_full <- matchit(photo_id ~ as.factor(felony_laws) +
                                  white_perc + bach_perc,
                                data = causal_inference_data,
                                method = "full")

# Extract matched data
matched_df_full <- match.data(matched_outcome_full)

# Fit regression model on matched data with weights
model3 <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) +
               white_perc + bach_perc, 
             data = matched_df_full, 
             weights = weights)

# Add predicted values and residuals for Model 3
matched_df_full$predicted3 <- predict(model3)
matched_df_full$residuals3 <- residuals(model3)

# Perform Shapiro-Wilk test for Model 3 residuals
shapiro3 <- shapiro.test(matched_df_full$residuals3)$p.value



# Residual Plot for Model 3
residual_plot3 <- ggplot(matched_df_full, aes(x = predicted3, y = residuals3)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Model 4 Residual Plot (Full Matching)",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal()

# Q-Q Plot for Residuals of Model 3
qq_plot3 <- ggplot(matched_df_full, aes(sample = residuals3)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot (Model 4 - Full Matching)",
       x = "Theoretical Quantiles",
       y = "Residuals") +
  theme_minimal()

########## Combine All Plots ##########
(residual_plot1 | residual_plot2 | residual_plot3) /
  (qq_plot1 | qq_plot2 | qq_plot3)
