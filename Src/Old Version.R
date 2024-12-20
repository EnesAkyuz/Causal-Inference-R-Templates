install.packages("tidyr")
install.packages("corrplot")
install.packages("fastDummies")
install.packages("rgenoud")
install.packages("cobalt")
install.packages("MatchIt")
install.packages("WeightIt")
install.packages("stargazer")
install.packages("car")
install.packages("ggplot2")
install.packages("optmatch")
install.packages("Matching")
install.packages("dplyr")
install.packages("patchwork")
install.packages("tibble")
install.packages("kable")
install.packages("stringr")
install.packages("effectsize")
install.packages("lm.beta")
install.packages("psych")
library(psych)
library(tidyr)
library(corrplot)
library(fastDummies)
library(rgenoud)
library(cobalt)
library(MatchIt)
library(WeightIt)
library(stargazer)
library(car)
library(ggplot2)
library(optmatch)
library(Matching)
library(dplyr)
library(patchwork)
library(tibble)
library(stringr)
library(effectsize)



url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRF_X8POsM2RAt2I7ogda-V8hiPBv5_csym2Tnoz051WmXUSqTlPVK8tcE5G1TaprtYU16R_aCh21bL/pub?gid=1853431303&single=true&output=csv"
causal_inference_data <- read.csv(url)
causal_inference_data <- na.omit(causal_inference_data)


"Assumptions___________________________________________________________________"
model2 <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) +
               white_perc + bach_perc,
             data = causal_inference_data)
stargazer(model2, type="text")
causal_inference_data$predicted2 <- predict(model2)
causal_inference_data$residuals2 <- residuals(model2)
ggplot(causal_inference_data, aes(x = predicted2, y = residuals2)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Model 2 Residual Plot",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal()
ggplot(causal_inference_data, aes(sample = residuals2)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Residuals") +
  theme_minimal() +
  coord_fixed(ratio = 1/10)
vif_values <- vif(model2)
vif_values
matched_outcome_optimal <- matchit(photo_id ~ as.factor(felony_laws) +
                                     white_perc + bach_perc,
                                   data = causal_inference_data,
                                   method = "optimal")
matched_df <- match.data(matched_outcome_optimal)
model3 <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) +
               white_perc + bach_perc, data=matched_df)
stargazer(model3, type="text")
"Regression Models_____________________________________________________________"
model1 <- lm(turnout ~ as.factor(photo_id) + political_lean,
             data = causal_inference_data)
stargazer(model1, type="text")
stargazer(model1, type = "html", title = "model1_summary",
          out = "model1_summary.html")
model2 <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) +
               white_perc + bach_perc,
             data = causal_inference_data)
stargazer(model2, type="text")
stargazer(model2, type = "html", title = "model2_summary",
          out = "model2_summary.html")
model3 <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) +
               white_perc + bach_perc, data=matched_df)
stargazer(model3, type="text")
stargazer(model3, type = "html", title = "model3_summary",
          out = "model3_summary.html")
"Matching______________________________________________________________________"
matched_outcome_ps <- matchit(as.factor(photo_id) ~ as.factor(felony_laws) +
                                white_perc + bach_perc,
                              data = causal_inference_data,
                              method = "nearest")
love.plot(matched_outcome_ps)
bal.plot(matched_outcome_ps, "white_perc", which = "both")
bal.plot(matched_outcome_ps, "bach_perc", which = "both")
matched_outcome_genetic <- matchit(as.factor(photo_id) ~
                                     as.factor(felony_laws) +
                                     white_perc + bach_perc,
                                   data = causal_inference_data,
                                   method = "genetic")
love.plot(matched_outcome_genetic)
bal.plot(matched_outcome_genetic, "white_perc", which = "both")
bal.plot(matched_outcome_genetic, "bach_perc", which = "both")
matched_outcome_optimal <- matchit(photo_id ~ as.factor(felony_laws) +
                                     white_perc + bach_perc,
                                   data = causal_inference_data,
                                   method = "optimal")
love.plot(matched_outcome_optimal)
bal.plot(matched_outcome_optimal, "white_perc", which = "both")
bal.plot(matched_outcome_optimal, "bach_perc", which = "both")
matched_df <- match.data(matched_outcome_optimal)
model3 <- lm(turnout ~ as.factor(photo_id) + as.factor(felony_laws) +
               white_perc + bach_perc, data=matched_df)
"Correlation Matrices__________________________________________________________"
summary(model3)

dummy_df <- causal_inference_data %>%
  select(photo_id, white_perc, bach_perc, felony_laws)
dummy_df <- dummy_cols(dummy_df, remove_first_dummy = TRUE,
                       remove_selected_columns = TRUE)
dummy_df <- dummy_df %>%
  rename(P = 5,
         PP = 4,
         PPP = 6,
         PPPP = 7)
order <- c("photo_id", "white_perc", "bach_perc", "P", "PP", "PPP", "PPPP")
cor_matrix <- cor(dummy_df, use = "complete.obs")
cor_matrix <- cor_matrix[order, order]
corrplot(cor_matrix, method = "color",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         type = "full", tl.col = "black", tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.8)


