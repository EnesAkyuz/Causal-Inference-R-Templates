install.packages("tidyverse")
library(tidyverse)

setwd("~/Desktop")

################
#Import and clean data
################

election_data <- read.csv("County_Election_Results_2000-2020.csv")
causal_inference_data <- read.csv("Causal Inference Data.csv")

adj_elec_data <- election_data %>%
  filter(year %in% c(2008, 2012, 2016, 2020)) %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  mutate(vote_perc = 100*candidatevotes/totalvotes) %>%
  group_by(county_fips, year) %>%
  summarize(county_name = max(county_name),
            state = max(state),
            tot_votes = max(totalvotes),
            dem_perc = max(vote_perc[party=="DEMOCRAT"]),
            rep_perc = max(vote_perc[party=="REPUBLICAN"]),
            margin = rep_perc-dem_perc) 


demographic_data <- causal_inference_data %>%
  select(county_fips, political_lean, white_perc, bach_perc)

vap_2008 <- read.csv("County2008.csv")
vap_2012 <- read.csv("County2012.csv")
vap_2016 <- read.csv("County2016.csv")
vap_2020 <- read.csv("County2020.csv")

adj_vap_data_2008 <- vap_2008 %>%
  filter(LNTITLE == "Total") %>%
  mutate(GEOID = str_sub(GEOID, -5, -1) %>% str_remove("^0+")) %>%
  rename(county_fips = GEOID,
         vap = TOT_EST) %>%
  mutate(county_fips = as.numeric(county_fips),
         vap_2008 = as.numeric(vap)) %>%
  select(county_fips, vap_2008) %>%
  left_join(adj_elec_data %>% filter(year == 2008), by="county_fips") %>%
  mutate(turnout_2008 = 100*tot_votes/vap_2008) %>%
  filter(state != "ALASKA",
         turnout_2008 < 100) %>%
  rename(tot_votes_2008 = tot_votes,
         dem_perc_2008 = dem_perc,
         rep_perc_2008 = rep_perc,
         margin_2008 = margin)

adj_vap_data_2008 <- na.omit(adj_vap_data_2008)


adj_vap_data_2012 <- vap_2012 %>%
  filter(LNTITLE == "Total") %>%
  mutate(GEOID = str_sub(GEOID, -5, -1) %>% str_remove("^0+")) %>%
  rename(county_fips = GEOID,
         vap = TOT_EST) %>%
  mutate(county_fips = as.numeric(county_fips),
         vap_2012 = as.numeric(vap)) %>%
  select(county_fips, vap_2012) %>%
  left_join(adj_elec_data %>% filter(year == 2012), by="county_fips") %>%
  mutate(turnout_2012 = 100*tot_votes/vap_2012) %>%
  filter(state != "ALASKA",
         turnout_2012 < 100) %>%
  rename(tot_votes_2012 = tot_votes,
         dem_perc_2012 = dem_perc,
         rep_perc_2012 = rep_perc,
         margin_2012 = margin)

adj_vap_data_2012 <- na.omit(adj_vap_data_2012)


adj_vap_data_2016 <- vap_2016 %>%
  filter(LNTITLE == "Total") %>%
  mutate(GEOID = str_sub(GEOID, -5, -1) %>% str_remove("^0+")) %>%
  rename(county_fips = GEOID,
         vap = TOT_EST) %>%
  mutate(county_fips = as.numeric(county_fips),
         vap_2016 = as.numeric(vap)) %>%
  select(county_fips, vap_2016) %>%
  left_join(adj_elec_data %>% filter(year == 2016), by="county_fips") %>%
  mutate(turnout_2016 = 100*tot_votes/vap_2016) %>%
  filter(state != "ALASKA",
         turnout_2016 < 100) %>%
  rename(tot_votes_2016 = tot_votes,
         dem_perc_2016 = dem_perc,
         rep_perc_2016 = rep_perc,
         margin_2016 = margin)

adj_vap_data_2016 <- na.omit(adj_vap_data_2016)


adj_vap_data_2020 <- vap_2020 %>%
  filter(lntitle == "Total") %>%
  mutate(geoid = str_sub(geoid, -5, -1) %>% str_remove("^0+")) %>%
  rename(county_fips = geoid,
         vap = tot_est) %>%
  mutate(county_fips = as.numeric(county_fips),
         vap_2020 = as.numeric(vap)) %>%
  select(county_fips, vap_2020) %>%
  left_join(adj_elec_data %>% filter(year == 2020), by="county_fips") %>%
  mutate(turnout_2020 = 100*tot_votes/vap_2020) %>%
  filter(state != "ALASKA",
         turnout_2020 < 100) %>%
  rename(tot_votes_2020 = tot_votes,
         dem_perc_2020 = dem_perc,
         rep_perc_2020 = rep_perc,
         margin_2020 = margin)

adj_vap_data_2020 <- na.omit(adj_vap_data_2020)

final_data <- adj_vap_data_2008 %>%
  left_join(demographic_data, by="county_fips") %>%
  left_join(adj_vap_data_2012, by="county_fips") %>%
  left_join(adj_vap_data_2016, by="county_fips") %>%
  left_join(adj_vap_data_2020, by="county_fips") %>%
  select(county_fips,
         county_name.x,
         state.x,
         political_lean,
         white_perc,
         bach_perc,
         vap_2008,
         tot_votes_2008,
         turnout_2008,
         vap_2012,
         tot_votes_2012,
         turnout_2012,
         vap_2016,
         tot_votes_2016,
         turnout_2016,
         vap_2020,
         tot_votes_2020,
         turnout_2020) %>%
  rename(county_name = county_name.x,
         state = state.x)

#write.csv(final_data, "turnout_panel_data.csv")


fin_vap_data_2008 <- adj_vap_data_2008 %>%
  rename(vap = vap_2008, 
         tot_votes = tot_votes_2008,
         turnout = turnout_2008) %>%
  filter(state %in% c("IOWA", "MINNESOTA", "PENNSYLVANIA")) %>%
  mutate(post = 0,
         treat = ifelse(state == "IOWA", 1, 0),
         year = 2008) %>%
  left_join(demographic_data, by="county_fips") %>%
  select(year,
         county_fips,
         county_name,
         state,
         vap,
         tot_votes,
         turnout,
         treat,
         post,
         white_perc,
         bach_perc)


fin_vap_data_2012 <- adj_vap_data_2012 %>%
  rename(vap = vap_2012, 
         tot_votes = tot_votes_2012,
         turnout = turnout_2012) %>%
  filter(state %in% c("IOWA", "MINNESOTA", "PENNSYLVANIA")) %>%
  mutate(post = 0,
         treat = ifelse(state == "IOWA", 1, 0),
         year = 2012) %>%
  left_join(demographic_data, by="county_fips") %>%
  select(year,
         county_fips,
         county_name,
         state,
         vap,
         tot_votes,
         turnout,
         treat,
         post,
         white_perc,
         bach_perc)


fin_vap_data_2016 <- adj_vap_data_2016 %>%
  rename(vap = vap_2016, 
         tot_votes = tot_votes_2016,
         turnout = turnout_2016) %>%
  filter(state %in% c("IOWA", "MINNESOTA", "PENNSYLVANIA")) %>%
  mutate(post = 0,
         treat = ifelse(state == "IOWA", 1, 0),
         year = 2016) %>%
  left_join(demographic_data, by="county_fips") %>%
  select(year,
         county_fips,
         county_name,
         state,
         vap,
         tot_votes,
         turnout,
         treat,
         post,
         white_perc,
         bach_perc)


fin_vap_data_2020 <- adj_vap_data_2020 %>%
  rename(vap = vap_2020, 
         tot_votes = tot_votes_2020,
         turnout = turnout_2020) %>%
  filter(state %in% c("IOWA", "MINNESOTA", "PENNSYLVANIA")) %>%
  mutate(post = 1,
         treat = ifelse(state == "IOWA", 1, 0),
         year = 2020) %>%
  left_join(demographic_data, by="county_fips") %>%
  select(year,
         county_fips,
         county_name,
         state,
         vap,
         tot_votes,
         turnout,
         treat,
         post,
         white_perc,
         bach_perc)

stacked_data_total <- rbind(fin_vap_data_2008, 
                            fin_vap_data_2012,
                            fin_vap_data_2016,
                            fin_vap_data_2020)

stacked_data_minnesota <- rbind(fin_vap_data_2008 %>% filter(state %in% c("IOWA", "MINNESOTA")), 
                                fin_vap_data_2012 %>% filter(state %in% c("IOWA", "MINNESOTA")),
                                fin_vap_data_2016 %>% filter(state %in% c("IOWA", "MINNESOTA")),
                                fin_vap_data_2020 %>% filter(state %in% c("IOWA", "MINNESOTA")))
#write.csv(stacked_data_minnesota, "stacked_county_data_minnesota.csv")

stacked_data_pennsylvania <- rbind(fin_vap_data_2008 %>% filter(state %in% c("IOWA", "PENNSYLVANIA")), 
                                   fin_vap_data_2012 %>% filter(state %in% c("IOWA", "PENNSYLVANIA")),
                                   fin_vap_data_2016 %>% filter(state %in% c("IOWA", "PENNSYLVANIA")),
                                   fin_vap_data_2020 %>% filter(state %in% c("IOWA", "PENNSYLVANIA")))


# graph parallel trends
summary_data <- stacked_data_total %>%
  group_by(state, year) %>%
  summarize(
    mean_turnout = mean(turnout),
    lower_bound = quantile(turnout, 0.25), 
    upper_bound = quantile(turnout, 0.75),
    .groups = "drop"
  )

ggplot(summary_data, aes(x = year, y = mean_turnout, color = state, fill = state)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.2, color = NA) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "black", size = 0.5) +
  annotate("text", x = 2018, y = max(summary_data$mean_turnout) + 2, label = "Post Treatment", color = "black") +
  annotate("text", x = 2016, y = max(summary_data$mean_turnout) + 2, label = "Pre Treatment", color = "black") +
  scale_x_continuous(breaks = c(2008, 2012, 2016, 2020)) +
  labs(
    title = "Mean county turnout in presidential elections (with interquartile range).",
    x = "Year",
    y = "Turnout",
    color = "State",
    fill = "State"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# test parallel trends
stacked_data_minnesota_placebo1 <- stacked_data_minnesota %>%
  mutate(post = ifelse(year %in% c(2012, 2016, 2020), 1, 0 ))

stacked_data_minnesota_placebo2 <- stacked_data_minnesota %>%
  mutate(post = ifelse(year %in% c(2016, 2020), 1, 0 ))

placebo1 <- lm(turnout ~ post * treat + white_perc + bach_perc, data=stacked_data_minnesota_placebo1)
summary(placebo1)

placebo2 <- lm(turnout ~ post * treat + white_perc + bach_perc, data=stacked_data_minnesota_placebo2)
summary(placebo2)




stacked_data_pennsylvania_placebo1 <- stacked_data_pennsylvania %>%
  mutate(post = ifelse(year %in% c(2012, 2016, 2020), 1, 0 ))

stacked_data_pennsylvania_placebo2 <- stacked_data_pennsylvania %>%
  mutate(post = ifelse(year %in% c(2016, 2020), 1, 0 ))

placebo3 <- lm(turnout ~ post * treat + white_perc + bach_perc, data=stacked_data_pennsylvania_placebo1)
summary(placebo3)

placebo4 <- lm(turnout ~ post * treat + white_perc + bach_perc, data=stacked_data_pennsylvania_placebo2)
summary(placebo4)


# run regression
model1 <- lm(turnout ~ post * treat + white_perc + bach_perc, data=stacked_data_minnesota)
summary(model1)

model2 <- lm(turnout ~ post * treat + white_perc + bach_perc, data=stacked_data_pennsylvania)
summary(model2)

means <- stacked_data_pennsylvania%>%
  group_by(post, treat) %>%
  summarize(turnout = mean(turnout))

# Example: Difference-in-Differences regression for practical significance
# Using `model1` from your existing DiD analysis

# Step 1: Extract the treatment effect (interaction term: post:treat)
treatment_effect <- coef(model1)["post:treat"]

# Step 2: Calculate the average pre-treatment turnout in treated units
pre_treatment_turnout <- stacked_data_minnesota %>%
  filter(post == 0, treat == 1) %>%
  summarize(avg_turnout = mean(turnout)) %>%
  pull(avg_turnout)

# Step 3: Compute practical significance metrics
# Percent change in turnout due to the treatment
percent_change <- (treatment_effect / pre_treatment_turnout) * 100

# Step 4: Range of turnout values to understand the relative magnitude
turnout_range <- stacked_data_minnesota %>%
  summarize(min_turnout = min(turnout), max_turnout = max(turnout)) %>%
  mutate(range = max_turnout - min_turnout) %>%
  pull(range)

relative_effect <- (treatment_effect / turnout_range) * 100

# Step 5: Display results
cat("Treatment Effect (Percentage Points):", round(treatment_effect, 2), "\n")
# Step 1: Calculate standard deviations for treated and control groups
sd_treated <- stacked_data_minnesota %>%
  filter(treat == 1) %>%
  summarize(sd = sd(turnout)) %>%
  pull(sd)

sd_control <- stacked_data_minnesota %>%
  filter(treat == 0) %>%
  summarize(sd = sd(turnout)) %>%
  pull(sd)

# Step 2: Compute pooled standard deviation
pooled_sd <- sqrt(((sd_treated^2 + sd_control^2) / 2))

# Step 3: Compute effect size (Cohen's d)
effect_size <- treatment_effect / pooled_sd

# Step 4: Display effect size
cat("Standardized Effect Size (Cohen's d):", round(effect_size, 2), "\n")
