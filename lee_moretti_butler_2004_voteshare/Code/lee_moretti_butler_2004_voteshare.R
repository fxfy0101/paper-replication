################################################################################
### Empirical Methods II Spring 2022                                         ###
### Homework 2: Replication of Lee, Moretti, and Butler (2004)               ###
###                                                                          ###
### student: Yong Feng                                                       ###
### Date of this version: 2022-02-26                                         ###
###                                                                          ###
################################################################################


################################### Preamble ###################################

### import packages
packages <- c("tidyverse", "haven", "AER", "texreg", "rvest", "devtools")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
if (!require(ggbrace)) {
  devtools::install_github("nicolash2/ggbrace")
}

library(tidyverse)
library(ggbrace)
library(haven)
library(AER)
library(texreg)
library(rvest)

### clear the environment and set the path 
rm(list = ls())
if (Sys.info()["user"] == "Brittany" | Sys.info()["user"] == "brsz8f") {
  
  ### the Sys.info() function return different outputs in Mac OS and Windows system
  ### if the flow control doesn't work, please just run the two lines of codes below and delete other lines
  output_path <- "C:/Users/Brittany/OneDrive - University of Missouri/MUteaching/GradMethods/Assignments/SP2021/hw2"
  data_path <- "C:/Users/Brittany/OneDrive - University of Missouri/MUteaching/GradMethods/Assignments/LeeMorettiButler_2004_votershare/data"
  
} else if (Sys.info()["user"] == "yong") {
  output_path <- "/Users/yong/Documents/Programming/R/Replication/hw2"
  data_path <- "/Users/yong/Documents/Programming/R/Replication/data"
}

### import data
vote_data <- read_dta(str_c(data_path, "/LeeMorettiBulter_rdreplication.dta"))

################################################################################


################################## Question 1 ##################################

### figure replication ---------------------------------------------------------

### figure 1: total effect of initial win on future ada scores: gamma
vote_data %>%
  mutate(dem_bins = trunc(lagdemvoteshare * 100) / 100) %>%
  group_by(dem_bins) %>%
  summarise(mean_score = mean(score, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = dem_bins, y = mean_score)) +
    geom_point(size = 1) +
    geom_vline(xintercept = 0.5) +
    geom_smooth(mapping = aes(group = factor(dem_bins < 0.5)), method = "lm", formula = y ~ poly(x, 2), 
                show.legend = FALSE, color = "black", lwd = 0.7) +
    geom_brace(aes(c(0.5, 0.55), c(32, 52)), inherit.data = FALSE, rotate = 90) +
    annotate("text", x = 0.56, y = 42, label = "gamma", parse = TRUE) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),expand = c(0.01, 0)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0.01, 0)) +
    labs(x = "Democratic Vote Share, time t", y = "ADA Score, time t + 1", 
         title = expression(paste("FIGURE I TOTAL EFFECT OF INITIAL WIN ON FUTURE ADA SCORE: ", gamma))) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw2_fig1.png", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

### figure 2a: effect of party affiliation: pi_1
vote_data %>%
  mutate(dem_bins = trunc(lagdemvoteshare * 100) / 100) %>%
  group_by(dem_bins) %>%
  summarise(mean_score = mean(lagscore, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = dem_bins, y = mean_score)) +
    geom_point(size = 1) +
    geom_vline(xintercept = 0.5) +
    geom_smooth(mapping = aes(group = factor(dem_bins < 0.5)), method = "lm", formula = y ~ poly(x, 2),
                show.legend = FALSE, color = "black", lwd = 0.7) +
    geom_brace(mapping = aes(c(0.5, 0.55), c(16, 61)), inherit.data = FALSE, rotate = 90) +
    annotate("text", x = 0.57, y = 38.5, label = "pi[1]", parse = TRUE) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),expand = c(0.01, 0)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0.01, 0)) +
    labs(x = "Democratic Vote Share, time t", y = "ADA Score, time t", 
        title = expression(paste("FIGURE IIa EFFECT OF PARTY AFFILIATION: ", pi[1]))) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw2_fig2a.png", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

### figure 2b: effect of initial win on winning next election
vote_data %>%
  mutate(dem_bins = trunc(lagdemvoteshare * 100) / 100) %>%
  group_by(dem_bins) %>%
  summarise(win_prob = mean(democrat, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = dem_bins, y = win_prob)) +
    geom_point(size = 1) +
    geom_vline(xintercept = 0.5) +
    geom_smooth(mapping = aes(group = factor(dem_bins < 0.5)), method = "lm", formula = y ~ poly(x, 2),
                show.legend = FALSE, color = "black", lwd = 0.7) +
    geom_brace(mapping = aes(c(0.5, 0.55), c(0.25, 0.8)), inherit.data = FALSE, rotate = 90) +
    annotate("text", x = 0.62, y = 0.53, label = "(P[t + 1]^D - P[t + 1]^R)", parse = TRUE) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),expand = c(0.01, 0)) +
    scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2), expand = c(0.01, 0)) +
    labs(x = "Democratic Vote Share, time t", y = "Probability of Democrat Win, time t + 1",
         title = expression(paste("FIGURE IIb EFFECT OF INITIAL WIN ON WINNING NEXT ELECTION: ", P[t + 1]^D - P[t + 1]^R))) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))  
ggsave("feng_yong_hw2_fig2b.png", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

### figure 3: income
vote_data %>%
  mutate(dem_bins = trunc(lagdemvoteshare * 100) / 100) %>%
  group_by(dem_bins) %>%
  summarise(mean_income = mean(realincome, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = dem_bins, y = mean_income)) +
    geom_point(shape = 1) +
    geom_vline(xintercept = 0.5) +
    geom_smooth(mapping = aes(group = factor(dem_bins < 0.5)), method = "lm", formula = y ~ poly(x, 2),
                show.legend = FALSE, color = "black", lwd = 0.7) +
    scale_x_continuous(limits = c(0.25, 0.75), breaks = seq(0.25, 0.75, 0.25)) +
    scale_y_continuous(limits = c(28000, 40000), breaks = seq(30000, 40000, 5000)) +
    labs(x = "Democrat Vote Share at time t", y = "Income", title = "FIGURE IIIa: INCOME") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw2_fig3_income.png", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

### figure 3: high school
vote_data %>%
  mutate(dem_bins = trunc(lagdemvoteshare * 100) / 100) %>%
  group_by(dem_bins) %>%
  summarise(mean_high_school = mean(pcthighschl, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = dem_bins, y = mean_high_school)) +
    geom_point(shape = 1) +
    geom_vline(xintercept = 0.5) +
    geom_smooth(mapping = aes(group = factor(dem_bins < 0.5)), method = "lm", formula = y ~ poly(x, 2),
                show.legend = FALSE, color = "black", lwd = 0.7) +
    scale_x_continuous(limits = c(0.25, 0.75), breaks = seq(0.25, 0.75, 0.25), expand = c(0.01, 0)) +
    scale_y_continuous(limits = c(0.3, 0.41), breaks = seq(0.3, 0.4, 0.05)) +
    labs(x = "Democrat Vote Share at time t", y = "High_School", title = "FIGURE IIIb: HIGH SCHOOL") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw2_fig3_high_school.png", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

### figure 3: black
vote_data %>%
  mutate(dem_bins = trunc(lagdemvoteshare * 100) / 100) %>%
  group_by(dem_bins) %>%
  summarise(mean_black = mean(pctblack, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = dem_bins, y = mean_black)) +
    geom_point(shape = 1) +
    geom_vline(xintercept = 0.5) +
    geom_smooth(mapping = aes(group = factor(dem_bins < 0.5)), method = "lm", formula = y ~ poly(x, 2),
                show.legend = FALSE, color = "black", lwd = 0.7) +
    scale_x_continuous(limits = c(0.25, 0.75), breaks = seq(0.25, 0.75, 0.25), expand = c(0.01, 0)) +
    scale_y_continuous(limits = c(0, 0.15), breaks = seq(0.05, 0.15, 0.05)) +
    labs(x = "Democrat Vote Share at time t", y = "High_School", title = "FIGURE IIIc: BLACK") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw2_fig3_black.png", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

### figure 3: eligible
vote_data %>%
  mutate(dem_bins = trunc(lagdemvoteshare * 100) / 100,
         pct_eligible = votingpop / totpop) %>%
  group_by(dem_bins) %>%
  summarise(mean_eligible = mean(pct_eligible, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = dem_bins, y = mean_eligible)) +
    geom_point(shape = 1) +
    geom_vline(xintercept = 0.5) +
    geom_smooth(mapping = aes(group = factor(dem_bins < 0.5)), method = "lm", formula = y ~ poly(x, 2),
              show.legend = FALSE, color = "black", lwd = 0.7) +
    scale_x_continuous(limits = c(0.25, 0.75), breaks = seq(0.25, 0.75, 0.25), expand = c(0.01, 0)) +
    scale_y_continuous(limits = c(0.63, 0.7), breaks = seq(0.64, 0.7, 0.02)) +
    labs(x = "Democrat Vote Share at time t", y = "Eligible", title = "FIGURE IIId: ELIGIBLE") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw2_fig3_eligible.png", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

### table replication ----------------------------------------------------------

### table 1 column 1
tab1_col1 <- vote_data %>%
  filter(lagdemvoteshare > 0.48 & lagdemvoteshare < 0.52) %>%
  mutate(D = ifelse(lagdemvoteshare > 0.5, 1, 0)) %>%
  mutate(dem_bins = trunc(lagdemvoteshare * 100) / 100) %>%
  select(score, dem_bins, D) %>%
  lm(data = ., score ~ D)

### table 1 column 2
tab1_col2 <- vote_data %>%
  filter(lagdemvoteshare > 0.48 & lagdemvoteshare < 0.52) %>%
  mutate(D = ifelse(lagdemvoteshare > 0.5, 1, 0),
         dem_bins = trunc(lagdemvoteshare * 100) / 100) %>%
  select(lagscore, dem_bins, D) %>%
  lm(data = ., lagscore ~ D)

### table 1 column 3
tab1_col3 <- vote_data %>%
  filter(lagdemvoteshare > 0.48 & lagdemvoteshare < 0.52) %>%
  mutate(D = ifelse(lagdemvoteshare > 0.5, 1, 0),
         dem_bins = trunc(lagdemvoteshare * 100) / 100) %>%
  select(democrat, dem_bins, D) %>%
  lm(data = ., democrat ~ D)

### export table 1 to csv file
minimal_html(str_c(htmlreg(list(tab1_col1, tab1_col2, tab1_col3),
                           caption = "TABLE I: RESULTS BASED ON ADA SCORES -- CLOSE ELECTIONS SAMPLE",
                           caption.above = TRUE,
                           omit.coef = "(Intercept)",
                           custom.coef.names = c("Estimated gap"),
                           custom.header = list("ADA_{t+1} = 1" = 1, "ADA_{t}" = 2, "DEM_{t+1}" = 3),
                           custom.model.names = c("(1)", "(2)", "(3)"),
                           include.rsquared = FALSE,
                           include.adjrs = FALSE))) %>%
  html_element("table") %>%
  html_table() %>%
  write_csv(str_c(output_path, "/feng_yong_hw2_tab1.csv"))

################################################################################


################################## Question 2 ##################################

### part a: re-estimate table 1 with full bandwidth ----------------------------

### re-estimation +/-2 bandwidth
tabpolyfit_col1 <- vote_data %>%
  filter(lagdemvoteshare > 0.48 & lagdemvoteshare < 0.52) %>%
  mutate(D = ifelse(lagdemvoteshare > 0.5, 1, 0)) %>%
  mutate(dem_bins = trunc(lagdemvoteshare * 100) / 100) %>%
  select(score, dem_bins, D) %>%
  lm(data = ., score ~ D)

### re-estimation using a constant on the full bandwidth
tabpolyfit_col2 <- vote_data %>%
  mutate(D = ifelse(lagdemvoteshare > 0.5, 1, 0)) %>%
  select(score, D) %>%
  lm(data = ., score ~ D)

### re-estimation using linear on the full bandwidth
tabpolyfit_col3 <- vote_data %>%
  mutate(D = ifelse(lagdemvoteshare > 0.5, 1, 0)) %>%
  select(score, D, lagdemvoteshare) %>%
  lm(data = ., score ~ D*I(lagdemvoteshare - 0.5))

### re-estimation using quadratic on the full bandwidth
tabpolyfit_col4 <- vote_data %>%
  mutate(D = ifelse(lagdemvoteshare > 0.5, 1, 0)) %>%
  select(score, D, lagdemvoteshare) %>%
  lm(data = ., score ~ D*I(lagdemvoteshare - 0.5) + D*I((lagdemvoteshare - 0.5)^2))

### export table
minimal_html(str_c(htmlreg(list(tabpolyfit_col1, tabpolyfit_col2, tabpolyfit_col3, tabpolyfit_col4),
                           caption = "TABLE II: RESULTS BASED ON ADA SCORES -- FULL BANDWIDTH",
                           caption.above = TRUE,
                           custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
                           include.rsquared = FALSE,
                           include.adjrs = FALSE,
                           custom.coef.names = c("Intercept", "Estimated gap", "Running variable", "Interaction between running variable and gap",
                                                 "Quadratic of running variable", "Interaction between running variable and quadratic")))) %>%
  html_element("table") %>%
  html_table() %>%
  write_csv(str_c(output_path, "/feng_yong_hw2_tabpolyfit.csv"))
rep(1:5, 3)
### part b: recreate figure 1 using bins of 0.05 -------------------------------
df <- vote_data %>%
  mutate(dem_bins = cut(lagdemvoteshare, seq(0, 1, 0.05), include.lowest = TRUE),
         D = (lagdemvoteshare < 0.5)) %>%
  group_by(dem_bins) %>%
  summarise(mean_score = mean(score, na.rm = TRUE),
            D_group = mean(D)) %>%
  filter(!is.na(dem_bins)) %>%
  rbind(.[rep(1:20, 2), ]) %>%
  mutate(function_form = rep(c("Constant", "Linear", "Quadratic"), each = 20))
ggplot(data = df, mapping = aes(x = dem_bins, y = mean_score)) +
  geom_point(size = 1) +
  geom_vline(xintercept = 11) +
  geom_smooth(data = filter(df, function_form == "Constant"), mapping = aes(group = D_group),
              method = "lm", formula = y ~ 1, color = "black", show.legend = TRUE) +
  geom_smooth(data = filter(df, function_form == "Linear"), mapping = aes(group = D_group),
              method = "lm", color = "red", show.legend = TRUE) +
  geom_smooth(data = filter(df, function_form == "Quadratic"), mapping = aes(group = D_group),
              method = "lm", formula = y ~ poly(x, 2), color = "blue", show.legend = TRUE) +
  facet_wrap(~function_form, nrow = 2, scales = "free") +
  scale_x_discrete(labels = seq(0, 1, 0.05)) +
  labs(x = "Democratic Vote Share, time t", y = "ADA Score, time t + 1",
       title = expression(paste("FIGURE I TOTAL EFFECT OF INITIAL WIN ON FUTURE ADA SCORE: ", gamma))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))
ggsave("feng_yong_hw2_fig1_functionalform.png", path = output_path, scale = 1.2, dpi = 300, width = 8.91, height = 5.66, units = "in")


################################## Question 3 ##################################

### part a: histogram with vertical line ---------------------------------------
vote_data %>%
  mutate(dem_bins = trunc(lagdemvoteshare * 100) / 100) %>%
  ggplot(mapping = aes(x = dem_bins)) +
    geom_histogram(aes(y = ..density..), fill = "gray70", binwidth = 0.01) +
    geom_vline(xintercept = 0.5, color = "#1a476f") +
    geom_density(color = "#1a476f") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Running Variable: Democrat Vote Share at time t", title = "DISTRIBUTION OF RUNNING VARIABLE (bins: 0.01)") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw2_figdensitya.png", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

### part b: number of observations as vertical axis ----------------------------
vote_data %>%
  ### two extremes 0 and 1 are excluded
  mutate(dem_bins = trunc(lagdemvoteshare * 100) / 100,
         D = (lagdemvoteshare < 0.5)) %>%
  group_by(dem_bins) %>%
  summarise(n_obs = n(),
            D_group = mean(D)) %>%
  filter(dem_bins != 0, dem_bins != 1) %>%
  ggplot(mapping = aes(x = dem_bins, y = n_obs)) +
    geom_point() +
    geom_smooth(mapping = aes(group = D_group)) +
    geom_vline(xintercept = 0.5) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),expand = c(0.01, 0)) +
    labs(x = "Democratic Vote Share, time t", y = "Number of observations, time t + 1", 
       title = expression(paste("NUMBER OF OBSERVATION PER BIN (kernal density)"))) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))    
ggsave("feng_yong_hw2_figdensityb.png", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

################################################################################


################################## Question 4 ##################################

### placebo check

placebo_test <- data.frame(cutpoint = seq(0.1, 0.9, 0.1),
                           coefficient = rep(0, 9), 
                           CI_lower = rep(0, 9), 
                           CI_upper = rep(0, 9))
for (cutpoint in seq(0.1, 0.9, 0.1)){
  rdd_reg <- vote_data %>%
    filter(lagdemvoteshare > cutpoint - 0.02 & lagdemvoteshare < cutpoint + 0.02) %>%
    mutate(D = ifelse(lagdemvoteshare > cutpoint, 1, 0)) %>%
    select(score, D) %>%
    lm(data = ., score ~ D)
  placebo_test[cutpoint * 10, 2] <- coef(rdd_reg)[2]
  placebo_test[cutpoint * 10, 3] <- confint(rdd_reg)[2, 1]
  placebo_test[cutpoint * 10, 4] <- confint(rdd_reg)[2, 2]
}
ggplot(data = placebo_test, mapping = aes(x = cutpoint, y = coefficient)) +
  geom_line(color = "dodgerblue") +
  geom_errorbar(mapping = aes(x = cutpoint, ymin = CI_lower, ymax = CI_upper), color = "dodgerblue", width = 0.01) +
  geom_point(size = 1) +
  scale_x_continuous(limits = c(0.1, 0.91), breaks = seq(0.1, 0.9, 0.1)) +
  theme_bw() +
  labs(title = "PLACEBO TEST") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 8))
ggsave("feng_yong_hw2_figplacebo.png", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

################################################################################


################################## Question 5 ##################################

### bandwidth analysis

bandwidth_test <- data.frame(bandwidth = seq(0.02, 0.4, 0.02),
                             coefficient = rep(0, 20),
                             CI_lower = rep(0, 20),
                             CI_upper = rep(0, 20))

for (i in 1:20) {
  rdd_reg <- vote_data %>%
    filter(lagdemvoteshare > 0.5 - bandwidth_test[i, 1] & lagdemvoteshare < 0.5 + bandwidth_test[i, 1]) %>%
    mutate(D = ifelse(lagdemvoteshare > 0.5, 1, 0)) %>%
    select(score, D) %>%
    lm(data = ., score ~ D)
  bandwidth_test[i, 2] <- coef(rdd_reg)[2]
  bandwidth_test[i, 3] <- confint(rdd_reg)[2, 1]
  bandwidth_test[i, 4] <- confint(rdd_reg)[2, 2]
}
ggplot(data = bandwidth_test, mapping = aes(x = bandwidth, y = coefficient)) +
  geom_line(color = "dodgerblue") +
  geom_errorbar(mapping = aes(x = bandwidth, ymin = CI_lower, ymax = CI_upper), color = "dodgerblue", width = 0.005) +
  geom_point(size = 1) +
  scale_x_continuous(limits = c(0.015, 0.405), breaks = seq(0.02, 0.4, 0.02)) +
  theme_bw() +
  labs(title = "BANDWIDTH ANALYSIS") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 8))
ggsave("feng_yong_hw2_figbandwidth.png", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

################################################################################
