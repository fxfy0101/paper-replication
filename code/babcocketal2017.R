###################################################################################
### Empirical Methods II Spring 2022                                            ###
### Homework I: Replication of Babcock, Recalde, Vesterlund and Weingart (2017) ###
###                                                                             ###
### student: Yong Feng                                                          ###
### Date of this version: 2022-01-26                                            ###
###################################################################################

### load packages and install if not found -------------------------------------------------

packages <- c("tidyverse", "haven", "AER", "mfx", "stargazer", "xtable", "texreg", "cobalt")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
library(tidyverse)
library(haven)
library(AER)
library(mfx)
library(cobalt)
library(stargazer)
library(xtable)
library(texreg)


### clear the environment and set the path -------------------------------------------------------------------

rm(list = ls())
if (Sys.info()["user"] == "Brittany" | Sys.info()["user"] == "brsz8f") {
  
  ### the Sys.info() function return different outputs in Mac OS and Windows system
  ### if the flow control doesn't work, please just run the two lines of codes below and delete other lines
  output_path <- "C:/Users/Brittany/OneDrive - University of Missouri/MUteaching/GradMethods/Assignments/SP2022/hw1"
  data_path <- "C:/Users/Brittany/OneDrive - University of Missouri/MUteaching/GradMethods/Assignments/BabcokRecaldeVesterlunWeingart_2017_nonpromotabletasks/data"
  
} else if (Sys.info()["user"] == "yong") {
  output_path <- "/Users/yong/Documents/Programming/R/Replication/hw1"
  data_path <- "/Users/yong/Documents/Programming/R/Replication/data"
}


### import data -------------------------------------------------------------

experiments_1_2 <- read_dta(str_c(data_path, "/BabcockRecaldeVesterlundWeingart_2017_experiments_1_2.dta"))
experiment_3 <- read_dta(str_c(data_path, "/BabcockRecaldeVesterlundWeingart_2017_experiment_3.dta"))


### table replication -------------------------------------------------------

### table 2:
###   - first column
tab2_col1 <- probitmfx(decision ~ female + period, data = filter(experiments_1_2, experiment == 1), atmean = FALSE, clustervar1 = "unique_subjectid")
###   - second column
tab2_col2 <- probitmfx(decision ~ female + period, data = filter(experiments_1_2, experiment == 1, period <= 5), atmean = FALSE, clustervar1 = "unique_subjectid")
###   - third column
tab2_col3 <- probitmfx(decision ~ female + period, data = filter(experiments_1_2, experiment == 1, period >= 6), atmean = FALSE, clustervar1 = "unique_subjectid")
### generate LaTeX table code and save to txt file
### use \input{...} to put tables into LaTeX file
sink(str_c(output_path, "/feng_yong_hw1_tab2.html"))
htmlreg(list(tab2_col1, tab2_col2, tab2_col3), stars = numeric(0), 
        custom.header = list("All rounds" = 1, "Rounds 1-5" = 2, "Rounds 6-10" = 3),
        custom.model.names = c("(1)", "(2)", "(3)"),
        custom.coef.names = c("Female", "Round"), digits = 3,
        custom.gof.names = c("Observations"),
        override.se = list(tab2_col1$mfxest[, 4], tab2_col2$mfxest[, 4], tab2_col3$mfxest[, 4]),
        caption = "TABLE2—PROBABILITY OF INVESTING (Probit): EXPERIMENT 1",
        caption.above = TRUE,
        label = "table2",
        booktabs = TRUE,
        include.loglik = FALSE,
        include.deviance = FALSE,
        include.aic = FALSE,
        include.bic = FALSE)
sink()

### table 3:
###   - first column
tab3_col1 <- probitmfx(decision ~ female + period, data = filter(experiments_1_2, experiment == 2), atmean = FALSE, clustervar1 = "unique_subjectid")
###   - second column
tab3_col2 <- probitmfx(decision ~ female + period, data = filter(experiments_1_2, experiment == 2, period <= 5), atmean = FALSE, clustervar1 = "unique_subjectid")
###   - third column
tab3_col3 <- probitmfx(decision ~ female + period, data = filter(experiments_1_2, experiment == 2, period >= 6), atmean = FALSE, clustervar1 = "unique_subjectid")
### generate LaTeX table code and save to txt file
sink(str_c(output_path, "/feng_yong_hw1_tab3.html"))
htmlreg(list(tab3_col1, tab3_col2, tab3_col3), stars = numeric(0), 
        custom.header = list("All rounds" = 1, "Rounds 1-5" = 2, "Rounds 6-10" = 3),
        custom.model.names = c("(1)", "(2)", "(3)"),
        custom.coef.names = c("Female", "Round"), digits = 3,
        custom.gof.names = c("Observations"),
        override.se = list(tab3_col1$mfxest[, 4], tab3_col2$mfxest[, 4], tab3_col3$mfxest[, 4]),
        caption = "TABLE3—PROBABILITY OF INVESTING (<i>Probit</i>): EXPERIMENT 2",
        caption.above = TRUE,
        label = "table3",
        booktabs = TRUE,
        include.loglik = FALSE,
        include.deviance = FALSE,
        include.aic = FALSE,
        include.bic = FALSE,
        use.packages = FALSE)
sink()

### table 4: requests received via the strategy method (ols): experiment 3
###   - first column
tab4_col1 <- experiment_3 %>%
  group_by(unique_subjectid, session_id, female, non_caucasian, n_com_by_session) %>%
  summarise(total_requests = sum(n_asked)) %>%
  lm(data = ., total_requests ~ female + non_caucasian)
###   - second column
tab4_col2 <- experiment_3 %>%
  group_by(unique_subjectid, session_id, female, non_caucasian, n_com_by_session) %>%
  summarise(total_requests = sum(n_asked)) %>%
  lm(data = ., total_requests ~ female + non_caucasian + n_com_by_session)
###   - third column
tab4_col3 <- experiment_3 %>%
  filter(period <= 5) %>%
  group_by(unique_subjectid, session_id, female, non_caucasian, n_com_by_session) %>%
  summarise(total_requests = sum(n_asked)) %>%
  lm(data = ., total_requests ~ female + non_caucasian)
###   - fourth column
tab4_col4 <- experiment_3 %>%
  filter(period <= 5) %>%
  group_by(unique_subjectid, session_id, female, non_caucasian, n_com_by_session) %>%
  summarise(total_requests = sum(n_asked)) %>%
  lm(data = ., total_requests ~ female + non_caucasian + n_com_by_session)
###   - fifth column
tab4_col5 <- experiment_3 %>%
  filter(period > 5) %>%
  group_by(unique_subjectid, session_id, female, non_caucasian, n_com_by_session) %>%
  summarise(total_requests = sum(n_asked)) %>%
  lm(data = ., total_requests ~ female + non_caucasian)
###   - sixth column
tab4_col6 <- experiment_3 %>%
  filter(period > 5) %>%
  group_by(unique_subjectid, session_id, female, non_caucasian, n_com_by_session) %>%
  summarise(total_requests = sum(n_asked)) %>%
  lm(data = ., total_requests ~ female + non_caucasian + n_com_by_session)
### generate table
sink(str_c(output_path, "/feng_yong_hw1_tab4.html"))
htmlreg(list(tab4_col1, tab4_col2, tab4_col3, tab4_col4, tab4_col5, tab4_col6),
        stars = numeric(0),
        custom.header = list("Rounds 1-10" = 1:2, "Rounds 1-5" = 3:4, "Rounds 6-10" = 5:6),
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
        custom.coef.names = c("Constant", "Female", "Non-Caucasian", "N communicate with in session"),
        reorder.coef = c(2, 3, 4, 1),
        digits = 3,
        override.se = list(coeftest(tab4_col1, vcov. = vcovHC(tab4_col1, cluster = "session_id"))[, 4],
                           coeftest(tab4_col2, vcov. = vcovHC(tab4_col2, cluster = "session_id"))[, 4],
                           coeftest(tab4_col3, vcov. = vcovHC(tab4_col3, cluster = "session_id"))[, 4],
                           coeftest(tab4_col4, vcov. = vcovHC(tab4_col4, cluster = "session_id"))[, 4],
                           coeftest(tab4_col5, vcov. = vcovHC(tab4_col5, cluster = "session_id"))[, 4],
                           coeftest(tab4_col6, vcov. = vcovHC(tab4_col6, cluster = "session_id"))[, 4]),
        caption = "TABLE4—REQUESTS RECEIVED VIA THE STRATEGY METHOD (OLS): EXPERIMENT 3",
        caption.above = TRUE,
        include.rsquared = FALSE,
        include.adjrs = FALSE,
        custom.gof.names = "Observations")
sink()


# figures replication -----------------------------------------------------

### figure 1: probability of investing: experiment 1
### data wrangling
experiments_1_2 %>%
  filter(experiment == 1) %>%
  group_by(period, female) %>%
  summarize(prob_invest = sum(decision) / n()) %>%
  ### plot figure
  ggplot(mapping = aes(x = period, y = prob_invest, color = factor(female))) +
    geom_line(size = 0.7) +
    ### set figure format
    labs(x = "Round", y = "Probability of investing", title = "FIGURE 1. PROBABILITY OF INVESTING: EXPERIMENT 1") +
    scale_y_continuous(limits = c(0, 0.5)) +
    scale_x_continuous(breaks = 1:10, labels = 1:10) +
    scale_color_manual(values = c("#1a476f", "#bfa19c"), labels = c("Male", "Female"), name = NULL) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "light grey"),
          legend.position = c(0.9, 0.14),
          legend.background = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw1_fig1.pdf", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

### figure 2
### data wrangling
experiments_1_2 %>%
  filter(experiment == 1) %>%
  group_by(unique_subjectid, female) %>%
  summarise(individual_total_invest = sum(decision)) %>%
  ### plot figure
  ggplot(mapping = aes(x = individual_total_invest, y = ..prop.., fill = factor(female))) +
    geom_bar(position = "dodge", width = 0.5) +
    ### set figure format
    labs(x = "Total investment", y = "Relative frequency", title = "FIGURE 2. DISTRIBUTION OF TOTAL INVESTMENT: EXPERIMENT 1") +
    scale_y_continuous(limits = c(0, 0.25)) +
    scale_x_continuous(breaks = 0:10, labels = 0:10) +
    scale_fill_manual(values = c("#1a476f", "#bfa19c"), labels = c("Male", "Female"), name = NULL) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "light grey"),
          legend.position = c(0.9, 0.88),
          legend.background = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw1_fig2.pdf", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")  

### figure 3: probability of investing: experiment 2
### data wrangling
experiments_1_2 %>%
  filter(experiment == 2) %>%
  group_by(period, female) %>%
  summarize(prob_invest = sum(decision) / n()) %>%
  ### plot figure
  ggplot(mapping = aes(x = period, y = prob_invest, color = factor(female))) +
    geom_line(size = 0.7) +
    ### set figure format
    labs(x = "Round", y = "Probability of investing", title = "FIGURE 3. PROBABILITY OF INVESTING: EXPERIMENT 2") +
    scale_y_continuous(limits = c(0, 0.5)) +
    scale_x_continuous(breaks = 1:10, labels = 1:10) +
    scale_color_manual(values = c("#1a476f", "#bfa19c"), labels = c("Male", "Female"), name = NULL) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "light grey"),
          legend.position = c(0.9, 0.14),
          legend.background = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw1_fig3.pdf", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

### figure 4: distribution of total investment: experiment 2
### data wrangling
experiments_1_2 %>%
  filter(experiment == 2) %>%
  group_by(unique_subjectid, female) %>%
  summarise(individual_total_invest = sum(decision)) %>%
  ### plot figure
  ggplot(mapping = aes(x = individual_total_invest, y = ..prop.., fill = factor(female))) +
    geom_bar(position = "dodge", width = 0.5) +
    ### set figure format
    labs(x = "Total investment", y = "Relative frequency", title = "FIGURE 4. DISTRIBUTION OF TOTAL INVESTMENT: EXPERIMENT 2") +
    scale_y_continuous(limits = c(0, 0.26), breaks = seq(0, 0.25, 0.05), labels = seq(0, 0.25, 0.05)) +
    scale_x_continuous(breaks = 0:10, labels = 0:10) +
    scale_fill_manual(values = c("#1a476f", "#bfa19c"), labels = c("Male", "Female"), name = NULL) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "light grey"),
          legend.position = c(0.9, 0.84),
          legend.background = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw1_fig4.pdf", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in") 

### figure 5: probability group invests
### data wrangling
experiments_1_2 %>%
  distinct(session_id, group, period, .keep_all = T) %>%
  mutate(gender_composition = case_when(share_females_session == 0 ~ "all_male",
                                        share_females_session == 1 ~ "all_female",
                                        T ~ "mixed")) %>%
  group_by(gender_composition, period) %>%
  summarise(total_invest = n(),
            success = sum(investment_group)) %>%
  mutate(likelihood = success / total_invest,
         gender_composition = factor(gender_composition, levels = c("mixed", "all_male", "all_female"))) %>%
  ### plot figure
  ggplot(mapping = aes(x = period, y = likelihood, color = gender_composition, linetype = gender_composition)) +
    geom_line(size = 0.7) +
    ### set figure format
    labs(x = "Round", y = "Likelihood group invests", title = "FIGURE 5. PROBABILITY GROUP INVESTS") +
    scale_y_continuous(limits = 0:1, breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = 1:10, labels = 1:10) +
    scale_color_manual(values = c("black", "#1a476f", "#bfa19c"), labels = c("Mixed", "All male", "All female"), name = NULL) +
    scale_linetype_manual(values = c("mixed" = 1, "all_male" = 2, "all_female" = 2), labels = c("Mixed", "All male", "All female"), name = NULL) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "light grey"),
          legend.position = c(0.8, 0.14),
          legend.background = element_blank(), 
          legend.key.width = unit(1.3, "cm"), 
          legend.key.height = unit(0.4, "cm"),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw1_fig5.pdf", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

### figure 6: individual propensity to invest
### data wrangling
experiments_1_2 %>%
  mutate(gender_composition = case_when(experiment == 1 & female == 0 ~ "Mixed_M",
                                        experiment == 1 & female == 1 ~ "Mixed_F",
                                        experiment == 2 & female == 0 ~ "Single_M",
                                        T ~ "Single_F")) %>%
  mutate(gender_composition = factor(gender_composition, levels = c("Single_M", "Single_F", "Mixed_M", "Mixed_F"))) %>%
  group_by(period, gender_composition) %>%
  summarize(prob_invest = sum(decision) / n()) %>%
  ### plot figure
  ggplot(mapping = aes(x = period, y = prob_invest, color = gender_composition, linetype = gender_composition)) +
    geom_line(size = 0.7) +
    ### set figure format
    labs(x = "Round", y = "Probability of investing", title = "FIGURE 6. INDIVIDUAL PROPENSITY TO INVEST") +
    scale_y_continuous(limits = c(0, 0.5)) +
    scale_x_continuous(breaks = 1:10, labels = 1:10) +
    scale_color_manual(values = c("#1a476f", "#bfa19c", "#1a476f", "#bfa19c"), name = NULL) +
    scale_linetype_manual(values = c("Single_M" = 2, "Single_F" = 2, "Mixed_M" = 1, "Mixed_F" = 1), name = NULL) +
    guides(color = guide_legend(nrow = 2)) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "light grey"),
          legend.position = c(0.7, 0.14),
          legend.background = element_blank(), 
          legend.key.width = unit(1.5, "cm"),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw1_fig6.pdf", path = output_path, scale = 0.7, dpi = 300, width = 8.91, height = 5.66, units = "in")

### figure 7: distribution of strategy-method requests per individual: experiment 3
### data wrangling
experiment_3 %>%
  distinct(session_id, unique_subjectid, .keep_all = T) %>%
  group_by(female) %>%
  count(n_asked_session) %>%
  group_by(female) %>%
  mutate(relative_freq = n / sum(n)) %>%
  ### plot figure
  ggplot(mapping = aes(x = n_asked_session, y = relative_freq, fill = factor(female))) +
    geom_col(position = "dodge", width = 0.6) +
    ### set figure format
    labs(x = "Total times asked to invest", y = "relative frequency", title = "FIGURE 7. DISTRIBUTION OF STRATEGY-METHOD REQUESTS PER INDIVIDUAL: EXPERIMENT 3") +
    scale_y_continuous(limits = c(0, 0.17)) +
    scale_x_continuous(breaks = seq(0, 16, 2), labels = seq(0, 16, 2)) +
    scale_fill_manual(values = c("#1a476f", "#bfa19c"), labels = c("Male", "Female"), name = NULL) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "light grey"),
          legend.position = c(0.1, 0.75),
          legend.background = element_blank(),
          plot.title = element_text(size = 10),
          axis.title = element_text(size = 8))
ggsave("feng_yong_hw1_fig7.pdf", path = output_path, scale = 0.8, dpi = 300, width = 8.91, height = 5.66, units = "in")
