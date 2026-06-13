library(tidyverse)
library(ggplot2)
library(dplyr)
library(gtsummary)
library(gt)
library(gtExtras)
library(tibble)
library(moments)
library(modelsummary)

tbl_reg_data <- read.csv("DATA/reg_ssb.csv")

tbl_reg_data <- tbl_reg_data %>%   
  mutate(gender = factor(gender, levels = c(0,1), labels = c("Gender: Female", "Gender: Male")))
         
str(tbl_reg_data)


#Modelsummary#-----
var_pol <- c("diversity", "attitude_change", "threats", "social_difference", "climate",
             "S5Q5r17","educationLevel", "household_income", "age", #"Life_Satisfaction",
             #"Health", 
             "Support_neig", "Belonging_neig", "SocialR_neig", 
             #"Distrust", "Yrs_in_Neig", 
             "Centrality")

plot_hist <- tbl_reg_data %>%
  select(all_of(var_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(dist = list(na.omit(value)), .groups = "drop")

labels_tbl  <-  c(
  "diversity" = "Ethnic Diversity",
  "attitude_change" = "Attitude Change",
  "threats" = "Threats",
  "social_difference" = "Social Difference",
  "climate" = "Climate",
  "S5Q5r17" = "Centralisation",
  "educationLevel" = "Education level",
  "household_income" = "Income",
  "age" = "Age",
  "gender" = "Gender",
  #"Life_Satisfaction" = "Life satisfaction",
  "Support_neig" = "Neighbourhood support",
  "Belonging_neig" ="Neighbourhood belonging",
  "SocialR_neig" = "Social relations neighbourhood"
  #"Yrs_in_Neig"= "Years in neighbourhood"
  )

#MeanSD <- function(x) {sprintf("%.2f (%.2f)", mean(x), sd(x))}
Skew <- function(x)moments::skewness(x, na.rm = T)

tbl_mod <- datasummary(
  diversity + attitude_change + threats+
  social_difference + climate+ S5Q5r17 +
  educationLevel + household_income + age + gender +
  #Life_Satisfaction + Health +
  Support_neig + Belonging_neig + SocialR_neig +
  #Distrust + Yrs_in_Neig + 
    Centrality
    ~
    N + Mean + SD + Median +  P25 + P75 + Min + Max + Skew,
  data = tbl_reg_data,
  title = "Descriptive Statistics",
  output = "data.frame"
) %>% 
  rename(variable = 2) %>% 
  left_join(plot_hist, by = "variable") %>% 
  mutate(variable = recode(variable, !!!labels_tbl)) %>% 
  gt(rowname_col = "variable") %>%
  opt_table_font(font = "Times New Roman") %>% 
  cols_label(
    dist = "Distribution"
  ) %>% 
  gt_plt_dist(column = dist, type = "histogram") %>% 
  tab_row_group(
    label = "Independent Variables",
    rows = 12:15) %>% 
  tab_row_group(
  label = "Control Variables (Independent)",
  rows = 7:11) %>% 
  tab_row_group(
  label = "Dependent Variables",
  rows = 1:6)
tbl_mod

gt_tbl_mod <- tbl_mod %>% 
  cols_hide(columns = 1) %>% 
  tab_header(
    title = "Descriptive Statistics",
    subtitle = "Variables included in the Spatial and Regression Analyses") 

gtsave(gt_tbl_mod, filename = "PLOTS/DescStat_tbl.png",
       vwidth = 1600, vheight = 900, zoom = 2)

gt_tbl_mod

# Categorical variables ####
tbl_gender <- datasummary(gender~ N + Percent(),
            data = tbl_reg_data,
            title = "Categorical variable", 
            fmt = 1,
            output = "PLOTS/tbl_gender.png")
tbl_gender
gtsave(tbl_gender, filename = "PLOTS/tbl_gender.png",
       vwidth = 1600, vheight = 900, zoom = 2)

# Outliers
box_div <- boxplot(tbl_reg_data$diversity)
box_att <- boxplot(tbl_reg_data$attitude_change)
box_thr <- boxplot(tbl_reg_data$threats)
box_sdif <- boxplot(tbl_reg_data$social_difference)
box_cli <- boxplot(tbl_reg_data$climate)
box_Cent <- boxplot(tbl_reg_data$S5Q5r17)

box_edl <- boxplot(tbl_reg_data$educationLevel)
box_inc <- boxplot(tbl_reg_data$household_income)
box_age <- boxplot(tbl_reg_data$age)

box_supp <- boxplot(tbl_reg_data$Support_neig)
box_bel <- boxplot(tbl_reg_data$Belonging_neig)
box_SocR <- boxplot(tbl_reg_data$SocialR_neig)

#TEST -----
# tmp <- datasummary(
#   diversity + attitude_change + threats+
#     social_difference + climate+
#     educationLevel + household_income + age + gender +
#     Life_Satisfaction + Health +
#     Support_neig + Belonging_neig + SocialR_neig +
#     Distrust + Yrs_in_Neig + Centrality
#   ~
#     N + Mean + SD + Min + Max,
#   data = tbl_reg_data,
#   output = "data.frame"
# )
# 
# names(tmp)
# rownames(tmp)
# View(tmp)
# 
# tbl_skim <- datasummary_skim(diversity + attitude_change + threats+
#                    social_difference + climate+
#                    educationLevel + household_income + age + gender +
#                    Life_Satisfaction + Health +
#                    Support_neig + Belonging_neig + SocialR_neig +
#                    Distrust + Yrs_in_Neig + Centrality, data = tbl_reg_data)
#######
dep_pol_var <- tbl_reg_data %>% 
  select(c(diversity, attitude_change, threats, social_difference, climate, S5Q5r17))
view(dep_pol_var)

#####
library(psych)

#Cor(dep_pol_var)
cor_pol <- lowerCor(dep_pol_var)

corTbl <- round(cor_pol, 2)
corTbl[upper.tri(corTbl)] <- NA

pol_corTbl <- as.data.frame(corTbl)
pol_corTbl [is.na(pol_corTbl)] <- ""
gt(pol_corTbl)

# Test
lowercor_matrix <- function (x
                             #, digits = 2
                             ){
  #x <- round(x, digits)
  x[upper.tri(x)] <- NA
  x
}

pol_labels  <-  c(
  "diversity" = "Ethnic Diversity",
  "attitude_change" = "Attitude Change",
  "threats" = "Threats",
  "social_difference" = "Social Difference",
  "climate" = "Climate",
  "S5Q5r17" = "Centralisation")

gt_cor_pol <- dep_pol_var %>% 
  lowerCor(method = "spearman") %>% 
  lowercor_matrix() %>% 
  as.data.frame() %>% 
  rename_with(~ifelse(.x%in% names(pol_labels), pol_labels[.x], .x)) %>% 
  rownames_to_column(var = "Variable") %>%
  mutate(Variable = ifelse(Variable %in% names(pol_labels), pol_labels[Variable], Variable)) %>% 
  gt() %>% 
  tab_header(title = "Correlation Matrix Dependent Variables") %>% 
  cols_label(Variable = "") %>% 
  fmt_number(columns = where(is.numeric), decimals = 2) %>% 
  sub_missing(everything(), missing_text = "") %>% 
  opt_table_font(font = "Times New Roman")
  
gt_cor_pol
gtsave(gt_cor_pol, filename = "PLOTS/Corr_Matrix_Pol.png",
        vwidth = 1600, vheight = 400, zoom = 2
       )


### TEST MED SIGNIFIKANS #######

cor_test <- corr.test(dep_pol_var, method = "spearman")

# Korrelasjoner
cor_matrix <- cor_test$r

# P-verdier
p_matrix <- cor_test$p

significant <- p_matrix < 0.05

cor_with_sig <- ifelse(p_matrix < 0.05,
                       paste0(round(cor_matrix, 2), "*"),
                       round(cor_matrix, 2))
cor_with_sig

p_matrix
########
ggplot(dep_pol_var, aes(diversity, climate))+
  geom_point()+
  geom_smooth()

ggplot(dep_pol_var, aes(threats, climate))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(dep_pol_var, aes(diversity, social_difference))+
  geom_point()+
  geom_smooth()

ggplot(dep_pol_var, aes(social_difference, climate))+
  geom_point()+
  geom_smooth()

ggplot(dep_pol_var, aes(social_difference, S5Q5r17))+
  geom_point()+
  geom_smooth(method = lm)
mod_s_diff <- lm(social_difference~S5Q5r17, data = dep_pol_var)
summary(mod_s_diff)
