library(tidyverse)
library(ggplot2)
library(jtools)
library(officer)
library(flextable)
library(broom)
library(clubSandwich)
library(modelsummary)

#model check
library(performance)


data_factors <- read.csv("/Users/eiraheleneberglemstad/R_Projects/Master/all_factors_results_uS.csv")
data_factors <- data_factors %>% 
  select(!c(City_area, City_part))

#Gender mann = 1, 2 = Kvinne class=numeric
data_factors$gender <- recode(data_factors$gender, `1`= 1, `2`= 0)
head(data_factors$gender)
class(data_factors$gender)
data_factors$gender <- as.factor(data_factors$gender)
levels(data_factors$gender)
#Gender mann = 1, 0 = kvinne, class = factor


imputation <- read.csv("/Users/eiraheleneberglemstad/R_Projects/Master/data_imputated.csv")
imputation_reg <- imputation %>%
  select(record, S5Q5r17, S7Q1, S7Q2, S4Q4)

imp_neig <- read.csv("/Users/eiraheleneberglemstad/R_Projects/Master/imputated_neig.csv")
imp_neig_reg <- imp_neig %>% 
  select(record, household_income, educationLevel, 
         S1Q3, S2Q5, 
         S2Q6r1, S2Q6r7, S3Q4r1) 

imp_data <- left_join( imp_neig_reg, imputation_reg, by = "record")

# reg_data ---------
reg_data <- left_join(data_factors, imp_data, by = "record")
reg_data <- reg_data %>% 
  rename(c("Life_Satisfaction"=S7Q1, 
           "Health" = S7Q2,
           "Distrust" = S2Q5,
           "Yrs_in_Neig" = S1Q3))

#TEST forslag fra chatGPT om å reskalere age
reg_data$age <- reg_data$age/10 # OBS! 1 heltall økning i Age skal tolkes som 10 år
# reg_data$household_income <- reg_data$household_income/2
#reg_data$age_col <- reg_data$age - mean(reg_data$age)
#reg_data$age2 <- reg_data$age_col^2
#reg_data$age2 <- (reg_data$age)^2

# view(reg_data)

reg_data$zipcode <- as.character(reg_data$zipcode)
class(reg_data$zipcode)
reg_data <- reg_data %>% 
  rename(postnummer = zipcode, kommune = kommune2024) %>% 
  mutate(postnummer = str_pad(str_replace_all(postnummer, "\\s", ""), width = 4,
                              side = "left", pad = "0")) %>% 
  mutate(kommune = str_pad(str_replace_all(kommune, "\\s", ""), width = 4,
                           side = "left", pad = "0"))

summary(reg_data)
#View(reg_data)
# 
reg_data %>%
  ggplot(aes(age, diversity))+
  geom_point()+
  geom_smooth(method = "lm")
#library(moments)
# skewness(reg_data$diversity)
# kurtosis(reg_data$diversity)
# hist(reg_data$diversity)
# median(reg_data$diversity)
# IQR(reg_data$diversity)
# 
# boxplot(reg_data$diversity)
# skewness(reg_data$attitude_change)
# kurtosis(reg_data$attitude_change)
# hist(reg_data$attitude_change)
# median(reg_data$attitude_change)
# 
# skewness(reg_data$threats)
# kurtosis(reg_data$threats)
# hist(reg_data$threats)
# 
# skewness(reg_data$social_difference)
# kurtosis(reg_data$social_difference)
# hist(reg_data$social_difference)
# 
# skewness(reg_data$climate)
# kurtosis(reg_data$climate)
# hist(reg_data$climate)

# ----- Sentralitet datasett ------
SSB <- read.csv("/Users/eiraheleneberglemstad/R_Projects/Master/DATA/sentralitetsindeks_2023-2024_kommuner.csv", sep = ";")
#Endre knr.2024 til character? --> sette en null foran alle 301 --> slå sammen datasett med felles kommune
SSB <- SSB %>%
  rename(c("kommune" = "knr.2024", "kommune_navn" = "Kommune.2024",
           "Centrality" = Klasse.2023))
SSB$kommune <- as.character(SSB$kommune)
class(SSB$kommune)
SSB$kommune[SSB$kommune == "301"] <- "0301"
summary(SSB)


# ------------
reg_ssb <- left_join(reg_data, SSB, by = "kommune")
#View(reg_ssb)
str(reg_ssb)
#write.csv(reg_ssb, "DATA/reg_ssb.csv")

# 1 Attitude towards diversity among immigrants ####

mod1.1 <- lm(diversity~ Support_neig + Belonging_neig + SocialR_neig, 
             reg_data)
summary(mod1.1)
plot(mod1.1)
check_model(mod1.1)
#
mod1.11 <- lm(diversity~ Support_neig + Belonging_neig + SocialR_neig + Distrust,# + 
                #Yrs_in_Neig, 
              reg_data)
summary(mod1.11)

mod1.2 <- lm(diversity ~ educationLevel + household_income + age + I(age^2) + gender, 
             reg_data)
summary(mod1.2)



## TEST: Polynomial regression TEST Stats globe ####
y <- reg_data$climate
x <- reg_data$age

modpoly <- lm(climate~ poly(x,2), 
             reg_data)
summary(modpoly)

age_plot <- ggplot(reg_data, aes(x, y))+
  geom_point()
age_plot + div_age2

div_age2 <- stat_smooth(method = "lm",
                       formula = y ~ poly(x, 2),
                       se = F) 
div_age <- stat_smooth(method = "lm",
              formula = y ~ x,
              se = F)
age_plot + div_age2 + div_age
  

# fitted_mod1.2 <- augment(mod1.2, data = reg_data)
# ggplot(fitted_mod1.2, aes(x = .fitted, y = .resid))+
#   geom_point(aes(color = kommune))+
#   geom_smooth(method = "lm")
# ggplot(fitted_mod1.2, aes(x = .resid))+
#   geom_histogram()
ggplot(data = reg_data, aes(x=age, y = climate))+
  geom_point()
#
mod1.21 <- lm(diversity~ educationLevel + household_income + age + gender +
                Life_Satisfaction + Health, 
             reg_data)
summary(mod1.21)


# mod_s_coent <- lm(climate~S5Q5r17, reg_data)
# summary(mod_s_coent)
# modS5q5r17 <- lm(S5Q5r17~ Support_neig + Belonging_neig + SocialR_neig+ Distrust + 
#                    Yrs_in_Neig, 
#              reg_data)
# summary(modS5q5r17)

# 2 Attitude change in immigration questions ####
mod2.1 <- lm(attitude_change~ Support_neig + Belonging_neig + SocialR_neig, 
             reg_data)
summary(mod2.1)

mod2.11 <- lm(attitude_change~ Support_neig + Belonging_neig + SocialR_neig + Distrust,# + 
                #Yrs_in_Neig, 
              reg_data)
summary(mod2.11)

#
mod2.2 <- lm(attitude_change~ educationLevel + household_income + age + gender, 
             reg_data)
summary(mod2.2)

# export_summs(mod2.2, lm2.1, lm2.2, lm2.3, lm2.4, scale = T, 
#              to.file = "docx", file.name = "reg_LM_back_attCh.docx")

mod2.21 <- lm(attitude_change~ educationLevel + household_income + age + gender +
                Life_Satisfaction + Health, 
              reg_data)
summary(mod2.21)

# ggplot(data = reg_data, aes(x = Support_neig, y = attitude_change))+
#   geom_smooth()+
#   geom_point()+
#   labs(title = "")+
#   theme_minimal()
# 
# ggplot(data = reg_data, aes(x = Belonging_neig, y = attitude_change))+
#   geom_smooth()+
#   geom_point()+
#   labs(title = "")+
#   theme_minimal()
# 
# ggplot(data = reg_data, aes(x = SocialR_neig, y = attitude_change))+
#   geom_smooth()+
#   geom_point()+
#   labs(title = "")+
#   theme_minimal()

#3 Threats ####
mod3.1 <- lm(threats~ Support_neig + Belonging_neig + SocialR_neig, 
             reg_data)
summary(mod3.1)

mod3.11 <- lm(threats~ Support_neig + Belonging_neig + SocialR_neig + Distrust,# + 
                #Yrs_in_Neig, 
              reg_data)
summary(mod3.11)

mod3.2 <- lm(threats~ educationLevel + household_income + age + gender, 
             reg_data)
summary(mod3.2)
mod3.21 <- lm(threats~ educationLevel + household_income + age + gender +
                Life_Satisfaction + Health, 
              reg_data)
summary(mod3.21)

# export_summs(mod3.2, lm3.1, lm3.2, lm3.3, lm3.4, scale = T, 
#              to.file = "docx", file.name = "reg_LM_back_threats.docx")

# ggplot(data = reg_data, aes(x = Support_neig, y = threats))+
#   geom_smooth()+
#   geom_point()+
#   labs(title = "")+
#   theme_minimal()
# 
# ggplot(data = reg_data, aes(x = Belonging_neig, y = threats))+
#   geom_smooth()+
#   geom_point()+
#   labs(title = "")+
#   theme_minimal()
# 
# ggplot(data = reg_data, aes(x = SocialR_neig, y = threats))+
#   geom_smooth()+
#   geom_point()+
#   labs(title = "")+
#   theme_minimal()

# 4 Social difference models ####
mod4.1 <- lm(social_difference~ Support_neig + Belonging_neig + SocialR_neig, 
             reg_data)
summary(mod4.1)

mod4.11 <- lm(social_difference~ Support_neig + Belonging_neig + SocialR_neig + Distrust, 
              #+ Yrs_in_Neig, 
              reg_data)
summary(mod4.11)

mod4.2 <- lm(social_difference ~ educationLevel + household_income + age + gender, 
             reg_data)
summary(mod4.2)
mod4.21 <- lm(social_difference~ educationLevel + household_income + age + gender +
                Life_Satisfaction + Health, 
              reg_data)
summary(mod4.21)


# ggplot(data = reg_data, aes(x = Support_neig, y = social_difference))+
#   geom_smooth()+
#   geom_point()+
#   labs(title = "")+
#   theme_minimal()
# 
# ggplot(data = reg_data, aes(x = Belonging_neig, y = social_difference))+
#   geom_smooth()+
#   geom_point()+
#   labs(title = "")+
#   theme_minimal()
# 
# ggplot(data = reg_data, aes(x = SocialR_neig, y = social_difference))+
#   geom_smooth()+
#   geom_point()+
#   labs(title = "")+
#   theme_minimal()
# hist(reg_data$social_difference)


# 5 Attitudes towards climate ####
mod5.1 <- lm(climate~ Support_neig + Belonging_neig + SocialR_neig, 
             reg_data)
summary(mod5.1)

mod5.11 <- lm(climate~ Support_neig + Belonging_neig + SocialR_neig + Distrust,
              #+ Yrs_in_Neig
              reg_data)
summary(mod5.11)

# Bakgrunn
mod5.2 <- lm(climate ~ educationLevel + household_income + age + gender, 
             reg_data)
summary(mod5.2)

mod5.21 <- lm(climate~ educationLevel + household_income + age + gender +
                Life_Satisfaction + Health, 
              reg_data)
summary(mod5.21)

# 6 Attitudes towards centralisation ####
mod6.1 <- lm(S5Q5r17~ Support_neig + Belonging_neig + SocialR_neig, 
             reg_data)
summary(mod6.1)

mod6.11 <- lm(S5Q5r17~ Support_neig + Belonging_neig + SocialR_neig + Distrust + 
                Yrs_in_Neig, 
              reg_data)
summary(mod6.11)

# Bakgrunn
mod6.2 <- lm(S5Q5r17 ~ educationLevel + household_income + age + gender, 
             reg_data)
summary(mod6.2)

mod6.21 <- lm(S5Q5r17~ educationLevel + household_income + age + gender +
                Life_Satisfaction + Health, 
              reg_data)
summary(mod6.21)



#### EXPORT ####
#effect_plot(mod1.1, pred = Support_neig, interval = T, plot.points = T, jitter = 0.05)
# export_summs(mod1.1, mod2.1, mod3.1, mod4.1, mod5.1, scale = T,
#              to.file = "docx", file.name = "regMod_neig_pol.docx")
tbl_modelsummary_fac_neig <- modelsummary(list("Ethnic Diversity" = mod1.1, "Attitude Change" = mod2.1,
                                           "Threats" = mod3.1, "Social Difference" = mod4.1,
                                           "Climate Change" = mod5.1, "Centralisation" = mod6.1),
                                      estimate = "{estimate}{stars}",
                                      starts = T,
                                      fmt = 2,
                                      coef_map = c(
                                        "(Intercept)" = "(Intercept)",
                                        Support_neig = "Support in neig",
                                        Belonging_neig = "Belonging to neig",
                                        SocialR_neig = "SocialR in neig"
                                      ),
                                      gof_omit = "AIC|BIC|Log\\.Lik|RMSE",
                                      title = "Social Cohesion in the Neighbourhood",
                                      output = "PLOTS/reg_MLR_fac_neig.png")
tbl_modelsummary_fac_neig
                                      
# export_summs(mod1.11, mod2.11, mod3.11, mod4.11, mod5.11, scale = T,
#              to.file = "docx", file.name = "regMod_neig_pol_11.docx")
tbl_modelsummary_neig <- modelsummary(list("Ethnic Diversity" = mod1.11, "Attitude Change" = mod2.11,
                                           "Threats" = mod3.11, "Social Difference" = mod4.11,
                                           "Climate Change" = mod5.11, "Centralisation" = mod6.11),
                                      estimate = "{estimate}{stars}",
                                      starts = T,
                                      fmt = 2,
                                      coef_map = c("(Intercept)" = "(Intercept)",
                                                   Support_neig = "Support in neig",
                                                   Belonging_neig = "Belonging to neig",
                                                   SocialR_neig = "SocialR in neig",
                                                   Distrust = "Distrust"
                                      ),
                                      gof_omit = "AIC|BIC|Log\\.Lik|RMSE",
                                      title = "Social Cohesion in the Neighbourhood with Distrust",
                                      output = "PLOTS/reg_MLR_neig.png"
)
tbl_modelsummary_neig
# 
# export_summs(mod1.2, mod2.2, mod3.2, mod4.2, mod5.2, scale = T,
#              to.file = "docx", file.name = "reg_MLR_back.docx")
# export_summs(mod1.21, mod2.21, mod3.21, mod4.21, mod5.21, scale = T,
# to.file = "docx", file.name = "reg_MLR_back_21.docx")
tbl_modelsummary_back_small <- modelsummary(list("Ethnic Diversity" = mod1.2, "Attitude Change" = mod2.2,
                                           "Threats" = mod3.2, "Social Difference" = mod4.2,
                                           "Climate Change" = mod5.2, "Centralisation" = mod6.2),
                                      estimate = "{estimate}{stars}",
                                      starts = T,
                                      fmt = 2,
                                      coef_map = c("(Intercept)" = "(Intercept)",
                                                   educationLevel = "Education level", 
                                                   household_income = "Income",
                                                   age = "Age (10-year units)",
                                                   gender1 = "Male (ref: Female)",
                                                   Life_Satisfaction = "Life Satisfaction",
                                                   Health = "Health"
                                      ),
                                      gof_omit = "AIC|BIC|Log\\.Lik|RMSE",
                                      title = "Background",
                                      output = "PLOTS/reg_MLR_back_small.png"
)

tbl_modelsummary_back_small

tbl_modelsummary_back <- modelsummary(list("Ethnic Diversity" = mod1.21, "Attitude Change" = mod2.21,
                                  "Threats" = mod3.21, "Social Difference" = mod4.21,
                                  "Climate Change" = mod5.21, "Centralisation" = mod6.21),
                             estimate = "{estimate}{stars}",
                             starts = T,
                             fmt = 2,
                             coef_map = c("(Intercept)" = "(Intercept)",
                                            educationLevel = "Education level", 
                                          household_income = "Income",
                                          age = "Age (10-year units)",
                                          gender1 = "Male (ref: Female)",
                                          Life_Satisfaction = "Life Satisfaction",
                                          Health = "Health"
                                          ),
                             gof_omit = "AIC|BIC|Log\\.Lik|RMSE",
                             title = "Background",
                             output = "PLOTS/reg_MLR_back.png"
                            )

tbl_modelsummary_back
#--------------- SENTRALITET INDIVID ---------------------


# Modeller sentralitetsindeks ----
# reg_ssb_uSval <- reg_ssb %>% 
#   filter(!postnummer==9170)
# library(modelsummary)
# lm1.6 <- lm(diversity ~ Centrality, 
#             reg_ssb_uSval)
# summary(lm1.6)
# 
# reg_ssb_uSval %>% ggplot(aes(x = Centrality, y = diversity))+
#   geom_point()+
#   geom_smooth(method = lm)
# 
# lm2.6 <- lm(attitude_change ~ Centrality, 
#             reg_ssb)
# lm3.6 <- lm(threats ~ Centrality, 
#             reg_ssb)
# lm4.6 <- lm(social_difference ~ Centrality , 
#             reg_ssb)
# lm5.6 <- lm(climate ~ Centrality, 
#             reg_ssb)
# 
# export_summs(lm1.6, lm2.6, lm3.6, lm4.6, lm5.6, scale = T, 
#              to.file = "docx", file.name = "lm_sentralitet_indeks_1905.docx")
# 
# modelsummary(list("Ethnic Diversity" = lm1.6, "Attitude Change" = lm2.6,
#                   "Threats" = lm3.6, "Social Difference" = lm4.6,
#                   "Climate Change" = lm5.6),
#              vcov = ~kommune,
#              estimate = "{estimate}{stars}",
#              statistic = "std.error",
#              starts = T,
#              fmt = 2,
#              gof_omit = "AIC|BIC|Log\\.Lik|RMSE",
#              output = "PLOTS/reg_sentIndeks_fitWClust.png")

# Modeller sentralitetsindeks fitted ----

# Ethnic Diversity with everything ----
mod1.3 <- lm(diversity ~ educationLevel + household_income + age + gender +
               #Life_Satisfaction + Health + 
               Support_neig + Belonging_neig + SocialR_neig + 
               Centrality, 
             reg_ssb)
summary(mod1.3)
effect_plot(mod1.3, pred = Centrality,
            interval = T,
            rug = T,
            plot.points = T)

mod1.31 <- lm(diversity ~ educationLevel + household_income + age + gender +
               Life_Satisfaction + Health + 
               Support_neig + Belonging_neig + SocialR_neig + 
               #Yrs_in_Neig + 
               Distrust +  Centrality, 
             reg_ssb)

# fit_with_clu_mod1.3 <- coef_test(mod1.3, vcov = "CR1", cluster = reg_ssb$kommune)
# fit_with_clu_mod1.3
# effect_plot(fit_with_clu_mod1.3, Centrality,
#             plot.points = T)
            
# Attitude Change with everything ----
mod2.3 <- lm(attitude_change ~ educationLevel + household_income + age + gender +
               #Life_Satisfaction + Health + 
               Support_neig + Belonging_neig + SocialR_neig + 
               Centrality, 
             reg_ssb)
summary(mod2.3)
effect_plot(mod2.3, pred = Centrality,
            interval = T,
            rug = T,
            plot.points = T)

mod2.31 <- lm(attitude_change ~ educationLevel + household_income + age + gender +
               Life_Satisfaction + Health + 
               Support_neig + Belonging_neig + SocialR_neig + 
               #Yrs_in_Neig + 
               Distrust +  Centrality, 
             reg_ssb)

# fit_with_clu_mod2.3 <- coef_test(mod2.3, vcov = "CR1", cluster = reg_ssb$kommune)
# fit_with_clu_mod2.3

# Threats with everything ----
mod3.3 <- lm(threats ~ educationLevel + household_income + age + gender +
               #Life_Satisfaction + Health + 
               Support_neig + Belonging_neig + SocialR_neig + 
               Centrality, 
             reg_ssb)

mod3.31 <- lm(threats ~ educationLevel + household_income + age + gender +
               Life_Satisfaction + Health + 
               Support_neig + Belonging_neig + SocialR_neig + 
               #Yrs_in_Neig + 
               Distrust +  Centrality, 
             reg_ssb)
summary(mod3.3)
effect_threats <- effect_plot(mod3.3, pred = Centrality,
            interval = T,
            rug = T,
            plot.points = T,
            cluster = reg_ssb$kommune)

fit_with_clu_mod3.3 <- coef_test(mod3.3, vcov = "CR1", cluster = reg_ssb$kommune)
fit_with_clu_mod3.3

# Social Difference with everything ----
mod4.3 <- lm(social_difference~ educationLevel + household_income + age + gender +
               #Life_Satisfaction + Health + 
               Support_neig + Belonging_neig + SocialR_neig + 
               Centrality, 
             reg_ssb)
summary(mod4.3)

mod4.31 <- lm(social_difference~ educationLevel + household_income + age + gender +
               Life_Satisfaction + Health + 
               Support_neig + Belonging_neig + SocialR_neig + 
               #Yrs_in_Neig + 
               Distrust +  Centrality, 
             reg_ssb)

effect_sDiff <- effect_plot(mod4.3, pred = Centrality,
            interval = T,
            rug = T,
            #plot.points = T,
            partial.residuals = T,
            jitter = .2,
            cluster = reg_ssb$kommune)
effect_sDiff
# fit_with_clu_mod4.3 <- coef_test(mod4.3, vcov = "CR1", cluster = reg_ssb$kommune)
# fit_with_clu_mod4.3

# Climate Change with everything ----
mod5.3 <- lm(climate~ educationLevel + household_income + age + gender +
               #Life_Satisfaction + Health + 
               Support_neig + Belonging_neig + SocialR_neig + 
               Centrality, 
             reg_ssb)
summary(mod5.3)

mod5.31 <- lm(climate~ educationLevel + household_income + age + gender +
               Life_Satisfaction + Health + 
               Support_neig + Belonging_neig + SocialR_neig + 
               #Yrs_in_Neig + 
               Distrust +  Centrality, 
             reg_ssb)
effect_plot(mod5.3, pred = Centrality,
            interval = T,
            rug = T,
            plot.points = T,
            cluster = reg_ssb$kommune)

# fit_with_clu_mod5.3 <- coef_test(mod5.3, vcov = "CR1", cluster = reg_ssb$kommune)
# fit_with_clu_mod5.3
summary(reg_ssb)

#### Test Centralisation of services #####
mod6.3 <- lm(S5Q5r17~ educationLevel + household_income + age + gender +
               #Life_Satisfaction + Health + 
               Support_neig + Belonging_neig + SocialR_neig + 
               Centrality, 
             reg_ssb)

mod6.31 <- lm(S5Q5r17~ educationLevel + household_income + age + gender +
               Life_Satisfaction + Health + 
               Support_neig + Belonging_neig + SocialR_neig + 
               #Yrs_in_Neig + 
               Distrust +  Centrality, 
             reg_ssb)
summary(mod6.3)
effect_plot(mod6.3, pred = Centrality,
            interval = T,
            rug = T,
            plot.points = T,
            cluster = reg_ssb$kommune)

##### TESTING TESTING ####
ggplot(reg_data, aes(age, climate))+
  geom_point()+
  geom_smooth()

#Export with everything and clustering #####

modelsummary_all <- modelsummary(list("Ethnic Diversity" = mod1.3, "Attitude Change" = mod2.3,
                  "Threats" = mod3.3, "Social Difference" = mod4.3,
                  "Climate Change" = mod5.3, "Centralisation" = mod6.3),
             vcov = ~kommune,
             estimate = "{estimate}{stars}",
             starts = T,
             fmt = 3,
             coef_map = c("(Intercept)" = "(Intercept)",
                          educationLevel = "Education level", 
                          household_income = "Income",
                          age = "Age (10-year units)",
                          gender1 = "Male (ref: Female)",
                          # Life_Satisfaction = "Life satisfaction",
                          # Health = "Health",
                          Support_neig = "Support in neig",
                          Belonging_neig = "Belonging to neig",
                          SocialR_neig = "SocialR in neig",
                          #Yrs_in_Neig = "Years in neig",
                          #Distrust = "Distrust",
                          #S5Q5r17 = "Centralisation services",
                          Centrality = "Centrality"
             ),
             title = "Background, Neighbourhood, and Centrality Variables",
             gof_omit = "AIC|BIC|Log\\.Lik|RMSE",
             output = "PLOTS/reg_MLR_all_Clust.png")

modelsummary_all


########
modelsummary_with_distrust <- modelsummary(list("Ethnic Diversity" = mod1.31,
                                                "Attitude Change" = mod2.31,
                                                "Threats" = mod3.31, 
                                                "Social Difference" = mod4.31,
                                                "Climate Change" = mod5.31, 
                                                "Centralisation" = mod6.31),
                                 vcov = ~kommune,
                                 estimate = "{estimate}{stars}",
                                 starts = T,
                                 fmt = 3,
                                 coef_map = c("(Intercept)" = "(Intercept)",
                                              educationLevel = "Education level", 
                                              household_income = "Income",
                                              age = "Age (10-year units)",
                                              gender1 = "Male (ref: Female)",
                                              Life_Satisfaction = "Life satisfaction",
                                              Health = "Health",
                                              Support_neig = "Support in neig",
                                              Belonging_neig = "Belonging to neig",
                                              SocialR_neig = "SocialR in neig",
                                              #Yrs_in_Neig = "Years in neig",
                                              Distrust = "Distrust",
                                              Centrality = "Centrality"),
                                 title = "Background, Neighbourhood, Distrust and Centrality Variables",
                                 gof_omit = "AIC|BIC|Log\\.Lik|RMSE",
                                 output = "PLOTS/reg_MLR_all_with_distrust.png")
modelsummary_with_distrust

# modelsummary_docx <- modelsummary(list("Ethnic Diversity" = mod1.3, "Attitude Change" = mod2.3,
#                                   "Threats" = mod3.3, "Social Difference" = mod4.3,
#                                   "Climate Change" = mod5.3),
#                              vcov = ~kommune,
#                              estimate = "{estimate}{stars}",
#                              starts = T,
#                              fmt = 2,
#                              output = "PLOTS/reg_every_fitWClust.docx", dpi = 800)
# modelsummary_docx

