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


data_factors <- read.csv("…/all_factors_results_uS.csv")
data_factors <- data_factors %>% 
  select(!c(City_area, City_part))

#Gender mann = 1, 2 = Kvinne class=numeric
data_factors$gender <- recode(data_factors$gender, `1`= 1, `2`= 0)
head(data_factors$gender)
class(data_factors$gender)
data_factors$gender <- as.factor(data_factors$gender)
levels(data_factors$gender)
#Gender mann = 1, 0 = kvinne, class = factor


imputation <- read.csv("/…/data_imputated.csv")
imputation_reg <- imputation %>%
  select(record, S5Q5r17, S7Q1, S7Q2, S4Q4)

imp_neig <- read.csv("/…imputated_neig.csv")
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
SSB <- read.csv("…/sentralitetsindeks_2023-2024_kommuner.csv", sep = ";")
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

mod1.2 <- lm(diversity ~ educationLevel + household_income + age + gender, 
             reg_data)
summary(mod1.2)


# 2 Attitude change in immigration questions ####
mod2.1 <- lm(attitude_change~ Support_neig + Belonging_neig + SocialR_neig, 
             reg_data)
summary(mod2.1)

#
mod2.2 <- lm(attitude_change~ educationLevel + household_income + age + gender, 
             reg_data)
summary(mod2.2)


#3 Threats ####
mod3.1 <- lm(threats~ Support_neig + Belonging_neig + SocialR_neig, 
             reg_data)
summary(mod3.1)

mod3.2 <- lm(threats~ educationLevel + household_income + age + gender, 
             reg_data)
summary(mod3.2)

# 4 Social difference models ####
mod4.1 <- lm(social_difference~ Support_neig + Belonging_neig + SocialR_neig, 
             reg_data)
summary(mod4.1)

mod4.2 <- lm(social_difference ~ educationLevel + household_income + age + gender, 
             reg_data)
summary(mod4.2)

# 5 Attitudes towards climate ####
mod5.1 <- lm(climate~ Support_neig + Belonging_neig + SocialR_neig, 
             reg_data)
summary(mod5.1)


# Bakgrunn
mod5.2 <- lm(climate ~ educationLevel + household_income + age + gender, 
             reg_data)
summary(mod5.2)


# 6 Attitudes towards centralisation ####
mod6.1 <- lm(S5Q5r17~ Support_neig + Belonging_neig + SocialR_neig, 
             reg_data)
summary(mod6.1)


# Bakgrunn
mod6.2 <- lm(S5Q5r17 ~ educationLevel + household_income + age + gender, 
             reg_data)
summary(mod6.2)


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
                                      


# export_summs(mod1.2, mod2.2, mod3.2, mod4.2, mod5.2, scale = T,
#              to.file = "docx", file.name = "reg_MLR_back.docx")

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
                                                   gender1 = "Male (ref: Female)"     
                                      ),
                                      gof_omit = "AIC|BIC|Log\\.Lik|RMSE",
                                      title = "Background",
                                      output = "PLOTS/reg_MLR_back_small.png"
)

tbl_modelsummary_back_small


#--------------- SENTRALITET INDIVID ---------------------


# Modeller sentralitetsindeks fitted ----

# Ethnic Diversity with everything ----
mod1.3 <- lm(diversity ~ educationLevel + household_income + age + gender +
               Support_neig + Belonging_neig + SocialR_neig + 
               Centrality, 
             reg_ssb)
summary(mod1.3)
effect_plot(mod1.3, pred = Centrality,
            interval = T,
            rug = T,
            plot.points = T)


            
# Attitude Change with everything ----
mod2.3 <- lm(attitude_change ~ educationLevel + household_income + age + gender +
               Support_neig + Belonging_neig + SocialR_neig + 
               Centrality, 
             reg_ssb)
summary(mod2.3)
effect_plot(mod2.3, pred = Centrality,
            interval = T,
            rug = T,
            plot.points = T)


# Threats with everything ----
mod3.3 <- lm(threats ~ educationLevel + household_income + age + gender +
               Support_neig + Belonging_neig + SocialR_neig + 
               Centrality, 
             reg_ssb)

summary(mod3.3)
effect_threats <- effect_plot(mod3.3, pred = Centrality,
            interval = T,
            rug = T,
            plot.points = T,
            cluster = reg_ssb$kommune)


# Social Difference with everything ----
mod4.3 <- lm(social_difference~ educationLevel + household_income + age + gender +
               Support_neig + Belonging_neig + SocialR_neig + 
               Centrality, 
             reg_ssb)
summary(mod4.3)


effect_sDiff <- effect_plot(mod4.3, pred = Centrality,
            interval = T,
            rug = T,
            #plot.points = T,
            partial.residuals = T,
            jitter = .2,
            cluster = reg_ssb$kommune)
effect_sDiff

# Climate Change with everything ----
mod5.3 <- lm(climate~ educationLevel + household_income + age + gender +
               Support_neig + Belonging_neig + SocialR_neig + 
               Centrality, 
             reg_ssb)
summary(mod5.3)

effect_plot(mod5.3, pred = Centrality,
            interval = T,
            rug = T,
            plot.points = T,
            cluster = reg_ssb$kommune)

#### Test Centralisation of services #####
mod6.3 <- lm(S5Q5r17~ educationLevel + household_income + age + gender +
               Support_neig + Belonging_neig + SocialR_neig + 
               Centrality, 
             reg_ssb)

summary(mod6.3)
effect_plot(mod6.3, pred = Centrality,
            interval = T,
            rug = T,
            plot.points = T,
            cluster = reg_ssb$kommune)


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
