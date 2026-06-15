library(tidyverse)
library(mice)
library(VIM)
library(gridExtra)
library(naniar)

set.seed(123)

clean <- read.csv("…/Master_1.csv") 

Imputation <- clean %>% 
  select(c(record,
           S5Q1, S2Q5, S2Q6r1, S2Q6r2, S2Q6r3, S2Q6r4, S2Q6r5, S2Q6r6, S2Q6r7,
           S2Q7r1,S2Q7r2, S2Q7r3, S2Q8r1, S2Q8r2, S2Q8r3,S2Q11r1, S2Q11r2,
           S2Q11r3, S2Q11r4, S2Q11r5, S2Q11r6,
           S3Q4r1, S3Q4r2, S3Q4r3, S3Q6r4, S3Q6r5,
           S5Q2r1, S5Q2r2, S5Q2r3, S5Q2r4, S5Q2r8,
           S5Q5r1, S5Q5r2,S5Q5r3, S5Q5r4, S5Q5r15, S5Q5r17,
           S4Q4, S4Q3, S5Q3a, S5Q3b, S6Q4, S7Q1, S7Q2)
           )

#### Imputere hele datasettet ####

Imputation %>% 
  summarise(across(everything(), ~sum(is.na(.))))
colMeans(is.na(Imputation))*100 # prosentandel NA's

vis_miss(Imputation)
miss_var_summary(Imputation)
miss_case_summary(Imputation)

imp_pol <- mice(Imputation, m = 10, method = "rf") #Endre navn på objekt

fin_imp <- complete(imp_pol, 1)

# hIncome <- Imputation %>% 
#   ggplot(aes(S5Q5r1))+
#   geom_histogram()
# hIncome_imp <- fin_imp %>% 
#   ggplot(aes(S5Q5r1))+
#   geom_histogram()
# 
# grid.arrange(hIncome, hIncome_imp, ncol = 2)

write.csv(fin_imp, "data_imputated.csv")


#### Local Community ####
nærmiljø <- clean %>% 
  select(record, educationLevel, household_income, living_type, S1Q3, 
         S1Q4r1, S1Q4r2, S1Q4r3, S1Q4r4, S1Q4r5, S1Q4r6,S2Q1, S2Q2, S2Q4r2,
         S2Q4r4, S2Q4r5, S2Q4r6, S2Q5, S2Q6r1, S2Q6r2, S2Q6r3, S2Q6r4, S2Q6r5, 
         S2Q6r6,S2Q6r7, S2Q5, S3Q4r1)

sum(is.na(nærmiljø)) # 1192 NA with 25 variables
which(is.na(nærmiljø))


colMeans(is.na(nærmiljø))*100 # percentage
aggr(nærmiljø, col = c ('navyblue', 'red'),
     numbers = T,
     sortvars = T,
     labels = names (nærmiljø),
     cex.axis = .7,
     gap = 3,
     ylab = c("Histogram of Missing data","Pattern"))

nærmiljø %>% summarise(across(everything(), ~sum(is.na(.))))

nærmiljø_imp <- mice(nærmiljø, m = 5, method = "rf")

finished_nær_imp <- complete(nærmiljø_imp, 1)

write.csv(finished_nær_imp, "imputated_neig.csv")
