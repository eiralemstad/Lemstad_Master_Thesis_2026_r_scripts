library(tidyverse)
library(psych)
library(gt)
library(gtExtras)

data2 <- read.csv("…/Master_1.csv")

sted <- data2 %>%
  select(record, age, gender, zipcode, fylke2024, kommune2024, landsdel2024,
         City_area, City_part, living_type, weight)

##### Polarisation Factor Analysis ####
imputed <- read.csv("…/data_imputated.csv")

# Polariseringsmål - innvandring, klima og sosiale forskjeller. 
pol_fac <- imputed %>% 
  select(S5Q1,S2Q5,
         S2Q7r2, S2Q7r3, S2Q8r2, S2Q8r3,S2Q11r3, S2Q11r5, S2Q11r6,
         S3Q6r4, S3Q6r5,
         S5Q2r1, S5Q2r2, S5Q2r3, S5Q2r4, S5Q2r8, 
         S5Q5r1, S5Q5r2,S5Q5r3, S5Q5r4,
         S4Q4, S4Q3, S5Q3a, S5Q3b, S6Q4)


cortest.bartlett(R = cor(pol_fac), n = 1905)
KMO(pol_fac)

#Factor analysis oblique rotation
paranalysis <- fa.parallel(pol_fac, fa = "fa", fm = "pa", show.legend = F)
print(paranalysis)

paranalysis_SMC <- fa.parallel(pol_fac, fa = "fa", fm = "pa", SMC = T, show.legend = F)
print(paranalysis_SMC)

squaredmc <- smc(pol_fac)
squaredmc
mean(squaredmc)
pol_fac_res <- fa(pol_fac, nfactors = 5, fm = "pa", rotate = "promax")

attributes(pol_fac_res)
pol_fac_res$communality
# Pattern matrix pol ####
print(pol_fac_res$loadings, cutoff = .4) 

pattern <- pol_fac_res$loadings

pattern_matrix_pol <- as.data.frame(unclass(pol_fac_res$loadings))
#fa.diagram(pol_fac_res)

# Factor correlation matrix
# phi <- pol_fac_res$Phi
# phi


pattern_matrix_pol$item <- rownames(pattern_matrix_pol)
pattern_matrix_pol <- pattern_matrix_pol[, c("item", "PA1", "PA2", "PA3", "PA4", "PA5")]


gt_table_pol <- gt(pattern_matrix_pol) %>%
  tab_header(
    title = "Pattern Matrix of Polarisation Factor Loadings",
    subtitle = "Cutoff applied at 0.40"
  ) %>%
  fmt_number(
    columns = starts_with("PA"),
    decimals = 3
  ) %>%
  gt_color_rows(
    columns = starts_with("PA"),
    domain = c(-1, -0.4 | 0.4, 1.026),
    palette = c("Reds")
  ) %>%
  data_color(
    columns = starts_with("PA"),
    colors = scales::col_bin(
      bins = c(-1, -0.4, 0.4, 1.026),
      palette = c("brown", "white", "lightgreen")
    )
  )

gt_table_pol
gtsave(gt_table_pol, filename = "pol_pattern_m_uStandars.png", expand = 20, zoom = 5)

# 
# summary_rows <- data.frame(
#   item = c("SS loadings", "Proportion Var", "Cumulative Var"),
#   t(pol_fac_res$Vaccounted)) # or extract manually
# 
# 
# pattern_matrix <- rbind(pattern_matrix, summary_rows)
# 
# structure_matrix$item <- rownames(structure_matrix)
# structure_matrix <- structure_matrix[, c("item", "PA1", "PA2", "PA3", "PA4", "PA5")]
# 
# 
# tb_structure <- gt(structure_matrix) %>%
#   tab_header(
#     title = "Structure Matrix of Factor Loadings",
#     subtitle = "Cutoff applied at 0.40") %>%
#   fmt_number(
#     columns = starts_with("PA"),
#     decimals = 3) %>%
#   gt_color_rows(
#     columns = starts_with("PA"),
#     domain = c(-1, 1.026),
#     palette = c("Reds")) %>%
#   data_color(
#     columns = starts_with("PA"),
#     colors = scales::col_bin(
#       bins = c(-0.4, -1, 0.399, 1.026),
#       palette = c("brown","white", "lightgreen")
#     )
#   )
# 
# tb_structure
# 
# gtsave(tb_structure, filename = "structure_1.png", expand = 20)

print(pol_fac_res$score.cor)
print(pol_fac_res$uniquenesses)
print(pol_fac_res$communality)
print(pol_fac_res$Structure)

### Making the factors ####
itemlist_pol_fac_uStandard <- list(diversity = c("S2Q7r2", "S2Q7r3", "S2Q8r2", "S2Q8r3", "S2Q11r5"),
                         attitude_change = c("S5Q2r1", "S5Q2r2", "S5Q2r3", "S5Q2r4"),
                         threats = c("S5Q5r1", "S5Q5r2", "S5Q5r4", "S2Q11r3", "S2Q11r6"),
                         social_difference = c("S3Q6r4", "S3Q6r5", "S5Q5r3", "S4Q3"),
                         climate = c("S5Q3a", "S5Q3b", "S6Q4")
)
score_pol_fac_ustandard <- scoreItems(itemlist_pol_fac_uStandard, pol_fac, totals = F)
factors_pol_fac_uS <- as.data.frame(score_pol_fac_ustandard$scores)
#View(factors_pol_fac_uS)

data_wfactors_uStandard <- bind_cols(imputed, factors_pol_fac_uS)
names(data_wfactors_uStandard)


# Binder resultatene fra faktoranalysen til et datasett med stedsindikatorer ####
data_resultat_uS <- bind_cols(sted, factors_pol_fac_uS)
#View(data_resultat_uS)
write.csv(data_resultat_uS, "factor_results_uStandard.csv")

###########################################################
# Neighbourhood and other explanatory variables/factors ###
###########################################################

#
fac_exp <- read.csv("/Users/eiraheleneberglemstad/R_Projects/Master/imputated_neig.csv") %>% 
  select(record, #educationLevel, household_income, living_type, 
    S1Q3, S1Q4r1, S1Q4r2, 
    S1Q4r3, S1Q4r4, S1Q4r5, S1Q4r6, S2Q2, S2Q1,
    S2Q4r2,S2Q4r4, S2Q4r5, S2Q4r6, S2Q5,
    S2Q6r1, S2Q6r2, S2Q6r3, S2Q6r4, S2Q6r5, S2Q6r6,
    S2Q6r7, S2Q5, S3Q4r1)
#View(fac_exp)
# Converting to standard scores / z-scores
# fac_exp_z <- fac_exp %>%
#   mutate(across(#-any_of("record"),
#     everything(),
#     ~ as.numeric(scale(.x))))

cortest.bartlett(R = cor(fac_exp), n = 1905)
KMO(fac_exp)

#Factor analysis oblique rotation
paranalysis_exp <- fa.parallel(fac_exp, fa = "fa", fm = "pa", show.legend = F)
print(paranalysis_exp)

# paranalysis_SMC <- fa.parallel(fac_exp, fa = "fa", fm = "pa", SMC = T, show.legend = F)
# print(paranalysis_SMC)

squaredmc_exp <- smc(fac_exp)
squaredmc_exp
mean(squaredmc_exp)
fac_exp_res_uS <- fa(fac_exp, nfactors = 4, fm = "pa", rotate = "promax")
# HAr prøvd med både 5,6 og 7 faktorer. Alle får flere feil enn 4, heywood problem og loading over 1. 
# Velger heller å gå for 4 som gir mer mening mtp screeplot og velger variabler som er dobbelt til de med høyest verdi.
attributes(fac_exp_res_uS)

# Pattern matrix 
#print(fac_exp_res_uS)
pattern_neig_uS <- fac_exp_res_uS$loadings
print(fac_exp_res_uS$loadings, cutoff = .4) 
pattern_matrix_neig_uS <- as.data.frame(unclass(fac_exp_res_uS$loadings)) #%>% 
#  rownames_to_column("Variable")

names(pattern_matrix_neig_uS)
#OBS skjekk hva som står av names. 
pattern_matrix_neig_uS$item <- rownames(pattern_matrix_neig_uS)
pattern_matrix_neig_uS <- pattern_matrix_neig_uS[, c("item", "PA1", "PA2", "PA3", "PA4"#, "PA5", "PA6"#, "PA7"
)]

fa.diagram(fac_exp_res_uS)

#Making factors and adding them to data frame #####

itemlist_neig_fac_uS <- list(Support_neig = c("S2Q4r4", "S2Q4r5", "S2Q4r6",
                                           "S2Q6r2", "S2Q6r3", "S2Q6r4", "S2Q6r5"),
                          Belonging_neig = c("S1Q4r1", "S1Q4r2", "S1Q4r3", 
                                             "S1Q4r4", "S1Q4r5", "S1Q4r6",
                                             "S2Q6r7", "S3Q4r1"),
                          SocialR_neig = c("S2Q2", "S2Q4r2", "S2Q4r6")#, "S2Q6r6")
                          #Acquaintance_neig = c(, , )  #"S2Q6r7"
)

score_neig_fac_uS <- scoreItems(itemlist_neig_fac_uS, fac_exp, totals = F)
factors_neig_uS <- as.data.frame(score_neig_fac_uS$scores)
#View(factors_neig_fac)

####--------------------- FIKS
data_w_neigfactors_uS <- bind_cols(fac_exp, factors_neig_uS)
names(data_w_neigfactors_uS)

# Binder reultatene fra faktoranalysen til et datasett med stedsindikatorer #
data_fac_resultat_neig_uS <- bind_cols(sted, factors_neig_uS)
#write.csv(data_fac_resultat_neig_uS, ".csv")

# Binder reultatene fra faktoranalysen til et datasett med stedsindikatorer #
data_all_fac_results_uS <- bind_cols(sted, factors_pol_fac_uS, factors_neig_uS)
View(data_all_fac_results_uS)
write.csv(data_all_fac_results_uS, "all_factors_results_uS.csv")

# Table of Pattern Matrix Loadings of Neighbourhood factors####

gt_table_neig_uS <- gt(pattern_matrix_neig_uS) %>%
  tab_header(
    title = "Pattern Matrix of Neighbourhood Factor Loadings",
    subtitle = "Cutoff applied at 0.40"
  ) %>%
  fmt_number(
    columns = starts_with("PA"),
    decimals = 3
  ) %>%
  gt_color_rows(
    columns = starts_with("PA"),
    domain = c(-1, -0.4 | 0.4, 1.026),
    palette = c("Reds")
  ) %>%
  data_color(
    columns = starts_with("PA"),
    colors = scales::col_bin(
      bins = c(-1, -0.4, 0.4, 1.026),
      palette = c("brown", "white", "lightgreen")
    ))

gt_table_neig_uS

gtsave(
  data = gt_table_neig_uS,
  filename = "pattern_matrix_neig_uS.png",
  vwidth = 1600,   # bredde i px
  vheight = 0      # 0 = auto-høyde
)

gtsave(
  data = gt_table_neig_uS,
  filename = "pattern_matrix_neig.docx",
  vwidth = 1600,   # bredde i px
  vheight = 0      # 0 = auto-høyde
)
