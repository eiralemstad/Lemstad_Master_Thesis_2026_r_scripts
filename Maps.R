library(ggplot2)
library(tidyverse)
library(sf)
library(spdep)
library(sfdep)

library(tibble)
library(flextable)

library(patchwork)
library(cowplot)

#Må kjøre for at koden til Oslo og Bergen skal fungere
data_hele <- read.csv("/…/all_factors_results_uS.csv")

data_hele$zipcode <- as.character(data_hele$zipcode)
data_hele <- data_hele %>% 
  rename(postnummer = zipcode, kommune = kommune2024) %>% 
  mutate(postnummer = str_pad(str_replace_all(postnummer, "\\s", ""), width = 4,
                              side = "left", pad = "0")) %>% 
  mutate(kommune = str_pad(str_replace_all(kommune, "\\s", ""), width = 4,
                           side = "left", pad = "0"))

imputation <- read.csv("/…/data_imputated.csv")
imputation <- imputation %>%
  select(record, S5Q5r17)

data_hele <- data_hele %>% 
  left_join(imputation, by = "record")

norge_geoJSON <- st_read("/…/GeoJSON_postnummer/Basisdata_0000_Norge_25833_Postnummeromrader_GeoJSON.geojson",
                         layer = "postnummeromrader.postnummeromrade")

norge_geoJSON <- norge_geoJSON %>% 
  select(c(postnummer, poststed, kommune, geometry))


#Fjerne duplikater i hele datasettet (Norge). Det er 69 duplikater der geodata overlapper
summary(norge_geoJSON$postnummer[duplicated(norge_geoJSON$postnummer)]) 

NOR_geo_valid <- st_make_valid(norge_geoJSON) 
NOR_geo_uDup <- NOR_geo_valid %>% 
  group_by(postnummer) %>% 
  summarize(geometry = st_union(geometry), 
            "poststed" = first(poststed),
            "kommune" = first(kommune),
            .groups = "drop")


###### NORGE #####
#Lastet ned GeoJSON fil fra GeoNorge med postnummer for hele landet. Dataens opphav: Posten/Kartverket

# Left join av geodata og spørreundersøkelse
data_norge <- NOR_geo_uDup %>%
  left_join(data_hele, by = c("postnummer","kommune"))

data_norge_uNA <- data_norge %>%
  select(!c(City_area, City_part)) %>%
  na.omit() %>%
  group_by(postnummer) %>%
  summarise(
    geometry = st_union(geometry), #slår sammen polygoner
    n_svar = n_distinct(record[!is.na(weight)]),
    total_weight = sum(weight, na.rm = TRUE),
    kommune = first(kommune),
    landsdel2024 = first(landsdel2024),
    fylke2024 = first(fylke2024),

    mean_diversity   = sum(diversity * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    mean_attitude_change   = sum(attitude_change * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    mean_threats   = sum(threats * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    mean_social_difference   = sum(social_difference * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    mean_climate   = sum(climate * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    mean_cent   = sum(S5Q5r17 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    .groups = "drop")

sum(data_norge_uNA$n_svar<= 3)
# Merging postcode sectors for all study areas #####

norge_større_postnr <- data_norge %>% 
  mutate(postnummer = case_when(
    #Oslo
    postnummer %in% c("0250", "0252") ~"0250_0252",
    postnummer %in% c("0253", "0254") ~ "0253_0254",
    postnummer %in% c("0256", "0257", "0258") ~ "0256_0257_0258",
    postnummer %in% c("0259", "0260") ~ "0259_0260",
    postnummer %in% c("0262", "0263","0264") ~ "0262_0263_0264",
    postnummer %in% c("0265", "0266","0267") ~ "0265_0266_0267",
    postnummer %in% c("0270", "0271","0272") ~ "0270_0271_0272",
    postnummer %in% c("0273", "0278","0279") ~ "0273_0278_0279",
    postnummer %in% c("0275", "0276") ~ "0275_0276",
    postnummer %in% c("0280", "0281") ~ "0280_0281",
    postnummer %in% c("0283", "0284") ~ "0283_0284",
    postnummer %in% c("0286", "0287") ~ "0286_0287",
    
    postnummer %in% c("0550", "0551") ~ "0550_0551",
    postnummer %in% c("0553", "0554", "0555") ~ "0553_54_55",
    postnummer %in% c("0556", "0557") ~ "0556_0557",
    postnummer %in% c("0558", "0559") ~ "0558_0559",
    postnummer %in% c("0561", "0562") ~ "0560_0562",
    postnummer %in% c("0564", "0565", "0566") ~ "0564_65_66",
    postnummer %in% c("0567", "0568", "0569") ~ "0567_68_69",
    postnummer %in% c("0570", "0572", "0573") ~ "0570_72_73",
    postnummer %in% c("0575", "0576") ~ "0575_0576",
    
    postnummer %in% c("0751", "0752", "0753") ~ "0751_52_53",
    postnummer %in% c("0754", "0755", "0756", "0757") ~ "0754_55_56_57",
    postnummer %in% c("0763", "0764", "0765", "0766") ~ "0763_64_65_66",
    postnummer %in% c("0767", "0768") ~ "0767_0768",
    postnummer %in% c("0770", "0771", "0772") ~ "0770_71_72",
    postnummer %in% c("0774", "0775", "0776") ~ "0774_75_76",
    postnummer %in% c("0781", "0782", "0783", "0784") ~ "0781_82_83_84",
    postnummer %in% c("0785", "0786", "0787") ~ "0785_86_87",
    
    postnummer %in% c("0951", "0952", "0953") ~ "0951_52_53",
    postnummer %in% c("0955", "0956") ~ "0955_0956",
    postnummer %in% c("0957", "0958", "0959") ~ "0957_58_59",
    postnummer %in% c("0970", "0971", "0972", "0973", "0976") ~ "0970_71_72_73_76",
    postnummer %in% c("0980", "0981", "0982", "0983") ~ "0980_81_82_83",
    postnummer %in% c("0986", "0987") ~ "0986_0987",
    
    #Bergen
    postnummer %in% c("5055", "5056") ~"5055_5056",
    postnummer %in% c("5052", "5059") ~"5052_5059",
    postnummer %in% c("5089", "5094", "5096") ~ "5089_94_96", 
    postnummer %in% c("5098", "5099") ~"5098_5099",
    
    postnummer %in% c("5101", "5104") ~"5101_5104",
    postnummer %in% c("5107", "5137") ~"5107_5137",
    postnummer %in% c("5113", "5114", "5122") ~"5113_14_22",
    postnummer %in% c("5143", "5154") ~"5143_5154",
    postnummer %in% c("5174", "5178") ~"5174_5178",
    postnummer %in% c("5243", "5244") ~"5243_5244",
    
    #Trondheim
    postnummer %in% c("7082", "7080") ~"7082_7080",
    
    #Innlandet
    postnummer %in% c("2100", "2114") ~"2100_2114",
    postnummer %in% c("2120", "2123") ~"2120_2123",
    postnummer %in% c("2130", "2133") ~"2130_2133",
    #(Kongsvinger)
    postnummer %in% c("2208", "2209", "2210", "2211") ~"2208_09_10_11",
    postnummer %in% c("2213", "2214", "2217") ~"2213_14_17",
    postnummer %in% c("2235", "2240") ~"2235_2240", #grenser ikke
    #(Flisa)
    postnummer %in% c("2260", "2265", "2270") ~"2260_65_70",
    #(Hamar/Mjøs-området)
    postnummer %in% c("2332", "2334", "2335", "2337") ~"2332_34_35_37",
    postnummer %in% c("2340", "2345") ~"2340_2345",
    postnummer %in% c("2355", "2360", "2384") ~"2355_60_84",
    #(Brumunddal)
    postnummer %in% c("2380", "2382", "2383") ~"2380_82_83",
    postnummer %in% c("2385", "2386", "2387", "2388") ~"2385_86_87_88",
    #(Moelv)
    postnummer %in% c("2372", "2390") ~"2372_2390",
    
    #(Elverum)
    postnummer %in% c("2406", "2407", "2408", "2409", "2413", "2414") ~"2406_07_08_09_13_14",
    postnummer %in% c("2410", "2411", "2412") ~"2410_11_12",
    #(Alvdal) 
    postnummer %in% c("2420", "2423", "2428") ~"2420_23_28",
    postnummer %in% c("2484", "2485") ~"2484_2485",
    postnummer %in% c("2500", "2510", "2560") ~"2500_10_60",
    #(Lillehammer/Mjøs-området)
    postnummer %in% c("2607", "2608") ~"2607_2608",
    postnummer %in% c("2609", "2613", "2614") ~"2609_2613_2614",
    postnummer %in% c("2611", "2616") ~"2611_2616",
    postnummer %in% c("2615", "2619", "2624") ~"2615_2619",
    postnummer %in% c("2618", "2625") ~"2618_2625",
    #(Ringebu)
    postnummer %in% c("2630", "2634") ~"2630_2634",
    postnummer %in% c("2635", "2636") ~"2635_2636",
    #(Vinstra)
    postnummer %in% c("2640", "2647") ~"2640_2647",
    #(Vestre Gausdal - grenser til Saksumsdalen)
    postnummer %in% c("2653", "2657") ~"2653_2657",
    #(Lesja)
    postnummer %in% c("2665", "2666", "2669") ~"2665_66_69",
    #(Vågå)
    postnummer %in% c("2680", "2685", "2686") ~"2680_85_86",
    postnummer %in% c("2750", "2760") ~"2750_2760",
    #(Gjøvik)
    postnummer %in% c("2815", "2821") ~"2815_2821",
    postnummer %in% c("2817", "2818") ~"2817_2818",
    postnummer %in% c("2830", "2833", "2835") ~"2830_33_35",
    postnummer %in% c("2836", "2838") ~"2836_2838",
    postnummer %in% c("2840", "2843", "2846") ~"2840_43_46",
    postnummer %in% c("2849", "2850") ~"2849_2850",
    #(Dokka/Valdresområdet)
    postnummer %in% c("2860", "2862", "2863", "2870") ~"2860_62_63_70",
    postnummer %in% c("2890", "2910", "2930") ~"2890_2910_2930",
    postnummer %in% c("2900", "2943") ~"2900_2943",
    
    #Vestlandet gamle Hordaland
    #(Os)
    postnummer %in% c("5208", "5209") ~"5208_5209",
    postnummer %in% c("5200", "5210", "5211") ~"5200_10_11",
    postnummer %in% c("5215", "5216","5217", "5218") ~"5215_16_17_18",
    postnummer %in% c("5281", "5282", "5286") ~"5281_82_86",
    #(Askøy - Sotra - Øygarden)
    postnummer %in% c("5300", "5301", "5308") ~"5300_01_08",
    postnummer %in% c("5302", "5303", "5304") ~"5302_03_04",
    postnummer %in% c("5307", "5310") ~"5307_5310",
    postnummer %in% c("5350", "5353", "5354") ~"5350_53_54",
    postnummer %in% c("5357", "5360") ~"5357_5360",
    postnummer %in% c("5378", "5379", "5380", "5382") ~"5378_79_80_82",
    postnummer %in% c("5396", "5398") ~"5396_5398",
    #(Stord - Bømlo - Kvinnherad)
    postnummer %in% c("5410", "5412") ~"5410_5412",
    postnummer %in% c("5411", "5416", "5417") ~"5411_16_17",
    postnummer %in% c("5420", "5427", "5428") ~"5420_27_28",
    postnummer %in% c("5430", "5437") ~"5430_5437",
    postnummer %in% c("5450", "5452", "5455") ~ "5450_52_54",
    postnummer %in% c("5460", "5462", "5694") ~"5460_62_5694",
    postnummer %in% c("5470", "5498") ~"5470_5498",
    #(Norheimsund)
    postnummer %in% c("5600", "5610") ~"5600_5610",
    postnummer %in% c("5630", "5640", "5641", "5642") ~"5630_40_41_42",
    
    #(Voss)
    postnummer %in% c("5700", "5704") ~"5700_5704",
    postnummer %in% c("5706", "5710", "5736") ~"5706_10_36",
    #(Vaksdal)
    postnummer %in% c("5725", "5727", "") ~"5725_5727",
    
    
    #(Odda)
    postnummer %in% c("5750", "5770") ~"5750_5770",
    #(Alversund/Knarrvik)
    postnummer %in% c("5911", "5914", "5916") ~"5911_14_16",
    postnummer %in% c("5912", "5913", "5915") ~"5912_13_15",
    postnummer %in% c("5917", "5918", "5919") ~"5917_18_19",
    #(Manger/Lindås)
    postnummer %in% c("5936", "5938", "5939") ~"5936_38_39",
    postnummer %in% c("5953", "5955") ~"5953_5955",
    
    #Vestlandet gamle Sogn og fjordane
    #(Måløy/Bremanger)
    postnummer %in% c("6700", "6718") ~"6700_6718",
    postnummer %in% c("6727", "6734") ~"6727_6734",
    postnummer %in% c("6711", "6776") ~"6711_6776",
    #(Nordfjordeid)
    postnummer %in% c("6770", "6773") ~"6770_6773",
    postnummer %in% c("6788", "6797", "6823", "6826") ~"6788_97_6823_26",
    
    #(Førde)
    postnummer %in% c("6800", "6809", "6810", "6814") ~"6800_09_10_14",
    postnummer %in% c("6812", "6819") ~"6812_6819",
    postnummer %in% c("6815", "6817", "6847") ~"6815_17_47",
    #(Sogndal)
    postnummer %in% c("6854", "6856") ~"6854_6856",
    postnummer %in% c("6857", "6863", "6899") ~"6857_63_99",
    postnummer %in% c("6868", "6875") ~"6868_6875",
    postnummer %in% c("6869", "6879") ~"6869_6879",
    postnummer %in% c("6884", "6885") ~"6884_6885",
    #(Florø)
    postnummer %in% c("6900", "6905", "6906") ~"6900_05_06",
    #(Sunnfjord)
    postnummer %in% c("6953", "6957", "5966") ~"6953_57_5966",
    postnummer %in% c("6963", "6973", "6978") ~"6963_73_78",
    postnummer %in% c("6982", "6985") ~"6982_6985",
    #Høyanger
    postnummer %in% c("6993", "6995") ~"6993_6995",
    
    #Troms og Finnmark
    #(Tromsø)
    postnummer %in% c("9008", "9009") ~"9008_9009",
    postnummer %in% c("9011", "9012") ~"9011_9012",
    postnummer %in% c("9018", "9019") ~"9018_9019",
    #()
    postnummer %in% c("9040", "9045") ~"9040_9045",
    postnummer %in% c("9050", "9055") ~"9050_9055",
    postnummer %in% c("9060", "9062") ~"9060_9062",
    #()
    postnummer %in% c("9100", "9101") ~"9100_9101",
    postnummer %in% c("9130", "9132") ~"9130_9132",
    postnummer %in% c("9147", "9154") ~"9147_9154",
    postnummer %in% c("9151", "9152") ~"9151_9152",
    postnummer %in% c("9161", "9162") ~"9161_9162",
    #(Senja)
    postnummer %in% c("9300", "9303") ~"9300_9303",
    postnummer %in% c("9307", "9308", "9309") ~"9307_08_09",
    postnummer %in% c("9321", "9325", "9336") ~"9321_25_36",
    postnummer %in% c("9380", "9385") ~"9380_9385", 
    #(Harstad)
    postnummer %in% c("9404", "9405") ~"9404_9405",
    postnummer %in% c("9406", "9407") ~"9406_9407",
    postnummer %in% c("9408", "9409") ~"9408_9409",
    #(Alta)
    postnummer %in% c("9514", "9515") ~"9514_9515",
    #(Kautokeino)
    postnummer %in% c("9520", "9522", "9528") ~"9520_22_28",
    #()
    postnummer %in% c("9545", "9550") ~"9545_9550",
    #
    postnummer %in% c("9580", "9582") ~"9580_9582",
    #(Hammerfest)
    postnummer %in% c("9612", "9620") ~"9612_9620",
    postnummer %in% c("9672", "9690") ~"9672_9690",
    #(Lakselv)
    postnummer %in% c("9710", "9713") ~"9710_9713",
    #(Karasjok)
    postnummer %in% c("9730", "9731") ~"9730_9731",
    #(Honningsvåg)
    postnummer %in% c("9760", "9765") ~"9760_9765",
    #(Mehamn/Gamik)
    postnummer %in% c("9770", "9773", "9775") ~"9770_73_75",
    #(Vadsø)
    postnummer %in% c("9802", "9804") ~"9802_9804",
    #(Tana)
    postnummer %in% c("9843", "9844") ~"9843_9844",
    #(Kirkenes)
    postnummer %in% c("9910", "9911") ~"9910_9911",
    postnummer %in% c("9926", "9930") ~"9926_9930",
    postnummer %in% c("9980", "9990") ~"9980_9990",
    #(Vardø)
    postnummer %in% c("9950", "9952") ~"9950_9952",
    
    TRUE ~ postnummer))

#####
norge_større_postnr <- norge_større_postnr %>%
  group_by(postnummer) %>%
summarise(
  geometry = st_union(geometry), #slår sammen polygoner
  n_svar = n_distinct(record[!is.na(weight)]),
  total_weight = sum(weight, na.rm = TRUE),
  kommune = first(kommune),
  landsdel2024 = first(landsdel2024),
  fylke2024 = first(fylke2024),
  
  mean_diversity   = sum(diversity * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
  mean_attitude_change   = sum(attitude_change * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
  mean_threats   = sum(threats * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
  mean_social_difference   = sum(social_difference * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
  mean_climate   = sum(climate * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
  mean_cent   = sum(S5Q5r17 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
  .groups = "drop")

norge_postnr_grupper_uNA <- norge_større_postnr %>% 
  na.omit()


# SSB ------
SSB <- read.csv("/…/sentralitetsindeks_2023-2024_kommuner.csv", sep = ";")

SSB <- SSB %>%
  rename(c("kommune" = "knr.2024", "kommune_navn" = "Kommune.2024"))
SSB$kommune <- as.character(SSB$kommune)
class(SSB$kommune)
SSB$kommune[SSB$kommune == "301"] <- "0301"


data_norge_SSB <- SSB %>%
  left_join(norge_større_postnr, by = "kommune")

tbl_merged <- flextable(data.frame(
    Value = c("postcode sectors with 1 respondent", 
              "postcode sectors with 2 respondents", 
              "postcode sectors with 3 respondents"),
    "before_merger" = c(sum(data_norge_uNA$n_svar == 1),
                        sum(data_norge_uNA$n_svar == 2),
                        sum(data_norge_uNA$n_svar == 3)),
    
    "after_merger" = c(sum(norge_postnr_grupper_uNA$n_svar == 1),
                       sum(norge_postnr_grupper_uNA$n_svar == 2),
                       sum(norge_postnr_grupper_uNA$n_svar == 3)))) %>% 
  theme_booktabs() %>%           
  autofit() %>%                  
  set_caption("Results Before and After the Merging of Postcode Sectors") %>% 
  add_footer_lines("Note: Total postcode sectors reduced from 551 to 299") %>% 
  fontsize(size = 10, part = "all")

tbl_merged
#print(tbl_merged, preview = "docx")

### KNN ####
NOR_coo <- st_coordinates(st_point_on_surface(norge_postnr_grupper_uNA))

NOR_kNN_4 <- knn2nb(knearneigh(NOR_coo, k = 4)) # k number nearest neighbors k=4 6 grupper
comp4 <-  n.comp.nb (NOR_kNN_4)
comp4$nc
comp4$comp.id
norge_postnr_grupper_uNA$comp4 <- factor(comp4$comp.id)

comp <- n.comp.nb(NOR_kNN_4)
norge_postnr_grupper_uNA$comp.id <- comp$comp.id
split(norge_postnr_grupper_uNA$postnummer,
      norge_postnr_grupper_uNA$comp.id)

plot(st_geometry(norge_postnr_grupper_uNA),
     col = norge_postnr_grupper_uNA$comp4,
     border = "grey", lwd = 0.005)

plot(st_geometry(norge_postnr_grupper_uNA), border = "lightgray")
plot.nb(NOR_kNN_5, st_geometry(norge_postnr_grupper_uNA), add = TRUE)
NOR_kNN_list <- nb2listw(NOR_kNN_4, style = "W")
#
set.seed(500)
### Moran test mean attitude towards immigrant diversity ### ----

MC_div <- moran.mc(norge_postnr_grupper_uNA$mean_diversity, NOR_kNN_list, 
                   nsim = 999, alternative = "greater")
MC_div
mean(MC_div$res)
hist(MC_div$res)
abline(v = MC_div$statistic, col = "red")

### Moran test mean attitude change in migration questions ####

MC_att <- moran.mc(norge_postnr_grupper_uNA$mean_attitude_change, NOR_kNN_list, 
                   nsim = 999, alternative = "greater")
MC_att
mean(MC_att$res)
hist(MC_att$res)
abline(v = MC_att$statistic, col = "red")

### Moran test mean threats ####

MC_thr <- moran.mc(norge_postnr_grupper_uNA$mean_threats, NOR_kNN_list, 
                   nsim = 999, alternative = "greater")
MC_thr
mean(MC_thr$res)
hist(MC_thr$res)
abline(v = MC_thr$statistic, col = "red")

### Moran test mean attitudes towards social difference ####

MC_sDiff <- moran.mc(norge_postnr_grupper_uNA$mean_social_difference, NOR_kNN_list, 
                   nsim = 999, alternative = "greater")
MC_sDiff
mean(MC_sDiff$res)
hist(MC_sDiff$res)
abline(v = MC_sDiff$statistic, col = "red")

### Moran test mean attitude in questions about climate change ####

MC_cli <- moran.mc(norge_postnr_grupper_uNA$mean_climate, NOR_kNN_list, 
                     nsim = 999, alternative = "greater")
MC_cli
mean(MC_cli$res)
hist(MC_cli$res)
abline(v = MC_cli$statistic, col = "red")

### Moran test mean attitude in questions about centralisation #####
MC_cent <- moran.mc(norge_postnr_grupper_uNA$mean_cent, NOR_kNN_list, 
                   nsim = 999, alternative = "greater")
MC_cent
mean(MC_cent$res)
hist(MC_cent$res)
abline(v = MC_cent$statistic, col = "red")

#Presentere resultater for global Moran's I ####
Monte_C_tbl <- tibble(
  Factor = c("Mean ethnic diversity", "Mean Attitude Change", 
             "Mean Threats", "Mean Social Difference", 
             "Mean Climate Change", "Mean Centralisation"),
  N = c(299, 299, 299, 299, 299, 299),
  W = c("kNN",
        "kNN",
        "kNN",
        "kNN",
        "kNN",
        "kNN"),
  Moran_I_MC = c(0.040, 0.012, -0.018, 0.128, 0.154, 0.307),
  E_I = c(-0.0038, -0.0034, -0.0023, -0.0028, -0.0028, -0.0006),
  nsim = c(999, 999, 999, 999, 999, 999),
  p_value = c("0.115", "0.311", "0.666", "0.001", "0.002", "0.001"))

Monte_C_tbl <- flextable(Monte_C_tbl) %>% 
  theme_booktabs() %>%           
  autofit() %>%                  
  set_caption("Polarisation Factors: Global Moran’s I Monte Carlo Method") %>% 
  add_footer_lines(
    "Note: Calculated with Moran's I (moran.mc), alternative = greater.\nk = 4 and sub-graphs = 6."
  ) %>% 
  fontsize(size = 10, part = "all") 

Monte_C_tbl
save_as_docx(Monte_C_tbl, path = "/…/table_Monte_carlo_GMoran_pol_NOR.docx")

## Norge desiler ####

data_norge_decile <- norge_større_postnr %>% 
  mutate(decile_diversity = ntile(mean_diversity, 10),
         decile_att_change = ntile(mean_attitude_change,10),
         decile_threats = ntile(mean_threats,10),
         decile_social_diff = ntile(mean_social_difference,10),
         decile_climate = ntile(mean_climate,10),
         decile_centr = ntile(mean_cent, 10))

norge_desil <- data_norge_decile %>%
  mutate(decile_diversity = factor(decile_diversity, levels = 1:10, ordered = T)) %>% 
  mutate(decile_att_change = factor(decile_att_change, levels = 1:10, ordered = T)) %>% 
  mutate(decile_threats = factor(decile_threats, levels = 1:10, ordered = T)) %>% 
  mutate(decile_social_diff = factor(decile_social_diff, levels = 1:10, ordered = T)) %>% 
  mutate(decile_climate = factor(decile_climate, levels = 1:10, ordered = T)) %>% 
  mutate(decile_centr = factor(decile_centr, levels = 1:10, ordered = T))


norge_desil_uNA <- norge_desil %>% 
  na.omit()
class(norge_desil)

###### OSLO ######

oslo_geodata <- NOR_geo_uDup %>% 
  filter(kommune == "0301")

data_oslo <- data_hele %>% 
  filter(kommune== "0301") 

# Left join mellom geodata og faktorer
oslo_sum <- oslo_geodata %>%
  left_join(data_oslo, by = c("postnummer", "kommune")) #  uten duplikater, fjernet i Norge seksjonen
# plot oslo City area ----
oslo_sum_plot <- oslo_sum
class(oslo_sum_plot$City_area)
# oslo_sum_plot$City_area <- as.factor(oslo_sum_plot$City_area)
# oslo_sum_plot$City_part <- as.factor(oslo_sum_plot$City_part)
oslo_sum_plot <- oslo_sum_plot %>% 
  mutate(City_part =recode(as.factor(City_part),
                           "4" = "Oslo vest",
                           "17" = "Grunerløkka",
                           "18" = "Grorud",
                           "19" = "Stovner"))


osl_city_areas <- ggplot(oslo_sum_plot) +
  geom_sf(aes(fill = City_part), color = "grey10", linewidth = 0.06) +
  scale_fill_brewer(palette = "Paired", na.value = "grey90")+
  #scale_fill_grey(na.value = "grey90")+
  #scale_fill_viridis_d(option = "E",na.value = "grey90")+
  labs(title = "Study Areas Oslo",
       fill = "area name",
       caption = "Source: GeoNorge") +
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 0, size = 8)) + 
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  scale_x_continuous(n.breaks = 4)+
  scale_y_continuous(n.breaks = 5)

osl_city_areas
#ggsave(osl_city_areas, file = "…/PLOTS/Map_Oslo/osl_city_parts.png", dpi = 500)


### SLÅ SAMMEN POSTNUMMER MANUELT for Oslo, likt som i Norge seksjon ######
oslo_sum <- oslo_sum %>% 
  mutate(postnummer = case_when(postnummer %in% c("0250", "0252") ~"0250_0252",
                                postnummer %in% c("0253", "0254") ~ "0253_0254",
                                postnummer %in% c("0256", "0257", "0258") ~ "0256_0257_0258",
                                postnummer %in% c("0259", "0260") ~ "0259_0260",
                                postnummer %in% c("0262", "0263","0264") ~ "0262_0263_0264",
                                postnummer %in% c("0265", "0266","0267") ~ "0265_0266_0267",
                                postnummer %in% c("0270", "0271","0272") ~ "0270_0271_0272",
                                postnummer %in% c("0273", "0278","0279") ~ "0273_0278_0279",
                                postnummer %in% c("0275", "0276") ~ "0275_0276",
                                postnummer %in% c("0280", "0281") ~ "0280_0281",
                                postnummer %in% c("0283", "0284") ~ "0283_0284",
                                postnummer %in% c("0286", "0287") ~ "0286_0287",
                                
                                postnummer %in% c("0550", "0551") ~ "0550_0551",
                                postnummer %in% c("0553", "0554", "0555") ~ "0553_0554_0555",
                                postnummer %in% c("0556", "0557") ~ "0556_0557",
                                postnummer %in% c("0558", "0559") ~ "0558_0559",
                                postnummer %in% c("0561", "0562") ~ "0560_0562",
                                postnummer %in% c("0564", "0565", "0566") ~ "0564_0565_0566",
                                postnummer %in% c("0567", "0568", "0569") ~ "0567_0568_0569",
                                postnummer %in% c("0570", "0572", "0573") ~ "0570_0572_0573",
                                postnummer %in% c("0575", "0576") ~ "0575_0576",
                                
                                postnummer %in% c("0751", "0752", "0753") ~ "0751_0752_0753",
                                postnummer %in% c("0754", "0755", "0756", "0757") ~ "0754_0755_0756_0757",
                                postnummer %in% c("0763", "0764", "0765", "0766") ~ "0763_0764_0765_0766",
                                postnummer %in% c("0767", "0768") ~ "0767_0768",
                                postnummer %in% c("0770", "0771", "0772") ~ "0770_0771_0772",
                                postnummer %in% c("0774", "0775", "0776") ~ "0774_0775_0776",
                                postnummer %in% c("0781", "0782", "0783", "0784") ~ "0781_0782_0783_0784",
                                postnummer %in% c("0785", "0786", "0787") ~ "0785_0786_0787",
                                
                                postnummer %in% c("0951", "0952", "0953") ~ "0951_0952_0953",
                                postnummer %in% c("0955", "0956") ~ "0955_0956",
                                postnummer %in% c("0957", "0958", "0959") ~ "0957_0958_0959",
                                #postnummer %in% c("0968", "0969") ~ "0968_0969",
                                postnummer %in% c("0970", "0971", "0972", "0973") ~ "0970_0971_0972_0973",
                                postnummer %in% c("0980", "0981", "0982", "0983") ~ "0980_0981_0982_0983",
                                postnummer %in% c("0986", "0987") ~ "0986_0987",
                                
                                TRUE ~ postnummer))

oslo_sum <- oslo_sum %>% 
  group_by(postnummer) %>% 
  summarise( geometry = st_union(geometry), #slår sammen polygoner
             n_svar = n_distinct(record[!is.na(weight)]),
             total_weight = sum(weight, na.rm = TRUE),
             
             kommune = first(kommune),
             landsdel2024 = first(landsdel2024),
             fylke2024 = first(fylke2024),
             City_area = first(City_area),
             City_part = first(City_part),
             
             mean_diversity   = sum(diversity * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
             mean_attitude_change   = sum(attitude_change * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
             mean_threats   = sum(threats * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
             mean_social_difference   = sum(social_difference * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
             mean_climate   = sum(climate * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
             .groups = "drop")




# Map Attitudes towards immigrants ethnic diversity ####
map_oslo_div <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_diversity), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 5)+
  labs(#title = "Ethnic Diversity", 
       fill = "Deciles")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right")+
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))
  #theme(axis.text = element_text(angle = 45, hjust = 1))
map_oslo_div 

map_oslo_div <- map_oslo_div +
  plot_annotation(title = "Attitudes Towards Ethnic Diversity, Oslo",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 12),
                                plot.caption = element_text(hjust = 0)),
                  caption = "Source: GeoNorge")
map_oslo_div
ggsave(map_oslo_div, file = "PLOTS/map_oslo_div.png", dpi = 500)

# Map - attitude change among people around you in immigration questions the last 5 years #####

map_oslo_att_change <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_att_change), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis",
                       na.value = "grey90",
                       direction = -1) +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 5)+
  labs(#title = "Attitude change", 
       fill = "Deciles")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right")+
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))
            
map_oslo_att_change

map_oslo_att_change <- map_oslo_att_change +
  plot_annotation(title = "Attitude Change Towards Immigrants, Oslo",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 12),
                                plot.caption = element_text(hjust = 0)),
                  caption = "Source: GeoNorge")
map_oslo_att_change
ggsave(map_oslo_att_change, file = "PLOTS/map_oslo_att_change.png", dpi = 500)

# Map - immigration as a threat to social cohesion in the Norwegian society #####
# Lavt 1 "ikke bekymret i det hele tatt" - høyt 5 "ekstremt bekymret"
map_oslo_threats <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_threats), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 5)+
  labs( 
       fill = "Deciles")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right")+
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))
        
map_oslo_threats

map_oslo_threats <- map_oslo_threats +
  plot_annotation(title = "Immigrants as a Threat to Norwegian Society, Oslo",
                  theme = theme(plot.title = element_text(hjust = 0.5),
                                plot.caption = element_text(hjust = 0)),
                  caption = "Source: GeoNorge")
map_oslo_threats
ggsave(map_oslo_threats, file = "PLOTS/map_oslo_threats.png", dpi = 500)

# Map - attitudes towards social difference #####
map_oslo_sDiff <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_social_diff), color ="grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 5)+
  labs(#title = "Social Difference", 
       fill = "Deciles")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right")+
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))

map_oslo_sDiff <- map_oslo_sDiff +
  plot_annotation(title = "Attitudes Towards Social Difference, Oslo",
                  #subtitle = "Light Colours - Immigration Friendly Att | Dark Colours - Immigration Hostile Att",
                  theme = theme(plot.title = element_text(hjust = 0.5),
                                plot.caption = element_text(hjust = 0)),
                  caption = "Source: GeoNorge")
map_oslo_sDiff
ggsave(map_oslo_sDiff, file = "PLOTS/map_oslo_sDiff.png", dpi = 500)


# Map - attitudes towards climate change #####
map_oslo_climate <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_climate), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 5)+
  labs( 
       fill = "Deciles")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right")+
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))

map_oslo_climate <- map_oslo_climate +
  plot_annotation(title = "Attitudes Towards Climate Change, Oslo",
                  #subtitle = "Light Colours - Immigration Friendly Att | Dark Colours - Immigration Hostile Att",
                  theme = theme(plot.title = element_text(hjust = 0.5),
                                plot.caption = element_text(hjust = 0)),
                  caption = "Source: GeoNorge")
map_oslo_climate
ggsave(map_oslo_climate, file = "PLOTS/map_oslo_climate.png", dpi = 500)


### Oslo desiler ####

oslo_decile <- oslo_sum %>%
  mutate(decile_diversity = ntile(mean_diversity, 10),
         decile_att_change = ntile(mean_attitude_change,10),
         decile_threats = ntile(mean_threats,10),
         decile_social_diff = ntile(mean_social_difference,10),
         decile_climate = ntile(mean_climate,10))

oslo_decile <- oslo_decile %>%
  mutate(decile_diversity = factor(decile_diversity, levels = 1:10, ordered = T)) %>%
  mutate(decile_att_change = factor(decile_att_change, levels = 1:10, ordered = T)) %>%
  mutate(decile_threats = factor(decile_threats, levels = 1:10, ordered = T)) %>%
  mutate(decile_social_diff = factor(decile_social_diff, levels = 1:10, ordered = T)) %>%
  mutate(decile_climate = factor(decile_climate, levels = 1:10, ordered = T))

oslo_decile_uNA <- oslo_decile %>%
  na.omit()
st_bbox(oslo_decile_uNA)

## Oslo Nabolagsvekter IDW ####

coords_osl <- st_coordinates(st_point_on_surface(oslo_decile))

#Inverted distance weights
keep_oslo <- !is.na(oslo_decile$mean_diversity) # NA is the same for all the factors
oslo_sub_dist <- oslo_decile[keep_oslo, ]
coords_osl_sub <- coords_osl[keep_oslo, ]

#Neighbours based on distance:
nb_sub_oslo_dist <- dnearneigh(coords_osl_sub, d1 = 0, d2 = 2500)
n.comp.nb(nb_sub_oslo_dist)
table(n.comp.nb(nb_sub_oslo_dist)$comp.id)
dlist_sub <- nbdists(nb_sub_oslo_dist, coords_osl_sub)

# IDW weights
lw_idw_sub <- nb2listw(nb_sub_oslo_dist,
                   glist = lapply(dlist_sub, function(x)1/(x^2)),
                   style = "W",
                   zero.policy = T)

plot(st_geometry(oslo_decile_uNA), border = "lightgrey")
plot.nb(nb_sub_oslo_dist, coords_osl_sub, add = T, col = "red")

summary(unlist(dlist_sub))
any(unlist(dlist_sub) == 0)

# Local Gi IDW
oslo_decile_uNA$gi_div_z <- as.numeric(localG_perm(oslo_decile_uNA$mean_diversity, lw_idw_sub, nsim=499, zero.policy = TRUE))
oslo_decile_uNA$gi_div_z

oslo_decile_uNA$gi_attCh_z <- as.numeric(localG_perm(oslo_decile_uNA$mean_attitude_change, lw_idw_sub,nsim=499, zero.policy = TRUE))
oslo_decile_uNA$gi_attCh_z

oslo_decile_uNA$gi_thr_z <- as.numeric(localG_perm(oslo_decile_uNA$mean_threats, lw_idw_sub, nsim=499, zero.policy = TRUE))
oslo_decile_uNA$gi_thr_z

oslo_decile_uNA$gi_sDiff_z <- as.numeric(localG_perm(oslo_decile_uNA$mean_social_difference, lw_idw_sub, nsim=499, zero.policy = TRUE))
oslo_decile_uNA$gi_sDiff_z

oslo_decile_uNA$gi_cli_z <- as.numeric(localG_perm(oslo_decile_uNA$mean_climate, lw_idw_sub, nsim=499, zero.policy = TRUE))
oslo_decile_uNA$gi_cli_z

#######
oslo_decile <- oslo_decile %>% 
  left_join(oslo_decile_uNA %>% st_drop_geometry() %>% 
  select(postnummer, gi_div_z, gi_attCh_z,
         gi_thr_z, gi_sDiff_z, gi_cli_z),
  by = "postnummer")


oslo_tbl_gi <- flextable(oslo_decile_uNA,
                        col_keys = c("postnummer", "n_svar", "gi_div_z", "gi_attCh_z",
                                     "gi_thr_z", "gi_sDiff_z", "gi_cli_z")) %>%
  colformat_double(digits = 2, big.mark = ".") %>% 
  theme_booktabs() %>%
  set_caption("Z-score results from Getis-Ord Gi Test in Oslo")

oslo_tbl_gi <- color(oslo_tbl_gi,~ gi_div_z >= 1.96, ~ gi_div_z, color = "red")
oslo_tbl_gi <- color(oslo_tbl_gi,~ gi_div_z <= -1.96, ~ gi_div_z, color = "blue")

oslo_tbl_gi <- color(oslo_tbl_gi,~ gi_attCh_z >= 1.96, ~ gi_attCh_z, color = "red")
oslo_tbl_gi <- color(oslo_tbl_gi,~ gi_attCh_z <= -1.96, ~ gi_attCh_z, color = "blue")

oslo_tbl_gi <- color(oslo_tbl_gi,~ gi_thr_z >= 1.96, ~ gi_thr_z, color = "red")
oslo_tbl_gi <- color(oslo_tbl_gi,~ gi_thr_z <= -1.96, ~ gi_thr_z, color = "blue")

oslo_tbl_gi <- color(oslo_tbl_gi,~ gi_sDiff_z >= 1.96, ~ gi_sDiff_z, color = "red")
oslo_tbl_gi <- color(oslo_tbl_gi,~ gi_sDiff_z <= -1.96, ~ gi_sDiff_z, color = "blue")

oslo_tbl_gi <- color(oslo_tbl_gi,~ gi_cli_z >= 1.96, ~ gi_cli_z, color = "red")
oslo_tbl_gi <- color(oslo_tbl_gi,~ gi_cli_z <= -1.96, ~ gi_cli_z, color = "blue")
oslo_tbl_gi
save_as_docx(oslo_tbl_gi, path = '/…/oslo_tbl_gi_2500.docx')
# save_as_image(oslo_tbl_gi, path = '/…/PLOTS/Map_Oslo_Gi/oslo_tbl_gi_2500.png')
# save_as_image(oslo_tbl_gi, path = '/…/PLOTS/Map_Oslo_Gi/oslo_tbl_gi_5000.png')


# Oslo Visualisation of Getis Ord Results ####
                       
# Oslo attitudes toward immigrants ethnic diversity ####
oslo_decile <- oslo_decile %>%
  mutate(
    gi_div_class = case_when(is.na(gi_div_z) ~ "No Data",
                          gi_div_z >=  1.96  ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                          gi_div_z <= -1.96  ~ "Coldspot (>= 95%)",
                          TRUE               ~ "Insignificant"),
    gi_div_class = factor(gi_div_class,
                          levels = c("Coldspot (>= 95%)",
                                     "Insignificant",
                                     "Hotspot (>= 95%)",
                                     "No Data")))

map_oslo_gi_div <- ggplot(oslo_decile) +
  geom_sf(aes(fill = gi_div_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c("Coldspot (>= 95%)"="royalblue4",
               "Insignificant" ="snow4",
               "Hotspot (>= 95%)" ="brown4",
               "No Data"       ="grey90"),
    name = "Local Gi") +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 5)+
  labs(title = "Hotspots and coldspots, Oslo: Ethnic Diversity",#)+ #"Clusters Attitudes towards immigrant diversity",
       #subtitle = "Hotspots and coldspots in Oslo",
       caption = "Source: GeoNorge") +
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  theme_linedraw() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0))

map_oslo_gi_div
ggsave(map_oslo_gi_div, file = "PLOTS/Map_Oslo_Gi/map_oslo_gi_div_2500.png", dpi = 700)

# Oslo attitude change toward immigrants #####
oslo_decile <- oslo_decile %>%
  mutate(
    gi_attCh_class = case_when(is.na(gi_attCh_z) ~ "No Data",
                             gi_attCh_z >=  1.96  ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                             gi_attCh_z <= -1.96  ~ "Coldspot (>= 95%)",
                             TRUE               ~ "Insignificant"),
    gi_attCh_class = factor(gi_attCh_class,
                          levels = c("Coldspot (>= 95%)",
                                     "Insignificant",
                                     "Hotspot (>= 95%)",
                                     "No Data")))

map_oslo_gi_attCh <- ggplot(oslo_decile) +
  geom_sf(aes(fill = gi_attCh_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c("Coldspot (>= 95%)"="royalblue4",
               "Insignificant" ="snow4",
               "Hotspot (>= 95%)" ="brown4",
               "No Data"       ="grey90"),
    name = "Local Gi") +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 5)+
  labs(title = "Hotspots and coldspots, Oslo: Attitude Change",#) + #"Clusters Attitude change towards immigrants",
       #subtitle = "Hotspots and coldspots in Oslo",
       caption = "Source: GeoNorge") +
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  theme_linedraw() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0))
map_oslo_gi_attCh
ggsave(map_oslo_gi_attCh, file = "PLOTS/Map_Oslo_Gi/map_oslo_gi_attCh_2500.png", dpi = 700)



# Oslo attitudes "immigrants are a threat to norwegian society" ####
oslo_decile <- oslo_decile %>%
  mutate(
    gi_thr_class = case_when(is.na(gi_thr_z) ~ "No Data",
                          gi_thr_z >=  1.96  ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                          gi_thr_z <= -1.96  ~ "Coldspot (>= 95%)",
                               TRUE               ~ "Insignificant"),
    gi_thr_class = factor(gi_thr_class,
                            levels = c("Coldspot (>= 95%)",
                                       "Insignificant",
                                       "Hotspot (>= 95%)",
                                       "No Data")))

map_oslo_gi_thr <- ggplot(oslo_decile) +
  geom_sf(aes(fill = gi_thr_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c("Coldspot (>= 95%)"="royalblue4",
               "Insignificant" ="snow4",
               "Hotspot (>= 95%)" ="brown4",
               "No Data"       ="grey90"),
    name = "Local Gi") +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 5)+
  labs(title = "Hotspots and Coldspots, Oslo: Threats",#) +#"Clusters Immigrants as a threat",
       #subtitle = "Hotspots and coldspots in Oslo",
       caption = "Source: GeoNorge") +
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  theme_linedraw() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0))
map_oslo_gi_thr
ggsave(map_oslo_gi_thr, file = "PLOTS/Map_Oslo_Gi/map_oslo_gi_thr_2500.png", dpi = 700)

# Oslo attitudes toward social difference #####
oslo_decile <- oslo_decile %>%
  mutate(
    gi_sDiff_class = case_when(is.na(gi_sDiff_z) ~ "No Data",
                               gi_sDiff_z >=  1.96  ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                               gi_sDiff_z <= -1.96  ~ "Coldspot (>= 95%)",
                             TRUE               ~ "Insignificant"),
    gi_sDiff_class = factor(gi_sDiff_class,
                          levels = c("Coldspot (>= 95%)",
                                     "Insignificant",
                                     "Hotspot (>= 95%)",
                                     "No Data")))

map_oslo_gi_sDiff <- ggplot(oslo_decile) +
  geom_sf(aes(fill = gi_sDiff_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c("Coldspot (>= 95%)"="royalblue4",
               "Insignificant" ="snow4",
               "Hotspot (>= 95%)" ="brown4",
               "No Data"       ="grey90"),
    name = "Local Gi") +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 5)+
  labs(title = "Hotspots and coldspots, Oslo: Social Difference",#) +
       #subtitle = "Hotspots and coldspots in Oslo",
       caption = "Source: GeoNorge") +
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  theme_linedraw() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0))
map_oslo_gi_sDiff
ggsave(map_oslo_gi_sDiff, file = "PLOTS/Map_Oslo_Gi/map_oslo_gi_sDiff_2500.png", dpi = 600)

                       
# Oslo attitudes toward Climate Change Gi #####
oslo_decile <- oslo_decile %>%
  mutate(
    gi_cli_class = case_when(is.na(gi_cli_z) ~ "No Data",
                               gi_cli_z >=  1.96  ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                               gi_cli_z <= -1.96  ~ "Coldspot (>= 95%)",
                               TRUE               ~ "Insignificant"),
    gi_cli_class = factor(gi_cli_class,
                            levels = c("Coldspot (>= 95%)",
                                       "Insignificant",
                                       "Hotspot (>= 95%)",
                                       "No Data")))

map_oslo_gi_cli <- ggplot(oslo_decile) +
  geom_sf(aes(fill = gi_cli_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c("Coldspot (>= 95%)"="royalblue4",
               "Insignificant" ="snow4",
               "Hotspot (>= 95%)" ="brown4",
               "No Data"       ="grey90"),
    name = "Local Gi") +
  scale_x_continuous(n.breaks = 5) +
  scale_y_continuous(n.breaks = 5) +
  labs(title = "Hotspots and coldspots,Oslo: Climate Change",
       #subtitle = "Hotspots and coldspots,Oslo:",
       caption = "Source: GeoNorge") +
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  theme_linedraw() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0))
map_oslo_gi_cli
ggsave(map_oslo_gi_cli, file = "PLOTS/Map_Oslo_Gi/map_oslo_gi_cli_2500.png", dpi = 600)


###### TRONDHEIM #####
trondheim_geoJSON <- read_sf("/…/GeoJSON_postnummer/Basisdata_5001_Trondheim_25832_Postnummeromrader_GeoJSON.geojson")
st_layers("/…/GeoJSON_postnummer/Basisdata_5001_Trondheim_25832_Postnummeromrader_GeoJSON.geojson")

trondheim_geoJSON <- trondheim_geoJSON %>% 
  select(c(postnummer, poststed, kommune, geometry))
summary(trondheim_geoJSON$postnummer[duplicated(trondheim_geoJSON$postnummer)]) #0 duplicates

data_trondheim <- data_hele %>% 
  filter(kommune==5001) 

# Left join mellom geodata og faktorer
trondheim_sum <- trondheim_geoJSON %>%
  left_join(data_trondheim, by = c("postnummer", "kommune"))


# Plot City areas -----
trondheim_sum_plot <- trondheim_sum
class(trondheim_sum_plot$City_area)

trondheim_sum_plot <- trondheim_sum_plot %>% 
  mutate(City_area =recode(as.factor(City_area),
                           "4" = "Central city areas",
                           "5" = "Trondheim South"
                           ))
trd_city_areas <- ggplot(trondheim_sum_plot) +
  geom_sf(aes(fill = City_area), color = "grey10", linewidth = 0.06) +
  scale_fill_brewer(palette = "Paired", na.value = "grey90")+
  labs(title = "Study Areas Trondheim",
       fill = "area name",
       caption = "Source: GeoNorge") +
  theme_linedraw()+
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0)) + 
  coord_sf( xlim = c(565634.9, 572160.5),
            ylim = c(7023391.1, 7034921.0))+
  scale_x_continuous(n.breaks = 5) +
  scale_y_continuous(n.breaks = 6)

trd_city_areas
ggsave(trd_city_areas, file = "/…/PLOTS/Map_Trondheim/trd_city_areas.png")

##### Postnummer TRD #####

trondheim_sum <- trondheim_sum %>%
  group_by(postnummer) %>%
  summarise(geometry = st_union(geometry),
            n_svar = n_distinct(record[!is.na(weight)]),
            total_weight = sum(weight, na.rm = TRUE),
            
            kommune = first(kommune),
            landsdel2024 = first(landsdel2024),
            fylke2024 = first(fylke2024),
            City_area = first(City_area),
            City_part = first(City_part),
            
            mean_diversity   = sum(diversity * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            mean_attitude_change   = sum(attitude_change * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            mean_threats   = sum(threats * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            mean_social_difference   = sum(social_difference * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            mean_climate   = sum(climate * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
            .groups = "drop")

             
## Trondheim deciles #####

data_trondheim <- trondheim_sum %>% 
  mutate(decile_diversity = ntile(mean_diversity, 10),
         decile_att_change = ntile(mean_attitude_change,10),
         decile_threats = ntile(mean_threats,10),
         decile_social_diff = ntile(mean_social_difference,10),
         decile_climate = ntile(mean_climate,10))
  
  
trondheim_desil <- data_trondheim %>%
  mutate(decile_diversity = factor(decile_diversity, levels = 1:10, ordered = T)) %>% 
  mutate(decile_att_change = factor(decile_att_change, levels = 1:10, ordered = T)) %>% 
  mutate(decile_threats = factor(decile_threats, levels = 1:10, ordered = T)) %>% 
  mutate(decile_social_diff = factor(decile_social_diff, levels = 1:10, ordered = T)) %>% 
  mutate(decile_climate = factor(decile_climate, levels = 1:10, ordered = T))


trondheim_desil_uNA <- trondheim_desil %>% 
  na.omit()


# Map Attitudes towards immigrants ethnic diversity ####
map_trd_div <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_diversity), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = ("grey90"), 
                       direction = -1) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(n.breaks = 6) +
  labs(title = #"Attitudes towards Immigrants' \nEthnic Background, Trondheim",
         "Ethnic Diversity",
       fill = "Deciles",
       #caption = "Source: GeoNorge"
       )+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0)) + 
  coord_sf( xlim = c(265339.9, 273400.0),
            ylim = c(7030675.6, 7043009.4))#+
  #theme(axis.text = element_text(angle = 45, hjust = 1))

map_trd_div

# Map - attitude change among people around you in immigration questions the last 5 years #####

map_trd_att_change <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_att_change), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = ("grey90"), 
                       direction = -1) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(n.breaks = 6) +
  labs(title = #"Attitude Change Towards\n Immigrants', Trondheim",
         "Attitude Change",
       fill = "Deciles",
       #caption = "Source: GeoNorge"
       )+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0)) + 
  coord_sf( xlim = c(265339.9, 273400.0),
            ylim = c(7030675.6, 7043009.4))#+
#theme(axis.text = element_text(angle = 45, hjust = 1))
map_trd_att_change
#ggsave(map_trd_att_change, file = "map_trd_att_change.png", dpi = 500)

# Map - immigration as a threat to social cohesion in the Norwegian society #####
# Lavt 1 "ikke bekymret i det hele tatt" - høyt 5 "ekstremt bekymret"

map_trd_threats <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_threats), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = ("grey90"), 
                       direction = -1) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(n.breaks = 6) +
  labs(title = #"Immigrants as a Threat to\nNorwegian Society, Trondheim",
         "Threats",
       fill = "Deciles",
       #caption = "Source: GeoNorge"
       )+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0)) + 
  coord_sf( xlim = c(265339.9, 273400.0),
            ylim = c(7030675.6, 7043009.4))

map_trd_threats


# Map - attitudes towards social difference #####
map_trd_sDiff <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_social_diff), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = ("grey90"), 
                       direction = -1) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(n.breaks = 6) +
  labs(title = #"Attitudes Towards Social\nDifference, Trondheim", 
         "Social Difference",
       fill = "Deciles",
       #caption = "Source: GeoNorge"
       )+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0)) + 
  coord_sf( xlim = c(265339.9, 273400.0),
            ylim = c(7030675.6, 7043009.4))

map_trd_sDiff

  
# Map - attitudes towards climate change #####
map_trd_climate <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_climate), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = ("grey90"), 
                       direction = -1) +
  scale_x_continuous(n.breaks = 5) +
  scale_y_continuous(n.breaks = 6) +
  labs(title = "Attitudes Towards\nClimate Change, Trondheim", 
       fill = "Deciles",
       caption = "Source: GeoNorge")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0)) + 
  coord_sf( xlim = c(265339.9, 273400.0),
            ylim = c(7030675.6, 7043009.4))

map_trd_climate
ggsave(map_trd_climate, file = "PLOTS/Map_Trondheim/map_trd_climate.png", dpi = 500)

#####
map_trd_immigration <- map_trd_div + map_trd_att_change+  
  plot_layout(guides = "collect") +
  plot_annotation(title = "Polarisation Factors, Trondheim",
                  #subtitle = "Light Colours - Immigration Friendly | Dark Colours - Immigration Hostile",
                  theme = theme(plot.title = element_text(hjust = 0.5),
                                plot.caption = element_text(hjust = 0)),
                  caption = "Source: GeoNorge")
map_trd_immigration
ggsave(map_trd_immigration, file = "PLOTS/Map_Trondheim/map_trd_immigration.png", dpi = 800)

map_trd_thr_sDiff <- map_trd_threats + map_trd_sDiff +
  plot_layout(guides = "collect")+
  plot_annotation(title = "Polarisation Factors, Trondheim",
                  #subtitle = "Presented in Deciles",
                  theme = theme(plot.title = element_text(hjust = 0.5),
                                plot.caption = element_text(hjust = 0)),
                  caption = "Source: GeoNorge"
  )
map_trd_thr_sDiff
ggsave(map_trd_thr_sDiff, file = "PLOTS/Map_Trondheim/map_trd_thr_sDiff.png", dpi = 600)

## Trondheim neighbougweights IDW ####

coords_trd <- st_coordinates(st_point_on_surface(trondheim_desil))

#Inverted distance weights

keep_trd <- !is.na(trondheim_desil$mean_diversity) # NA is the same for all the factors
trd_sub_dist <- trondheim_desil[keep_trd, ]

coords_trd_sub <- coords_trd[keep_trd, ]

#Naboer basert på avstand:
nb_sub_trd_dist <- dnearneigh(coords_trd_sub, d1 = 0, d2 = 2900)
n.comp.nb(nb_sub_trd_dist)
table(n.comp.nb(nb_sub_trd_dist)$comp.id)
dlist_sub <- nbdists(nb_sub_trd_dist, coords_trd_sub)

# IDW vekter
lw_idw_sub_trd <- nb2listw(nb_sub_trd_dist,
                       glist = lapply(dlist_sub, function(x)1/(x^2)),
                       style = "W",
                       zero.policy = T)

plot(st_geometry(trondheim_desil_uNA), border = "lightgrey")
plot.nb(nb_sub_trd_dist, coords_trd_sub, add = T, col = "red")

summary(unlist(dlist_sub))
any(unlist(dlist_sub) == 0)

# Lokal Gi IDW
trondheim_desil_uNA$gi_div_z <- as.numeric(localG_perm(trondheim_desil_uNA$mean_diversity, lw_idw_sub_trd, zero.policy = TRUE))
trondheim_desil_uNA$gi_div_z

trondheim_desil_uNA$gi_attCh_z <- as.numeric(localG_perm(trondheim_desil_uNA$mean_attitude_change, lw_idw_sub_trd, zero.policy = TRUE))
trondheim_desil_uNA$gi_attCh_z

trondheim_desil_uNA$gi_thr_z <- as.numeric(localG_perm(trondheim_desil_uNA$mean_threats, lw_idw_sub_trd, zero.policy = TRUE))
trondheim_desil_uNA$gi_thr_z

trondheim_desil_uNA$gi_sDiff_z <- as.numeric(localG_perm(trondheim_desil_uNA$mean_social_difference, lw_idw_sub_trd, zero.policy = TRUE))
trondheim_desil_uNA$gi_sDiff_z

trondheim_desil_uNA$gi_cli_z <- as.numeric(localG_perm(trondheim_desil_uNA$mean_climate, lw_idw_sub_trd, zero.policy = TRUE))
trondheim_desil_uNA$gi_cli_z

trondheim_desil <- trondheim_desil %>%
  left_join(trondheim_desil_uNA %>% st_drop_geometry() %>%
              select(postnummer, gi_div_z, gi_attCh_z,
                     gi_thr_z, gi_sDiff_z, gi_cli_z),
            by = "postnummer")

trondheim_desil_uNA <- trondheim_desil %>%
  na.omit()


trd_tbl_gi <- flextable(trondheim_desil_uNA,
                        col_keys = c("postnummer", "n_svar", "gi_div_z", "gi_attCh_z",
                                     "gi_thr_z", "gi_sDiff_z", "gi_cli_z")) %>%
                        colformat_double(digits = 2, big.mark = ".") %>%
                        theme_booktabs() %>%
                        set_caption("Z-scores from Getis-Ord Gi Trondheim")

trd_tbl_gi <- color(trd_tbl_gi,~ gi_div_z >= 1.96, ~ gi_div_z, color = "red")
trd_tbl_gi <- color(trd_tbl_gi,~ gi_div_z <= -1.96, ~ gi_div_z, color = "blue")

trd_tbl_gi <- color(trd_tbl_gi,~ gi_attCh_z >= 1.96, ~ gi_attCh_z, color = "red")
trd_tbl_gi <- color(trd_tbl_gi,~ gi_attCh_z <= -1.96, ~ gi_attCh_z, color = "blue")

trd_tbl_gi <- color(trd_tbl_gi,~ gi_thr_z >= 1.96, ~ gi_thr_z, color = "red")
trd_tbl_gi <- color(trd_tbl_gi,~ gi_thr_z <= -1.96, ~ gi_thr_z, color = "blue")

trd_tbl_gi <- color(trd_tbl_gi,~ gi_sDiff_z >= 1.96, ~ gi_sDiff_z, color = "red")
trd_tbl_gi <- color(trd_tbl_gi,~ gi_sDiff_z <= -1.96, ~ gi_sDiff_z, color = "blue")

trd_tbl_gi <- color(trd_tbl_gi,~ gi_cli_z >= 1.96, ~ gi_cli_z, color = "red")
trd_tbl_gi <- color(trd_tbl_gi,~ gi_cli_z <= -1.96, ~ gi_cli_z, color = "blue")
trd_tbl_gi
save_as_docx(trd_tbl_gi, path = '/…/trd_tbl_gi_2900.docx')


### TRD visual results local Gi Diversity #####
trondheim_desil <- trondheim_desil %>%
  mutate(gi_div_class = case_when(is.na(gi_div_z)     ~ "No Data",
                                  gi_div_z >=  1.96 ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                                  gi_div_z >=  1.66 & gi_div_z<1.96 ~ "Hotspot (>= 90%)",
                                  gi_div_z <= -1.96 ~ "Coldspot (>= 95%)",
                                  gi_div_z <= -1.66 & gi_div_z>-1.96~ "Coldspot (>= 90%)",
                                  TRUE              ~ "Insignificant"),
         gi_div_class = factor(gi_div_class,
                               levels = c("Coldspot (>= 95%)","Insignificant",
                                          "Hotspot (>= 95%)","No Data")))

trd_plot_div <- ggplot(trondheim_desil) +
  geom_sf(aes(fill = gi_div_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c(
      "Coldspot (>= 95%)"="royalblue4",
      "Coldspot (>= 90%)" = "royalblue3",
      "Insignificant" ="snow4",
      "Hotspot (>= 95%)" ="brown4",
      "Hotspot (>= 90%)" = "brown3",
      "No Data"       ="grey90"
    ),
    name = "Legend") +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 7)+
  labs(title = "Hotspots and Coldspots,\nTrondheim: Ethnic Diversity",
       caption = "Source: GeoNorge") +
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0))+
  coord_sf( xlim = c(565634.9, 572160.5),
            ylim = c(7023391.1, 7034921.0))
  
trd_plot_div

### TRD visual results local Gi Attitude change - Immigration #####
trondheim_desil <- trondheim_desil %>%
  mutate(gi_attCh_class = case_when(is.na(gi_attCh_z) ~ "No Data",
                                    gi_attCh_z >=  1.96    ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                                    gi_attCh_z <= -1.96    ~ "Coldspot (>= 95%)",
                                    TRUE                ~ "Insignificant"),
         gi_attCh_class = factor(gi_attCh_class,
                                 levels = c("Coldspot (>= 95%)","Insignificant",
                                            "Hotspot (>= 95%)","No Data")))

trd_plot_attCh <- ggplot(trondheim_desil) +
  geom_sf(aes(fill = gi_attCh_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c("Coldspot (>= 95%)"="royalblue4",
               "Insignificant" ="snow4",
               "Hotspot (>= 95%)" ="brown4",
               "No Data"       ="grey90"),
    name = "Legend") +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 7)+
  labs(title = "Hotspots and Coldspots,\nTrondheim: Attitude Change",
       #"Clusters Attitudes Change towards immigrants",
       #subtitle = "Hotspots and coldspots in Trondheim",
       caption = "Source: GeoNorge") +
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0))+
  coord_sf( xlim = c(565634.9, 572160.5),
            ylim = c(7023391.1, 7034921.0))
  #theme(axis.text = element_text(angle = 45, hjust = 1))

trd_plot_attCh


### TRD visual results local Gi Threats #####
trondheim_desil <- trondheim_desil %>%
  mutate(gi_thr_class = case_when(is.na(gi_thr_z)     ~ "No Data",
                                  gi_thr_z >=  1.96   ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                                  gi_thr_z <= -1.96   ~ "Coldspot (>= 95%)",
                                  TRUE                ~ "Insignificant"),
         gi_thr_class = factor(gi_thr_class,
                               levels = c("Coldspot (>= 95%)","Insignificant",
                                          "Hotspot (>= 95%)","No Data")))

trd_plot_thr <- ggplot(trondheim_desil) +
  geom_sf(aes(fill = gi_thr_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c(
      "Coldspot (>= 95%)"="royalblue4",
      "Insignificant" ="snow4",
      "Hotspot (>= 95%)" ="brown4",
      "No Data"       ="grey90"
    ),
    name = "Legend"
  ) +
  labs(title = "Clusters Attitudes - immigrants as threats",
       subtitle = "Hotspots and coldspots in Trondheim",
       caption = "") +
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12))+
  coord_sf( xlim = c(565634.9, 572160.5),
            ylim = c(7023391.1, 7034921.0))
  #theme(axis.text = element_text(angle = 45, hjust = 1))
trd_plot_thr
### TRD visual results local Gi Social Difference #####
trondheim_desil <- trondheim_desil %>%
  mutate(gi_sDiff_class = case_when(is.na(gi_sDiff_z) ~ "No Data",
                                    gi_sDiff_z >=  1.96    ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                                    gi_sDiff_z <= -1.96    ~ "Coldspot (>= 95%)",
                                    TRUE                ~ "Insignificant"),
         gi_sDiff_class = factor(gi_sDiff_class,
                                 levels = c("Coldspot (>= 95%)","Insignificant",
                                            "Hotspot (>= 95%)","No Data")))

trd_plot_sDiff <- ggplot(trondheim_desil) +
  geom_sf(aes(fill = gi_sDiff_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c("Coldspot (>= 95%)"="royalblue4",
               "Insignificant" ="snow4",
               "Hotspot (>= 95%)" ="brown4",
               "No Data"       ="grey90"),
    name = "Legend") +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 7)+
  labs(title = "Hotspots and Coldspots,\nTrondheim: Social Difference",
       #"Clusters Attitudes towards Social Difference",
       #subtitle = "Hotspots and coldspots in Trondheim",
       caption = "Source:GeoNorge") +
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0))+
  coord_sf( xlim = c(565634.9, 572160.5),
            ylim = c(7023391.1, 7034921.0))
  #theme(axis.text = element_text(angle = 45, hjust = 1))
trd_plot_sDiff

### TRD visual results local Gi Climate #####
trondheim_desil <- trondheim_desil %>%
  mutate(gi_cli_class = case_when(is.na(gi_cli_z) ~ "No Data",
                                  gi_cli_z >=  1.96 ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                                  #gi_cli_z >=  1.66 ~ "Hotspot (>= 95%)",
                                  gi_cli_z <= -1.96 ~ "Coldspot (>= 95%)",
                                  #gi_cli_z <= -1.66 ~ "Coldspot (>= 90%)",
                                  TRUE              ~ "Insignificant"),
         gi_cli_class = factor(gi_cli_class,
                               levels = c("Coldspot (>= 95%)","Insignificant",
                                          "Hotspot (>= 95%)","No Data")))

trd_plot_cli <- ggplot(trondheim_desil) +
  geom_sf(aes(fill = gi_cli_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c("Coldspot (>= 95%)"="royalblue4",
               #"Coldspot (>= 90%)" = "royalblue3",
               "Insignificant" ="snow4",
               "Hotspot (>= 95%)" ="brown4",
               #"Hotspot (>= 90%)" = "brown3",
               "No Data"       ="grey90"),
    name = "Legend") +
  scale_x_continuous(n.breaks = 5)+
  scale_y_continuous(n.breaks = 7)+
  labs(title = "Hotspots and Coldspots,\nTrondheim: Climate Change",
       caption = "Source: GeoNorge") +
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0),
          element_rect(color = "black",
                       linewidth = 0.3))+
  coord_sf( xlim = c(565634.9, 572160.5),
            ylim = c(7023391.1, 7034921.0))#+
  #theme(axis.text = element_text(angle = 45, hjust = 1))
trd_plot_cli
ggsave(trd_plot_cli, file = "trd_plot_cli.png", dpi = 500)

#####
trd_plot_div
ggsave(trd_plot_div, file = "PLOTS/Map_Trondheim_Gi/trd_plot_div_2900.png", dpi = 600)

trd_plot_attCh
ggsave(trd_plot_attCh, file = "PLOTS/Map_Trondheim_Gi/trd_plot_attCh_2900.png", dpi = 600)

trd_plot_thr
trd_plot_sDiff
ggsave(trd_plot_sDiff, file = "PLOTS/Map_Trondheim_Gi/trd_plot_sDiff_2900.png", dpi = 600)

trd_plot_cli
ggsave(trd_plot_cli, file = "PLOTS/Map_Trondheim_Gi/trd_plot_cli_2900.png", dpi = 600)

###### BERGEN #####

bergen_geodata <- NOR_geo_uDup %>% 
  filter(kommune == 4601)

data_bergen <- data_hele %>% 
  filter(kommune==4601) 


# Left join mellom geodata og faktorer
data_bergen <- bergen_geodata %>%
  left_join(data_bergen, by = c("postnummer", "kommune"))

data_bergen_uNA <- data_bergen %>% 
  na.omit()

# Plot Bergen City area/part -----
library(stringr)
brg_sum_plot <- data_bergen
class(brg_sum_plot$City_area)

brg_sum_plot <- brg_sum_plot %>% 
  mutate(
    City_area = case_when(
      is.na(City_area) ~ NA_integer_,
      str_starts(postnummer, "51") & !str_starts(postnummer, "516") ~ 3L, 
      T ~ City_area
    )) %>% 
  
  mutate(City_area =recode(as.factor(City_area),
                           "1" = "Ytrebygda",
                           "2" = "Årstad/Laksevåg",
                           "3" = "Åsane"
  ))

brg_city_areas <- ggplot(brg_sum_plot) +
  geom_sf(aes(fill = City_area), color = "grey10", linewidth = 0.06) +
  scale_fill_brewer(palette = "Paired", na.value = "grey90")+
  labs(title = "Study Areas Bergen",
       fill = "area name",
       caption = "Source: GeoNorge") +
  theme_linedraw()+
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0)) + 
  coord_sf( xlim = c(-43941.12, -26797.03),
            ylim = c(6712127.52, 6749240.28))+
  scale_x_continuous(n.breaks = 4)+
  scale_y_continuous(n.breaks = 5)

brg_city_areas
ggsave(brg_city_areas, file = "/…/PLOTS/Map_Bergen/brg_city_parts.png", dpi = 500)

# Bergen postnummer -----
bergen_postgrupper <- data_bergen %>% 
  mutate(postnummer = case_when(postnummer %in% c("5055", "5056") ~"5055_5056",
                                postnummer %in% c("5052", "5059") ~"5052_5059",
                                postnummer %in% c("5089", "5094", "5096") ~ "5089_94_96", 
                                postnummer %in% c("5098", "5099") ~"5098_5099",
                                
                                postnummer %in% c("5101", "5104") ~"5101_5104",
                                postnummer %in% c("5107", "5137") ~"5107_5137",
                                postnummer %in% c("5113", "5114", "5122") ~"5113_14_22",
                                postnummer %in% c("5143", "5154") ~"5143_5154",
                                postnummer %in% c("5174", "5178") ~"5174_5178",
                                postnummer %in% c("5243", "5244") ~"5243_5244",
                                
                                TRUE ~ postnummer))



bergen_postgrupper <- bergen_postgrupper %>% 
  group_by(postnummer) %>% 
  summarise( geometry = st_union(geometry), #slår sammen polygoner
             n_svar = n_distinct(record[!is.na(weight)]),
             total_weight = sum(weight, na.rm = T),
             
             kommune = first(kommune),
             landsdel2024 = first(landsdel2024),
             fylke2024 = first(fylke2024),

             mean_diversity   = sum(diversity * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
             mean_attitude_change   = sum(attitude_change * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
             mean_threats   = sum(threats * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
             mean_social_difference   = sum(social_difference * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
             mean_climate   = sum(climate * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
             .groups = "drop")

bergen_postgrupper_uNA <- bergen_postgrupper %>% 
  na.omit()

### Bergen deciles #####
bergen_decile <- bergen_postgrupper %>% 
  mutate(decile_diversity = ntile(mean_diversity, 10),
         decile_att_change = ntile(mean_attitude_change,10),
         decile_threats = ntile(mean_threats,10),
         decile_social_diff = ntile(mean_social_difference,10),
         decile_climate = ntile(mean_climate,10))

bergen_decile <- bergen_decile %>%
  #left_join(data_bergen, by = "postnummer") %>% 
  mutate(decile_diversity = factor(decile_diversity, levels = 1:10, ordered = T)) %>% 
  mutate(decile_att_change = factor(decile_att_change, levels = 1:10, ordered = T)) %>% 
  mutate(decile_threats = factor(decile_threats, levels = 1:10, ordered = T)) %>% 
  mutate(decile_social_diff = factor(decile_social_diff, levels = 1:10, ordered = T)) %>% 
  mutate(decile_climate = factor(decile_climate, levels = 1:10, ordered = T))

bergen_decile_uNA <- bergen_decile %>% 
  na.omit()
#####
bergen_NOR_inset <- norge_desil %>% 
  filter(kommune == "4601")

# BRG Map Attitudes towards immigrants ethnic diversity ####
map_brg_div <- ggplot(bergen_NOR_inset) +
  geom_sf(aes(fill = decile_diversity), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1,
                       drop = F) +
  labs(title = "Attitudes towards \nEthnic Diversity, Bergen",
       caption = "Source: GeoNorge",
       fill = "Deciles")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12, color = "black"),
        plot.caption = element_text(hjust = 0))+
  coord_sf( xlim = c(-43941.12, -26797.03),
            ylim = c(6712127.52, 6749240.28))
map_brg_div

ggsave(map_brg_div, file = "PLOTS/Map_Bergen/map_brg_div.png", dpi = 500)

# Map - attitude change among people around you in immigration questions the last 5 years #####

map_brg_att_change <- ggplot(bergen_NOR_inset) +
  geom_sf(aes(fill = decile_att_change), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1,
                       drop = F) +
  labs(title = "Attitude Change \nTowards Immigrants, Bergen",
       caption = "Source: GeoNorge",
       fill = "Deciles")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12, color = "black"),
        plot.caption = element_text(hjust = 0))+
  coord_sf( xlim = c(-43941.12, -26797.03),
            ylim = c(6712127.52, 6749240.28))
map_brg_att_change
ggsave(map_brg_att_change, file = "PLOTS/Map_Bergen/map_brg_att_change.png", dpi = 500)

# Map - immigration as a threat to social cohesion in the Norwegian society #####
# Lavt 1 "ikke bekymret i det hele tatt" - høyt 5 "ekstremt bekymret"
map_brg_threats <- ggplot(bergen_NOR_inset) +
  geom_sf(aes(fill = decile_threats), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1,
                       drop = F) +
  # scale_x_continuous(n.breaks = 4)+
  # scale_y_continuous(n.breaks = 4)+
  labs(title = "Immigration as a Threat, Bergen", 
       caption = "Source: GeoNorge",
       fill = "Deciles")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12, color = "black"),
        plot.caption = element_text(hjust = 0))+
  coord_sf( xlim = c(-43941.12, -26797.03),
            ylim = c(6712127.52, 6749240.28))
map_brg_threats
ggsave(map_brg_threats, file = "PLOTS/Map_Bergen/map_brg_threats.png", dpi = 500)


# Map - attitudes towards social difference #####
map_brg_sDiff <- ggplot(bergen_NOR_inset) +
  geom_sf(aes(fill = decile_social_diff), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1,
                       drop = F) +
  #scale_y_continuous(n.breaks = 5)+
  labs(title = "Attitudes Towards \nSocial Difference, Bergen", 
       caption = "Source: GeoNorge",
       fill = "Deciles")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12, color = "black"),
        plot.caption = element_text(hjust = 0))+
  coord_sf( xlim = c(-43941.12, -26797.03),
            ylim = c(6712127.52, 6749240.28))
map_brg_sDiff
ggsave(map_brg_sDiff, file = "PLOTS/Map_Bergen/map_brg_sDiff.png", dpi = 500)

# BRG Map - attitudes towards climate change #####
map_brg_climate <- ggplot(bergen_NOR_inset) +
  geom_sf(aes(fill = decile_climate), color = "grey10", linewidth = 0.06) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1,
                       drop = F) +
  labs(title = "Attitudes Towards \nClimate Change, Bergen", 
       caption = "Source: GeoNorge",
       fill = "Deciles")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12, color = "black"),
        plot.caption = element_text(hjust = 0))+
  coord_sf( xlim = c(-43941.12, -26797.03),
            ylim = c(6712127.52, 6749240.28))

map_brg_climate
ggsave(map_brg_climate, file = "PLOTS/Map_Bergen/map_brg_climate.png", dpi = 500)


# BRG Local analysis ####

# BRG neighbougweights IDW ####

coords_brg <- st_coordinates(st_centroid(bergen_decile))

keep_brg <- !is.na(bergen_decile$mean_diversity) # NA is the same for all the factors
brg_sub_dist <- bergen_decile[keep_brg, ]

coords_brg_sub <- coords_brg[keep_brg, ]

#Naboer basert på avstand:
nb_sub_brg_dist <- dnearneigh(coords_brg_sub, d1 = 0, d2 = 5000)
n.comp.nb(nb_sub_brg_dist)
table(n.comp.nb(nb_sub_brg_dist)$comp.id)
dlist_sub_brg <- nbdists(nb_sub_brg_dist, coords_brg_sub)

# IDW vekter
lw_idw_sub_brg <- nb2listw(nb_sub_brg_dist,
                           glist = lapply(dlist_sub_brg, function(x)1/(x^2)),
                           style = "W",
                           zero.policy = T)

plot(st_geometry(bergen_decile_uNA), border = "lightgrey")
plot.nb(nb_sub_brg_dist, coords_brg_sub, add = T, col = "red")

summary(unlist(dlist_sub_brg))
any(unlist(dlist_sub_brg) == 0)
# Brg Local Gi IDW ####
bergen_decile_uNA$gi_div_z <- as.numeric(localG_perm(bergen_decile_uNA$mean_diversity, lw_idw_sub_brg, zero.policy = TRUE))
bergen_decile_uNA$gi_div_z

bergen_decile_uNA$gi_attCh_z <- as.numeric(localG_perm(bergen_decile_uNA$mean_attitude_change, lw_idw_sub_brg, zero.policy = TRUE))
bergen_decile_uNA$gi_attCh_z

bergen_decile_uNA$gi_thr_z <- as.numeric(localG_perm(bergen_decile_uNA$mean_threats, lw_idw_sub_brg, zero.policy = TRUE))
bergen_decile_uNA$gi_thr_z

bergen_decile_uNA$gi_sDiff_z <- as.numeric(localG_perm(bergen_decile_uNA$mean_social_difference, lw_idw_sub_brg, zero.policy = TRUE))
bergen_decile_uNA$gi_sDiff_z

bergen_decile_uNA$gi_cli_z <- as.numeric(localG_perm(bergen_decile_uNA$mean_climate, lw_idw_sub_brg, zero.policy = TRUE))
bergen_decile_uNA$gi_cli_z

bergen_decile <- bergen_decile %>% 
  left_join(bergen_decile_uNA %>% st_drop_geometry() %>% 
              select(postnummer, n_svar, gi_div_z, gi_attCh_z, 
                     gi_thr_z, gi_sDiff_z, gi_cli_z),
            by = "postnummer")

bergen_decile_uNA <- bergen_decile_uNA %>%  #bruke bergen_decile i stedenfor? --> bergen_decile_uNA
 na.omit()

# Brg Table Local G
brg_tbl_gi <- flextable(bergen_decile_uNA,
  col_keys = c("postnummer", "n_svar", "gi_div_z", "gi_attCh_z", 
               "gi_thr_z", "gi_sDiff_z", "gi_cli_z")) %>% 
  colformat_double(digits = 2, big.mark = ".") %>% 
  theme_booktabs() %>% 
  set_caption("Z-scores from Getis-Ord Gi Bergen")

brg_tbl_gi <- color(brg_tbl_gi,~ gi_div_z >= 1.96, ~ gi_div_z, color = "red")
brg_tbl_gi <- color(brg_tbl_gi,~ gi_div_z <= -1.96, ~ gi_div_z, color = "blue")

brg_tbl_gi <- color(brg_tbl_gi,~ gi_attCh_z >= 1.96, ~ gi_attCh_z, color = "red")
brg_tbl_gi <- color(brg_tbl_gi,~ gi_attCh_z <= -1.96, ~ gi_attCh_z, color = "blue")

brg_tbl_gi <- color(brg_tbl_gi,~ gi_thr_z >= 1.96, ~ gi_thr_z, color = "red")
brg_tbl_gi <- color(brg_tbl_gi,~ gi_thr_z <= -1.96, ~ gi_thr_z, color = "blue")

brg_tbl_gi <- color(brg_tbl_gi,~ gi_sDiff_z >= 1.96, ~ gi_sDiff_z, color = "red")
brg_tbl_gi <- color(brg_tbl_gi,~ gi_sDiff_z <= -1.96, ~ gi_sDiff_z, color = "blue")

brg_tbl_gi <- color(brg_tbl_gi,~ gi_cli_z >= 1.96, ~ gi_cli_z, color = "red")
brg_tbl_gi <- color(brg_tbl_gi,~ gi_cli_z <= -1.96, ~ gi_cli_z, color = "blue")

brg_tbl_gi
save_as_docx(brg_tbl_gi, path = '/…/brg_tbl_gi.docx')

### BRG results local Gi Diversity #####
bergen_decile <- bergen_decile %>%
  mutate(gi_div_class = case_when(is.na(gi_div_z)     ~ "No Data",
                                  gi_div_z >=  1.96       ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                                  gi_div_z <= -1.96       ~ "Coldspot (>= 95%)",
                          TRUE                ~ "Insignificant"),
         gi_div_class = factor(gi_div_class,
                      levels = c("Coldspot (>= 95%)","Insignificant",
                                 "Hotspot (>= 95%)","No Data")))

map_brg_gi_div <- ggplot(bergen_decile) +
  geom_sf(aes(fill = gi_div_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c(
      "Coldspot (>= 95%)"="royalblue4",
      "Insignificant" ="snow4",
      "Hotspot (>= 95%)" ="brown4",
      "No Data"       ="grey95"
    ),
    name = "Legend") +
  coord_sf( xlim = c(-43941.12, -26797.03),
            ylim = c(6712127.52, 6749240.28))+
  labs(title = "Hotspots and Coldspots,\nBergen: Ethnic Diversity",
       caption = "Source: GeoNorge")+
  theme_linedraw()+
  theme(legend.position = "right", 
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0))
map_brg_gi_div
ggsave(map_brg_gi_div, file = "PLOTS/Map_Bergen_Gi/map_brg_gi_div_5000.png", dpi = 500)

### BRG results local Gi Attitude change - Immigration #####
bergen_decile <- bergen_decile %>%
  mutate(gi_attCh_class = case_when(is.na(gi_attCh_z) ~ "No Data",
                               gi_attCh_z >=  1.96    ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                               gi_attCh_z <= -1.96    ~ "Coldspot (>= 95%)",
                                  TRUE                ~ "Insignificant"),
         gi_attCh_class = factor(gi_attCh_class,
                               levels = c("Coldspot (>= 95%)","Insignificant",
                                          "Hotspot (>= 95%)","No Data")))

map_brg_gi_attCh <- ggplot(bergen_decile) +
  geom_sf(aes(fill = gi_attCh_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c("Coldspot (>= 95%)"="royalblue4",
               "Insignificant" ="snow4",
               "Hotspot (>= 95%)" ="brown4",
               "No Data"       ="grey95"),
    name = "Legend") +
  coord_sf( xlim = c(-43941.12, -26797.03),
            ylim = c(6712127.52, 6749240.28))+
  labs(title = "Hotspots and Coldspots,\nBergen: Attitude Change",
       caption = "Source: GeoNorge") +
  theme_linedraw()+
  theme(plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0),
        legend.position = "right"
        )
map_brg_gi_attCh
ggsave(map_brg_gi_attCh, file="PLOTS/Map_Bergen_Gi/map_brg_gi_attCh_5000.png", dpi = 500)
                           
### BRG results local Gi Threats #####
bergen_decile <- bergen_decile %>%
  mutate(gi_thr_class = case_when(is.na(gi_thr_z)     ~ "No Data",
                                  gi_thr_z >=  1.96   ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                                  gi_thr_z <= -1.96   ~ "Coldspot (>= 95%)",
                                  TRUE                ~ "Insignificant"),
         gi_thr_class = factor(gi_thr_class,
                               levels = c("Coldspot (>= 95%)","Insignificant",
                                          "Hotspot (>= 95%)","No Data")))

map_brg_gi_thr <- ggplot(bergen_decile) +
  geom_sf(aes(fill = gi_thr_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c(
      "Coldspot (>= 95%)"="royalblue4",
      "Insignificant" ="snow4",
      "Hotspot (>= 95%)" ="brown4",
      "No Data"       ="grey95"
    ),
    name = "Legend") +
  coord_sf( xlim = c(-43941.12, -26797.03),
            ylim = c(6712127.52, 6749240.28))+
  labs(title = "Hotspots and Coldspots,\nBergen: Threats",
       caption = "Source: GeoNorge") +
  theme_linedraw()+
  theme(plot.title = element_text(size = 12),
        legend.position = "right", 
        # legend.background = 
        #   element_rect(color = "black",
        #                linewidth = 0.3),
        plot.caption = element_text(hjust = 0))
map_brg_gi_thr
ggsave(map_brg_gi_thr, file="PLOTS/Map_Bergen_Gi/map_brg_gi_thr_5000.png", dpi = 500)
                           
### BRG results local Gi Social Difference #####
bergen_decile <- bergen_decile %>%
  mutate(gi_sDiff_class = case_when(is.na(gi_sDiff_z) ~ "No Data",
                                    gi_sDiff_z >=  1.96    ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                                    gi_sDiff_z <= -1.96    ~ "Coldspot (>= 95%)",
                                    TRUE                ~ "Insignificant"),
         gi_sDiff_class = factor(gi_sDiff_class,
                                 levels = c("Coldspot (>= 95%)","Insignificant",
                                            "Hotspot (>= 95%)","No Data")))

map_brg_gi_sDiff <- ggplot(bergen_decile) +
  geom_sf(aes(fill = gi_sDiff_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c("Coldspot (>= 95%)"="royalblue4",
               "Insignificant" ="snow4",
               "Hotspot (>= 95%)" ="brown4",
               "No Data"       ="grey95"),
    name = "Legend") +
  coord_sf( xlim = c(-43941.12, -26797.03),
            ylim = c(6712127.52, 6749240.28))+
  labs(title = "Hotspots and Coldspots,\nBergen: Social Difference",
       caption = "Source: GeoNorge") +
  theme_linedraw()+
  theme(legend.position = "right",
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0)) 
     
map_brg_gi_sDiff
ggsave(map_brg_gi_sDiff, file = "PLOTS/Map_Bergen_Gi/map_brg_gi_sDiff_5000.png", dpi = 500)

### BRG results local Gi Climate #####
bergen_decile <- bergen_decile %>%
  mutate(gi_cli_class = case_when(is.na(gi_cli_z) ~ "No Data",
                                  gi_cli_z >=  1.96    ~ "Hotspot (>= 95%)", #1.96 er z-score med signifikansnivå 95%
                                  gi_cli_z <= -1.96    ~ "Coldspot (>= 95%)",
                                    TRUE                ~ "Insignificant"),
         gi_cli_class = factor(gi_cli_class,
                                 levels = c("Coldspot (>= 95%)","Insignificant",
                                            "Hotspot (>= 95%)","No Data")))

map_brg_gi_cli <- ggplot(bergen_decile) +
  geom_sf(aes(fill = gi_cli_class), color = "grey10", size = 0.06) +
  scale_fill_manual(
    values = c("Coldspot (>= 95%)"="royalblue4",
               "Insignificant" ="snow4",
               "Hotspot (>= 95%)" ="brown4",
               "No Data"       ="grey95"),
    name = "Legend") +
  coord_sf( xlim = c(-43941.12, -26797.03),
            ylim = c(6712127.52, 6749240.28))+
  labs(title = "Hotspots and Coldspots,\nBergen: Climate Change",
       caption = "Source: GeoNorge") +
  theme_linedraw()+
  theme(legend.position = "right", 
        
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0))
map_brg_gi_cli
ggsave(map_brg_gi_cli, file = "PLOTS/Map_Bergen_Gi/map_brg_gi_cli_5000.png", dpi = 500)

# BRG Gi results Visualisation maps ####
map_brg_gi_div
map_brg_gi_attCh
map_brg_gi_thr
map_brg_gi_sDiff
map_brg_gi_cli


#------------ FINAL PLOTS NORWAY --------
#-------------- INSET MAPS ---------
#https://cran.r-project.org/web/packages/insetplot/vignettes/insetplot.html
st_bbox(norge_desil_uNA)


bergen_NOR_inset <- norge_desil %>% 
  filter(kommune == "4601")

#st_bbox(oslo_decile)
bbox_osl <- data.frame( xmin = 255497.9,
                        xmax = 273925.6,
                        ymin = 6645956.6,
                        ymax = 6659321.8)

#st_bbox(bergen_decile)
#st_crs(bergen_decile)
bbox_brg <- data.frame( xmin = -43941.12,
                        xmax = -11477.19,
                        ymin = 6712127.52,
                        ymax = 6750119.13)


bbox_trd <- data.frame( xmin = 250339.9, 
                        xmax = 286481.0,
                        ymin = 7010675.6,
                        ymax = 7051059.4)

bbox_trd_32 <- data.frame( xmin = 550180.1, 
                           xmax = 587461.0,
                           ymin = 7004531.7,
                           ymax = 7044010.0)


#st_bbox(svalbard)
bbox_sval <- data.frame (xmin = 379976.4, 
                         xmax = 868234.7,
                         ymin = 8462871.1,
                         ymax = 9000718.8)
norge_uSval <- norge_desil %>% 
  filter(!norge_desil$postnummer == "9170")
#st_bbox(norge_uSval)

##### DIV ------
NOR_map_div <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_diversity), color = "NA")+
  geom_rect(data = bbox_osl, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_brg, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_trd, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  labs(title = "Ethnic diversity", 
       fill = "Deciles",
       caption = "Source: GeoNorge. Inset maps not to scale")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0, size = 8))+
  coord_sf( xlim = c(-371225.4, 1115103.1),
            ylim = c(6440598.3, 8070610.9))
NOR_map_div

# Inset DIVERSITY ----
map_oslo_div_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_diversity), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Oslo",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_oslo_div_inset 

map_brg_div_inset <- ggplot(bergen_NOR_inset) +
  geom_sf(aes(fill = decile_diversity), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1,
                       drop = F) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(-42041.12, -26100),
            ylim = c(6712127.52, 6750119.13))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Bergen",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_brg_div_inset 

map_trd_div_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_diversity), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(265339.9, 273400.0),
            ylim = c(7030675.6, 7043009.4))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Trondheim",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_trd_div_inset

map_sval_div_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_diversity), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(379976.4, 868234.7),
            ylim = c(8462871.1, 9000718.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Svalbard",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_sval_div_inset

final_NOR_map_div <- NOR_map_div +
  inset_element(
    map_oslo_div_inset, 
    left = 0.60, bottom = 0.002, right = 0.95, top = 0.40
  ) +
  inset_element(
    map_brg_div_inset,
    left = 0.0000000001, bottom = 0.005, right = 0.23, top = 0.35
  ) + 
  inset_element(
    map_trd_div_inset,
    left = 0.0000001, bottom = 0.39, right = 0.28, top = 0.65
  ) + 
  inset_element(
      map_sval_div_inset,
    left = 0.07, bottom = 0.70, right = 0.29, top = 0.9999999
  )

final_NOR_map_div
ggsave(final_NOR_map_div, file = "PLOTS/final_NOR_map_div.png", dpi = 900)


##### ATT_CH ----
NOR_map_att_change <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_att_change), color = NA) +
  geom_rect(data = bbox_osl, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  geom_rect(data = bbox_brg, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_trd, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  labs(title = "Attitude change", 
       fill = "Deciles",
       caption = "Source: GeoNorge. Inset maps not to scale")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0, size = 8))+
  coord_sf( xlim = c(-371225.4, 1115103.1),
            ylim = c(6440598.3, 8070610.9))

NOR_map_att_change

# Inset attCh ----
map_oslo_attCh_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_att_change), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Oslo",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_oslo_attCh_inset 

map_brg_attCh_inset <- ggplot(bergen_NOR_inset) +
  geom_sf(aes(fill = decile_att_change), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1,
                       drop = F) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(-42041.12, -26100),
            ylim = c(6712127.52, 6750119.13))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Bergen",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_brg_attCh_inset 

map_trd_attCh_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_att_change), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(265339.9, 273400.0),
            ylim = c(7030675.6, 7043009.4))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Trondheim",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_trd_attCh_inset

map_sval_attCh_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_att_change), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(379976.4, 868234.7),
            ylim = c(8462871.1, 9000718.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Svalbard",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_sval_attCh_inset

final_NOR_map_attCh <- NOR_map_att_change +
  inset_element(
    map_oslo_attCh_inset, 
    left = 0.60, bottom = 0.002, right = 0.95, top = 0.40
  ) +
  inset_element(
    map_brg_attCh_inset,
    left = 0.0000000001, bottom = 0.005, right = 0.23, top = 0.35
  ) + 
  inset_element(
    map_trd_attCh_inset,
    left = 0.0000001, bottom = 0.39, right = 0.28, top = 0.65
  ) + 
  inset_element(
    map_sval_attCh_inset,
    left = 0.07, bottom = 0.70, right = 0.29, top = 0.9999999
  )

final_NOR_map_attCh
ggsave(final_NOR_map_attCh, file = "PLOTS/final_NOR_map_attCh.png", dpi = 900)


##### Threats -----
NOR_map_threats <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_threats), color = "white", linewidth = 0.0001) +
  geom_rect(data = bbox_osl, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_brg, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_trd, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  labs(title = "Threats", 
       fill = "Deciles",
       caption = "Source: GeoNorge. Inset maps not to scale")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0, size = 8))+
  coord_sf( xlim = c(-371225.4, 1115103.1),
            ylim = c(6440598.3, 8070610.9))
NOR_map_threats

# Inset Thr -----

map_oslo_thr_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_threats), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Oslo",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_oslo_thr_inset 

map_brg_thr_inset <- ggplot(bergen_NOR_inset) +
  geom_sf(aes(fill = decile_threats), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1,
                       drop = F) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(-42041.12, -26100),
            ylim = c(6712127.52, 6750119.13))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Bergen",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_brg_thr_inset 

map_trd_thr_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_threats), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(265339.9, 273400.0),
            ylim = c(7030675.6, 7043009.4))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Trondheim",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_trd_thr_inset

map_sval_thr_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_threats), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(379976.4, 868234.7),
            ylim = c(8462871.1, 9000718.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Svalbard",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_sval_thr_inset

final_NOR_map_thr <- NOR_map_threats +
  inset_element(
    map_oslo_thr_inset, 
    left = 0.60, bottom = 0.002, right = 0.95, top = 0.40
  ) +
  inset_element(
    map_brg_thr_inset,
    left = 0.0000000001, bottom = 0.005, right = 0.23, top = 0.35
  ) + 
  inset_element(
    map_trd_thr_inset,
    left = 0.0000001, bottom = 0.39, right = 0.28, top = 0.65
  ) + 
  inset_element(
    map_sval_thr_inset,
    left = 0.07, bottom = 0.70, right = 0.29, top = 0.9999999
  )

final_NOR_map_thr
ggsave(final_NOR_map_thr, file = "PLOTS/final_NOR_map_thr.png", dpi = 900)
###### SDIFF ----
NOR_map_sDiff <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_social_diff), color = "white", linewidth = 0.0001) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  geom_rect(data = bbox_osl, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_brg, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_trd, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  labs(title = "Social Difference", 
       fill = "Deciles",
       caption = "Source: GeoNorge. Inset maps not to scale")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0, size = 8))+
  coord_sf( xlim = c(-371225.4, 1115103.1),
            ylim = c(6440598.3, 8070610.9))
NOR_map_sDiff

# Inset sDiff ----

map_oslo_sDiff_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_social_diff), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Oslo",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_oslo_sDiff_inset

map_brg_sDiff_inset <- ggplot(bergen_NOR_inset) +
  geom_sf(aes(fill = decile_social_diff), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1,
                       drop = F) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(-42041.12, -26100),
            ylim = c(6712127.52, 6750119.13))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Bergen",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_brg_sDiff_inset 


map_trd_sDiff_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_social_diff), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(265339.9, 273400.0),
            ylim = c(7030675.6, 7043009.4))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Trondheim",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_trd_sDiff_inset

map_sval_sDiff_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_social_diff), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(379976.4, 868234.7),
            ylim = c(8462871.1, 9000718.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Svalbard",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_sval_sDiff_inset

final_NOR_map_sDiff <- NOR_map_sDiff +
  inset_element(
    map_oslo_sDiff_inset, 
    left = 0.60, bottom = 0.002, right = 0.95, top = 0.40
  ) +
  inset_element(
    map_brg_sDiff_inset,
    left = 0.0000000001, bottom = 0.005, right = 0.23, top = 0.35
  ) + 
  inset_element(
    map_trd_sDiff_inset,
    left = 0.0000001, bottom = 0.39, right = 0.28, top = 0.65
  ) + 
  inset_element(
    map_sval_sDiff_inset,
    left = 0.07, bottom = 0.70, right = 0.29, top = 0.9999999
  )

final_NOR_map_sDiff
ggsave(final_NOR_map_sDiff, file = "PLOTS/final_NOR_map_sDiff.png", dpi = 900)
##### Climate -----
NOR_map_climate <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_climate), color = "white", linewidth = 0.0001) +
  geom_rect(data = bbox_osl, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_brg, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_trd, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  labs(title = "Climate Change", 
       fill = "Deciles",
       caption = "Source: GeoNorge. Inset maps not to scale")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0, size = 8))+
  coord_sf( xlim = c(-371225.4, 1115103.1),
            ylim = c(6440598.3, 8070610.9))
NOR_map_climate

# Inset Climate ----

map_oslo_cli_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_climate), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Oslo",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_oslo_cli_inset 

map_brg_cli_inset <- ggplot(bergen_NOR_inset) +
  geom_sf(aes(fill = decile_climate), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1,
                       drop = F) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(-42041.12, -26100),
            ylim = c(6712127.52, 6750119.13))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Bergen",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_brg_cli_inset 

map_trd_cli_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_climate), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(265339.9, 273400.0),
            ylim = c(7030675.6, 7043009.4))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Trondheim",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_trd_cli_inset

map_sval_cli_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_climate), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(379976.4, 868234.7),
            ylim = c(8462871.1, 9000718.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Svalbard",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_sval_cli_inset

final_NOR_map_cli <- NOR_map_climate +
  inset_element(
    map_oslo_cli_inset, 
    left = 0.60, bottom = 0.002, right = 0.95, top = 0.40
  ) +
  inset_element(
    map_brg_cli_inset,
    left = 0.0000000001, bottom = 0.005, right = 0.23, top = 0.35
  ) + 
  inset_element(
    map_trd_cli_inset,
    left = 0.0000001, bottom = 0.39, right = 0.28, top = 0.65
  ) + 
  inset_element(
    map_sval_cli_inset,
    left = 0.07, bottom = 0.70, right = 0.29, top = 0.9999999
  )

final_NOR_map_cli
ggsave(final_NOR_map_cli, file = "PLOTS/final_NOR_map_cli.png", dpi = 900)

##### Centralisation ------
NOR_map_cent <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_centr), color = "NA")+
  geom_rect(data = bbox_osl, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_brg, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_trd, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  labs(title = "Centralisation", 
       fill = "Deciles",
       caption = "Source: GeoNorge. Inset maps not to scale")+
  guides(fill = guide_legend(reverse = T))+
  theme_linedraw()+
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0, size = 8))+
  coord_sf( xlim = c(-371225.4, 1115103.1),
            ylim = c(6440598.3, 8070610.9))
NOR_map_cent

# Inset Centralisation ----
map_oslo_cent_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_centr), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Oslo",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_oslo_cent_inset 

map_brg_cent_inset <- ggplot(bergen_NOR_inset) +
  geom_sf(aes(fill = decile_centr), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1,
                       drop = F) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(-42041.12, -26100),
            ylim = c(6712127.52, 6750119.13))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Bergen",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_brg_cent_inset 

map_trd_cent_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_centr), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(265339.9, 273400.0),
            ylim = c(7030675.6, 7043009.4))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Trondheim",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_trd_cent_inset

map_sval_cent_inset <- ggplot(norge_desil) +
  geom_sf(aes(fill = decile_centr), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "cividis", 
                       na.value = "grey90", 
                       direction = -1) +
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(379976.4, 868234.7),
            ylim = c(8462871.1, 9000718.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Svalbard",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_sval_cent_inset

final_NOR_map_cent <- NOR_map_cent +
  inset_element(
    map_oslo_cent_inset, 
    left = 0.60, bottom = 0.002, right = 0.95, top = 0.40
  ) +
  inset_element(
    map_brg_cent_inset,
    left = 0.0000000001, bottom = 0.005, right = 0.23, top = 0.35
  ) + 
  inset_element(
    map_trd_cent_inset,
    left = 0.0000001, bottom = 0.39, right = 0.28, top = 0.65
  ) + 
  inset_element(
    map_sval_cent_inset,
    left = 0.07, bottom = 0.70, right = 0.29, top = 0.9999999
  )

final_NOR_map_cent
ggsave(final_NOR_map_cent, file = "PLOTS/final_NOR_map_cent.png", dpi = 900)


#### Map study areas Norway ####
Norway_plot_areas <- data_norge
#class(Norway_plot_areas$fylke2024)
Norway_plot_areas <- Norway_plot_areas %>% 
  mutate(fylke2024 = recode(as.factor(fylke2024),
                            "3" = "Oslo",
                            "34" = "Innlandet",
                            "46" = "Vestland",
                            "50" = "Trøndelag",
                            "55" = "Troms",
                            "56" = "Finnmark"))

NOR_counties <- ggplot(Norway_plot_areas) +
  geom_sf(aes(fill = fylke2024), color = "white", linewidth = 0.0001) +
  geom_rect(data = bbox_osl, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_brg, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  
  geom_rect(data = bbox_trd, aes(xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax),
            inherit.aes = F,
            fill = NA,
            color = "grey10", linewidth = 0.2)+
  #scale_fill_brewer(palette = "Dark2", na.value = "grey90")+
  scale_fill_viridis_d(option = "viridis", na.value = "grey90")+
  labs(title = "Study Areas Norway, Counties",
       fill = "county name",
       caption = "Source: GeoNorge. Inset maps not to scale") +
  theme_linedraw()+
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0, size = 8)) + 
  coord_sf( xlim = c(-371225.4, 1115103.1),
            ylim = c(6440598.3, 8070610.9))
  
NOR_counties

# Inset Map Study areas #####

map_oslo_count_inset <- ggplot(Norway_plot_areas) +
  geom_sf(aes(fill = fylke2024), color = "grey10", linewidth = 0.03) +
  #scale_fill_brewer(palette = "Paired", na.value = "grey90")+
  scale_fill_viridis_d(option = "viridis", na.value = "grey90")+
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(255497.9, 273925.6),
            ylim = c(6645956.6, 6659321.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Oslo",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_oslo_count_inset 

bergen_NOR_count_inset <- Norway_plot_areas %>% 
  filter(kommune == "4601")
# bergen_NOR_count_inset <- bergen_NOR_count_inset %>% 
#   mutate(fylke2024 = factor(fylke2024, 
#                             levels = levels(Norway_plot_areas$fylke2024)))

map_brg_count_inset <- ggplot(bergen_NOR_count_inset) +
  geom_sf(aes(fill = fylke2024), color = "grey10", linewidth = 0.03) +
  # scale_fill_brewer(palette = "Paired", 
  #                   na.value = "grey90",
  #                   drop = F,
  #                   limits = levels(Norway_plot_areas$fylke2024))+
  scale_fill_viridis_d(option = "viridis", na.value = "grey90", drop = F)+
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none")+
  coord_sf( xlim = c(-42041.12, -26100),
            ylim = c(6712127.52, 6750119.13))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Bergen",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2)
map_brg_count_inset 

map_trd_count_inset <- ggplot(Norway_plot_areas) +
  geom_sf(aes(fill = fylke2024), color = "grey10", linewidth = 0.03) +
  scale_fill_viridis_d(option = "viridis", na.value = "grey90")+
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(265339.9, 273400.0),
            ylim = c(7030675.6, 7043009.4))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Trondheim",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_trd_count_inset

map_sval_count_inset <- ggplot(Norway_plot_areas) +
  geom_sf(aes(fill = fylke2024), color = "grey10", linewidth = 0.03) +
  # scale_fill_brewer(palette = "Paired", na.value = "grey90")+
  scale_fill_viridis_d(option = "viridis", na.value = "grey90")+
  guides(fill = guide_legend(reverse = T))+
  theme_void()+
  theme(legend.position = "none") + 
  coord_sf( xlim = c(379976.4, 868234.7),
            ylim = c(8462871.1, 9000718.8))+
  annotate(
    "label", x = -Inf, y = Inf,
    label = "Svalbard",
    hjust = -0.1, vjust = 1.1, 
    fill = "grey90", size = 2,)
map_sval_count_inset

#Final map Counties Norway #####
final_NOR_map_count <- NOR_counties +
  inset_element(
    map_oslo_count_inset, 
    left = 0.60, bottom = 0.002, right = 0.95, top = 0.40
  ) +
  inset_element(
    map_brg_count_inset,
    left = 0.0000000001, bottom = 0.005, right = 0.23, top = 0.35
  ) + 
  inset_element(
    map_trd_count_inset,
    left = 0.0000001, bottom = 0.39, right = 0.28, top = 0.65
  ) + 
  inset_element(
    map_sval_count_inset,
    left = 0.07, bottom = 0.70, right = 0.29, top = 0.9999999
  )

final_NOR_map_count
ggsave(final_NOR_map_count, file = "PLOTS/final_NOR_map_count.png", dpi = 900)



