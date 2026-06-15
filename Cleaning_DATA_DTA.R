library(tidyverse)
library(haven)
library(labelled)
library(readr)

data <- read_dta("…/SurveyData_Master.dta") 

#names(data)

  ###################################
 ### DEL 1 BOSTED OG TILKNYTNING ###
###################################
# (PART 1 PLACE OF LIVING AND ATTACMENT)
head(data$living_type) # 98 "Other" --> recode to 7
# Replace 98 with 7 while keeping the same label
data <- data %>%
  mutate(living_type = replace(living_type, living_type == 98, 7))

# Update the labels to reflect the new value by creating a new vector
lab_living_type <- val_labels(data$living_type)
names(lab_living_type)[lab_living_type == 98] <- "Annet"
lab_living_type[lab_living_type == 98] <- 7

# Applying the updated labels
val_labels(data$living_type) <- lab_living_type

#head(data$S1Q3) # 7 = do not want to answer
data$S1Q3[data$S1Q3>6] <- NA
print(data$S1Q3)
hist(data$S1Q3)

head(data$S1Q4r1) # 6 = "don't know" for S1Q4r1-r6
data$S1Q4r1[data$S1Q4r1>5] <- NA
data$S1Q4r2[data$S1Q4r2>5] <- NA

# N.B. Variable turned around!
data$S1Q4r3[data$S1Q4r3>5] <- NA
data$S1Q4r3 <- recode(data$S1Q4r3, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)
#Original before turning: "c)	Om jeg får muligheten, vil jeg gjerne flytte bort fra dette nærmiljøet".
# English: "c) If I get the chance, I would like to move away from this neighbourhood".

data$S1Q4r4[data$S1Q4r4>5] <- NA
data$S1Q4r5[data$S1Q4r5>5] <- NA
data$S1Q4r6[data$S1Q4r6>5] <- NA

  #################################
 ### DEL 2: SOSIALE RELASJONER ###
#################################
# PART 2 SOCIAL RELATIONS

hist(data$S2Q2) #Turned to fit with the directions of other variables in Factor in Neig PA1
# 1 = 0, 2 = 1-5, 3 = 6-10, 4 = 11-20, 5 = "More than 20", 6 = "Don't want to answer"
data$S2Q2[data$S2Q2==6] <- NA
data$S2Q2 <- recode(data$S2Q2, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)

# I hvilken grad er du enig eller uenig i følgende utsagn om ditt sosiale nettverk? 
# Matrise:	Helt enig, Enig, Hverken enig eller uenig, Uenig, Helt uenig (Vet ikke) – 1-5

#hist(data$S2Q4r1) #a)	Familien min er en viktig del av livet mitt.
data$S2Q4r1[data$S2Q4r1==6] <- NA

#hist(data$S2Q4r2) #b)	De vennskapene og tilknytningene jeg har til andre mennesker i nærmiljøet mitt betyr mye for meg.
data$S2Q4r2[data$S2Q4r2==6] <- NA

#hist(data$S2Q4r3) #c)	Jeg besøker venner og naboer hjemme hos dem.
data$S2Q4r3[data$S2Q4r3==6] <- NA

#hist(data$S2Q4r4) #d)	Jeg er villig til å samarbeide med andre om noe for å forbedre nærmiljøet mitt.
data$S2Q4r4[data$S2Q4r4==6] <- NA

hist(data$S2Q4r5) #e)	Jeg pleier ofte å stoppe og snakke med folk i nærmiljøet mitt.
data$S2Q4r5[data$S2Q4r5==6] <- NA
#data$S2Q4r5 <- recode(data$S2Q4r5, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)
#Jeg stopper sjelden for å snakke med folk i nærmiljøet mitt

#hist(data$S2Q4r6) #f)	Hvis jeg trenger råd om noe, har jeg noen i nærmiljøet mitt som jeg kan gå til.
data$S2Q4r6[data$S2Q4r6==6] <- NA

## S2Q5
data$S2Q5[data$S2Q5==12] <- NA
data$S2Q5 <- recode(data$S2Q5, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
       `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

## S2Q6
data$S2Q6r1[data$S2Q6r1==6] <- NA

data$S2Q6r2[data$S2Q6r2==6] <- NA

data$S2Q6r3[data$S2Q6r3==6] <- NA

data$S2Q6r4[data$S2Q6r4==6] <- NA

data$S2Q6r5[data$S2Q6r5==6] <- NA
data$S2Q6r5 <- recode(data$S2Q6r5, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1) # Turned

data$S2Q6r6[data$S2Q6r6==6] <- NA
data$S2Q6r6 <- recode(data$S2Q6r6, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1) #Turned

data$S2Q6r7[data$S2Q6r7==6] <- NA

## S2Q7
data$S2Q7r1[data$S2Q7r1==5] <- NA

data$S2Q7r2[data$S2Q7r2==5] <- NA

data$S2Q7r3[data$S2Q7r3==5] <- NA


## S2Q8
data$S2Q8r1[data$S2Q8r1==5] <- NA

data$S2Q8r2[data$S2Q8r2==5] <- NA

data$S2Q8r3[data$S2Q8r3==5] <- NA


### S2Q11 ###
# S2Q11r1
data$S2Q11r1[data$S2Q11r1==6] <- NA

# S2Q11r2
data$S2Q11r2[data$S2Q11r2==6] <- NA

# S2Q11r3
data$S2Q11r3[data$S2Q11r3==6] <- NA

# S2Q11r4
data$S2Q11r4[data$S2Q11r4==6] <- NA

# S2Q11r5 # Immigration is good for the Norwegian economy
data$S2Q11r5[data$S2Q11r5==6] <- NA
#data$S2Q11r5 <- recode(data$S2Q11r5, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)

# S2Q11r6
data$S2Q11r6[data$S2Q11r6==6] <- NA

  ##########################
 ### DEL 3: TILKNYTNING ###
##########################
# PART 3: Attactment 
 #### 3.1 IDENTITET #### (3.1 Identity)
# S3Q1
#sum(is.na(data$S3Q1))

# S3Q2
#sum(is.na(data$S3Q2))

## S3Q4 "Hvor sterk tilhørighet føler du til…? 1-5 "svært sterk" til "ingen tilhørighet i det hele tatt"

# S3Q4r1 Nærmiljøet du bor i nå?
data$S3Q4r1[data$S3Q4r1==6] <- NA

# S3Q4r2 byen/tettstedet/bygda du vokste opp i? Hvis du har bodd flere steder, velg det stedet du har bodd lengst.
data$S3Q4r2[data$S3Q4r2==6] <- NA

# S3Q4r3 Norge
data$S3Q4r3[data$S3Q4r3==6] <- NA

#### 3.2 TILLIT TIL INSTITUSJONER OG OPPFATNINGER AV RETTFERDIGHET ####

#S3Q6 I hvilken grad er du enig eller uenig i følgende utsagn

# r4) Original in English: "The distribution of wage is fair in Norway"
#     Original :           "Inntektsfordelingen i Norge er rettferdig"

data$S3Q6r4[data$S3Q6r4==6] <- NA
data$S3Q6r4 <- recode(data$S3Q6r4, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)
# After recode: r4)	"Inntektsfordelingen i Norge er urettferdig" 
# After recode English: "The distribution of wage is unfair in Norway"

# r5) Original in English: "The governments should place initiatives to reduce wage differences" 
#     Original :           "Myndighetene bør iverksette tiltak for å redusere inntektsforskjellene"

data$S3Q6r5[data$S3Q6r5==6] <- NA

  #######################################################
 ### DEL 4: SAMFUNNSDELTAKELSE OG POLITISK INTERESSE ###
#######################################################
# PART 4: Political interests and Community participation

#head(data$S4Q3) # value 1 = 0 venstresiden, value 11 = 10 - høyresiden. 
  # 0-10 in the survey is coded as 1-11 here in the dataset. 
#sum(data$S4Q3 == 12) # 273 of 1905 respondents answered "Don't know" 
#sum(data$S4Q3 < 12)
data$S4Q3[data$S4Q3 == 12] <- NA # Har her kodet "Vet ikke" til NA

head(data$S4Q4) # value 11 = Andre, noter: / Others note:,
#value 12 = None of these, value 13 = "Do not want to answer".
hist(data$S4Q4, breaks = 13, labels = T)
# Dette er vel kategoriske data (selv om det er her koded som numeriske.
# Tenker det er lurt å beholde alle kategoriene også "Vil ikke svare")
# da dette også gir et inntrykk om noe
print(data$S4Q4)
hist(data$S4Q4)

  ###################################
 ### DEL 5: POLARISERENDE TEMAER ###
###################################

### S5Q1 HVOR SPLITTET MENER DU DET NORSKE SAMFUNNET ER I DAG? ####
data$S5Q1[data$S5Q1>11] <- NA

### S5Q2 Har du i løpet av de siste 5 årene merket en holdningsendring ####
# blant folk rundt deg i forhold til...? 

data$S5Q2r1[data$S5Q2r1>11] <- NA
data$S5Q2r1 <- recode(data$S5Q2r1, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
`6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

data$S5Q2r2[data$S5Q2r2>11] <- NA #Innvandrere fra EU/EØS
data$S5Q2r2 <- recode(data$S5Q2r2, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
                      `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

data$S5Q2r3[data$S5Q2r3>11] <- NA #Innvandrere fra land utenfor EU/EØS 
data$S5Q2r3 <- recode(data$S5Q2r3, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
                      `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

data$S5Q2r4[data$S5Q2r4>11] <- NA #Asylsøkere og flyktninger - opplevd forverring
data$S5Q2r4 <- recode(data$S5Q2r4, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
                      `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

data$S5Q2r5[data$S5Q2r5>11] <- NA #LGBTQ+ personer (lesbisk, homofil, bifil, transseksuell, skeiv)

data$S5Q2r6[data$S5Q2r6>11] <- NA #Unge mennesker


data$S5Q2r7[data$S5Q2r7>11] <- NA #Gamle mennesker

data$S5Q2r8[data$S5Q2r8>11] <- NA #Den økonomiske eliten

##### S5Q3 GENERELT ##### 
# Har du i løpet av de siste fem årene merket en endring i holdningene til 
#klimaendringene hos folk rundt deg? [Single coded] 

data$S5Q3a[data$S5Q3a==12] <- NA #"Don't know" = NA
#0 Endring i retning av skepsis til menneskeskapte klimaendringer - 10 
#Endring i retning av aksept for menneskeskapte klimaendringer (Vet ikke)

data$S5Q3a <- recode(data$S5Q3a, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
       `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

data$S5Q3b[data$S5Q3b==12] <- NA
#0 Fokus på klimaendringer er betydelig overdrevet – 10 Fokus på 
#klimaendringer kan ikke bli stort nok (Vet ikke)
data$S5Q3b <- recode(data$S5Q3b, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
                     `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

##### S5Q4 Har du i løpet av de siste fem årene merket #####
# en endring i holdningene til myndigheter og politikk blant folk rundt deg? 
# 1 Skift mot venstre - 11 Skift mot høyre (Vet ikke) 


data$S5Q4[data$S5Q4>11] <- NA
# Skift mot høyre. 

#### S5Q5 ####
#Med tanke på potensielle trusler mot samhold i det norske samfunnet, hvor bekymret er du for...? 
#Matrise:	1- 5 (Ikke bekymret i det hele tatt, Litt bekymret, Noe bekymret, Svært bekymret, Ekstremt bekymret) (Vet ikke)


data$S5Q5r1[data$S5Q5r1>5] <- NA

data$S5Q5r2[data$S5Q5r2>5] <- NA

#Økende sosiale forskjeller -- litt høy, (3 og) 4 er toppen(e) OBS!
data$S5Q5r3[data$S5Q5r3>5] <- NA
data$S5Q5r3 <- recode(data$S5Q5r3, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)
### OBS!! Omvendt variabel! "Med tanke på potensielle trusler mot samhold 
  # i det norske samfunnet, hvor bekymret er du for økende sosiale forkjeller?"
  # FØR: 1-5 ikke bekymret i det hele tatt - ekstremt bekymret
  # NÅ: 1-5 Ekstremt bekymret - ikke bekymret i det hele tatt


data$S5Q5r4[data$S5Q5r4>5] <- NA #Vold og kriminalitet


#Naturkatastrofer (f.eks. storm, flom, skogbrann, skred, stormflo) 
data$S5Q5r15[data$S5Q5r15>5] <- NA

#Sentralisering av tjenester
data$S5Q5r17[data$S5Q5r17>5] <- NA
print(data$S5Q5r17)
# lite til moderat bekymret 
sum(is.na(data$S5Q5r17))


##### DEL 6: NATURFARER OG KLIMAENDRINGER ####

data$S6Q3[data$S6Q3==6] <- NA

data$S6Q4[data$S6Q4==6] <- NA
data$S6Q4 <- recode(data$S6Q4, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)

##### DEL 7: LIVSTILFREDSHET OG HELSE ####

#S7Q1 Alt tatt i betraktning, hvor fornøyd er du med livet ditt som helhet i dag? 
  #0 Ekstremt misfornøyd - 10 Ekstremt fornøyd (Vil ikke svare)
hist(data$S7Q1)
print(data$S7Q1)
data$S7Q1[data$S7Q1==12] <- NA
# data$S7Q1 <- recode(data$S7Q1, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
#                       `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

#S7Q2 Hvordan er helsen din stort sett? 
  # 1-5 Svært god, God, Middels, Dårlig, Svært dårlig (Vil ikke svare)
data$S7Q2[data$S7Q2==6] <- NA

  so############################
 ### DEL 8: BAKGRUNNSDATA ###
############################

### S8Q3 (educationLevel) - Hva er din høyeste fullførte utdannelse? ###
data$educationLevel[data$educationLevel>5] <- NA #98 = "Other" recoded to NA

### S8Q6 - Betrakter du deg selv som tilhørende noen 
      # bestemt religion eller trossamfunn? ###
#head(data$S8Q6) # ingen endring nødvendig
#hist(data$S8Q6) 

### S8Q9 (household_income) - Hva er husstandens bruttoinntekt (før skatt)? ###
data$household_income[data$household_income>89] <- NA 
    # 90 = "Vil ikke svare", 99 = "Vet ikke"


#### Creating new file ####
write_csv(data, "Master_1.csv")
write_dta(data, "Master.dta")


