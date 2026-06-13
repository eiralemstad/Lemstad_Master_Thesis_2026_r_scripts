library(tidyverse)
library(haven)
library(labelled)
library(readr)

data <- read_dta("/Users/eiraheleneberglemstad/Library/Mobile Documents/com~apple~CloudDocs/Documents/NTNU/GEOG3900 MASTER/SurveyData_Master.dta")

#names(data)

#print(data$kommune2024)
#print(data$landsdel2024)
#print(data$fylke2024)
#print(data$City_area)
print(data$City_part)
#print(data$sample_part)
print(data$household_income)
print(data$educationLevel)

  ###################################
 ### DEL 1 BOSTED OG TILKNYTNING ###
###################################
head(data$living_type) # 98 Annet --> omkode til 7?
#class(living_type)

head(data$gender)


# Replace 98 with 7 while keeping the same label
data <- data %>%
  mutate(living_type = replace(living_type, living_type == 98, 7))

# Update the labels to reflect the new value by creating a new vector
lab_living_type <- val_labels(data$living_type)
names(lab_living_type)[lab_living_type == 98] <- "Annet"
lab_living_type[lab_living_type == 98] <- 7

# Applying the updated labels
val_labels(data$living_type) <- lab_living_type


#head(data$S1Q3) # 7 = vil ikke svare
data$S1Q3[data$S1Q3>6] <- NA
print(data$S1Q3)
hist(data$S1Q3)

head(data$S1Q4r1) # 6 = vet ikke gjelder S1Q4r1-r6
data$S1Q4r1[data$S1Q4r1>5] <- NA
data$S1Q4r2[data$S1Q4r2>5] <- NA

# OBS ENDRINGER!
data$S1Q4r3[data$S1Q4r3>5] <- NA
data$S1Q4r3 <- recode(data$S1Q4r3, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)
#Original: "c)	Om jeg får muligheten, vil jeg gjerne flytte bort fra dette nærmiljøet".

data$S1Q4r4[data$S1Q4r4>5] <- NA
data$S1Q4r5[data$S1Q4r5>5] <- NA
data$S1Q4r6[data$S1Q4r6>5] <- NA

#hist(data$S1Q4r1)
#hist(data$S1Q4r2)
hist(data$S1Q4r3)
#hist(data$S1Q4r4)
#hist(data$S1Q4r5)
#hist(data$S1Q4r6)

  #################################
 ### DEL 2: SOSIALE RELASJONER ###
#################################

hist(data$S2Q2) #Kan snus for å passe inn i faktor i samme retning som de andre variablene i Neig PA1
# 1 = 0, 2 = 1-5, 3 = 6-10, 4 = 11-20, 5 = "Flere enn 20", 6 = "Vil ikke svare" # de andre variablene i samme faktor er 1-5 helt enig til helt uenig
# der de har positivt stilte spørsmål om nabolag og fellesskap i nærmiljøet. Det gir mening å snu denne
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

hist(data$S2Q5)
data$S2Q5[data$S2Q5==12] <- NA
print(data$S2Q5)
data$S2Q5 <- recode(data$S2Q5, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
       `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

## S2Q6

#hist(data$S2Q6r1)
data$S2Q6r1[data$S2Q6r1==6] <- NA

#hist(data$S2Q6r2)
data$S2Q6r2[data$S2Q6r2==6] <- NA

#hist(data$S2Q6r3)
data$S2Q6r3[data$S2Q6r3==6] <- NA

#hist(data$S2Q6r4)
data$S2Q6r4[data$S2Q6r4==6] <- NA

#hist(data$S2Q6r5)
data$S2Q6r5[data$S2Q6r5==6] <- NA
data$S2Q6r5 <- recode(data$S2Q6r5, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)

print(data$S2Q6r6)
hist(data$S2Q6r6)
data$S2Q6r6[data$S2Q6r6==6] <- NA
data$S2Q6r6 <- recode(data$S2Q6r6, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)


#hist(data$S2Q6r7)
data$S2Q6r7[data$S2Q6r7==6] <- NA

## S2Q7
#hist(data$S2Q7r1, labels = T, col = 4)
data$S2Q7r1[data$S2Q7r1==5] <- NA

#hist(data$S2Q7r2)
data$S2Q7r2[data$S2Q7r2==5] <- NA

#hist(data$S2Q7r3)
data$S2Q7r3[data$S2Q7r3==5] <- NA


## S2Q8
#hist(data$S2Q8r1, labels = T, col = 4)
data$S2Q8r1[data$S2Q8r1==5] <- NA

#hist(data$S2Q8r2)
data$S2Q8r2[data$S2Q8r2==5] <- NA

#hist(data$S2Q8r3)
data$S2Q8r3[data$S2Q8r3==5] <- NA

#hist(data$S2Q9)
#hist(data$S2Q11r1, xlab = "I Norge bør alle ha de samme verdiene")
data$S2Q11r1[data$S2Q11r1==6] <- NA #

### S2Q11 ###
# S2Q11r1
#hist(data$S2Q11r1)
data$S2Q11r1[data$S2Q11r1==6] <- NA

# S2Q11r2
#hist(data$S2Q11r2)
data$S2Q11r2[data$S2Q11r2==6] <- NA

# S2Q11r3
#hist(data$S2Q11r3, main = "Immigrants should try to fit into 
#norwegian society and culture")
data$S2Q11r3[data$S2Q11r3==6] <- NA

# S2Q11r4
#hist(data$S2Q11r4)
data$S2Q11r4[data$S2Q11r4==6] <- NA

# S2Q11r5
hist(data$S2Q11r5) # Immigration is good for the Norwegian economy
data$S2Q11r5[data$S2Q11r5==6] <- NA
#data$S2Q11r5 <- recode(data$S2Q11r5, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)


# S2Q11r6
#hist(data$S2Q11r6)
data$S2Q11r6[data$S2Q11r6==6] <- NA
  ##########################
 ### DEL 3: TILKNYTNING ###
##########################
 #### 3.1 IDENTITET ####
# S3Q1
#head(data$S3Q1)
#sum(!is.na(data$S3Q1))
#sum(is.na(data$S3Q1))
#hist(data$S3Q1)

# S3Q2
#head(data$S3Q2)
#sum(!is.na(data$S3Q2))
#sum(is.na(data$S3Q2))
#hist(data$S3Q2, breaks = 199)
# S3Q3


# S3Q4 "Hvor sterk tilhørighet føler du til…? 1-5 "svært sterk" til "ingen tilhørighet i det hele tatt"

# S3Q4r1 Nærmiljøet du bor i nå?
#hist(data$S3Q4r1)
data$S3Q4r1[data$S3Q4r1==6] <- NA
print(data$S3Q4r1)

# S3Q4r2 byen/tettstedet/bygda du vokste opp i? Hvis du har bodd flere steder, velg det stedet du har bodd lengst.
#hist(data$S3Q4r2)
data$S3Q4r2[data$S3Q4r2==6] <- NA

# S3Q4r3 Norge
#hist(data$S3Q4r3)
data$S3Q4r3[data$S3Q4r3==6] <- NA



#### 3.2 TILLIT TIL INSTITUSJONER OG OPPFATNINGER AV RETTFERDIGHET ####


#S3Q6 I hvilken grad er du enig eller uenig i følgende utsagn

# d) inntektfordelingen i Norge er rettferdig
# e) Myndighetene bør iverksette tiltak for å redusere inntektsforskjellene

#hist(data$S3Q6r4)
data$S3Q6r4[data$S3Q6r4==6] <- NA
data$S3Q6r4 <- recode(data$S3Q6r4, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)
### OBS!! Omvendt variabel! FØR: d)	Inntektsfordelingen i Norge er rettferdig.
### NÅ: d)	"Inntektsfordelingen i Norge er urettferdig".
#hist(data$S3Q6r5)
data$S3Q6r5[data$S3Q6r5==6] <- NA


  #######################################################
 ### DEL 4: SAMFUNNSDELTAKELSE OG POLITISK INTERESSE ###
#######################################################

#hist(data$S4Q3, labels = T)
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
#print(S5Q1)
#hist(data$S5Q1)
data$S5Q1[data$S5Q1>11] <- NA
#SAMLET PÅ MIDTEN

### S5Q2 Har du i løpet av de siste 5 årene merket en holdningsendring ####
# blant folk rundt deg i forhold til...? 
hist(data$S5Q2r1) #innvandrere generelt
data$S5Q2r1[data$S5Q2r1>11] <- NA
head(data$S5Q2r1)
data$S5Q2r1 <- recode(data$S5Q2r1, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
`6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

# LITEN TENDENS TIL FORVERRING/ MER INTOLERANT

#hist(data$S5Q2r2) #Innvandrere fra EU/EØS
data$S5Q2r2[data$S5Q2r2>11] <- NA
# NØYTRALT
data$S5Q2r2 <- recode(data$S5Q2r2, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
                      `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

#hist(data$S5Q2r3) #Innvandrere fra land utenfor EU/EØS - interessant fordeling
data$S5Q2r3[data$S5Q2r3>11] <- NA
# LITEN TENDENS TIL FORVERRING/ MER INTOLERANT
data$S5Q2r3 <- recode(data$S5Q2r3, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
                      `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

#hist(data$S5Q2r4) #Asylsøkere og flyktninger - opplevd forverring
data$S5Q2r4[data$S5Q2r4>11] <- NA
# LITEN TENDENS TIL FORVERRING/ MER INTOLERANT
data$S5Q2r4 <- recode(data$S5Q2r4, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
                      `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

#hist(data$S5Q2r5) #LGBTQ+ personer (lesbisk, homofil, bifil, transseksuell, skeiv)
data$S5Q2r5[data$S5Q2r5>11] <- NA
# veldig LITEN TENDENS TIL forbedring/ MER TOLERANT nesten normalfordelt. 

#hist(data$S5Q2r6) #Unge mennesker
data$S5Q2r6[data$S5Q2r6>11] <- NA

#hist(data$S5Q2r7) #Gamle mennesker
data$S5Q2r7[data$S5Q2r7>11] <- NA

#hist(data$S5Q2r8) #Den økonomiske eliten - litt interessant fordeling
data$S5Q2r8[data$S5Q2r8>11] <- NA
# TYDELIG TENDENS TIL FORVERRING 

#hist(data$S5Q2r9)


##### S5Q3 GENERELT ##### 
# Har du i løpet av de siste fem årene merket en endring i holdningene til 
#klimaendringene hos folk rundt deg? [Single coded] 
print(data$S5Q3a)
hist(data$S5Q3a)
# LITEN TENDENS TIL OPPLEVD MER AKSEPT FOR MENNESKESKAPTE KLIMAENDRINGER

data$S5Q3a[data$S5Q3a==12] <- NA #"Vet ikke" = NA
#0 Endring i retning av skepsis til menneskeskapte klimaendringer - 10 
#Endring i retning av aksept for menneskeskapte klimaendringer (Vet ikke)

data$S5Q3a <- recode(data$S5Q3a, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
       `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

print(data$S5Q3b)
hist(data$S5Q3b)
# TYDELIG HELNING MOT AT "FOKUS PÅ KLIMAENDRINGER KAN IKKE BLI STORT NOK"

data$S5Q3b[data$S5Q3b==12] <- NA
#0 Fokus på klimaendringer er betydelig overdrevet – 10 Fokus på 
#klimaendringer kan ikke bli stort nok (Vet ikke)
data$S5Q3b <- recode(data$S5Q3b, `1` = 11, `2`= 10, `3` = 9, `4`= 8, `5` = 7,
                     `6`= 6, `7` = 5, `8`= 4, `9` = 3, `10`= 2, `11` = 1)

##### S5Q4 Har du i løpet av de siste fem årene merket #####
# en endring i holdningene til myndigheter og politikk blant folk rundt deg? 
# 1 Skift mot venstre - 11 Skift mot høyre (Vet ikke) 

#print(data$S5Q4)
data$S5Q4[data$S5Q4>11] <- NA
#hist(data$S5Q4) 
# Skift mot høyre. 


#### S5Q5 ####

#Med tanke på potensielle trusler mot samhold i det norske samfunnet, 
#hvor bekymret er du for...? 
#Matrise:	1- 5 (Ikke bekymret i det hele tatt, Litt bekymret, Noe bekymret, Svært bekymret, Ekstremt bekymret) (Vet ikke)

#hist(data$S5Q5r1) #Innvandring OBS!
#Ikke bekymret
data$S5Q5r1[data$S5Q5r1>5] <- NA
#data$S5Q5r1 <- recode(data$S5Q5r1, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)
#sum(is.na(S5Q5r1))
#boxplot(S5Q5r1)
#print(S5Q5r1)

#hist(data$S5Q5r2) #Manglende integrering av innvandrere OBS!
data$S5Q5r2[data$S5Q5r2>5] <- NA
#data$S5Q5r2 <- recode(data$S5Q5r2, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)
#Moderat bekymret

#hist(data$S5Q5r3) #Økende sosiale forskjeller -- litt høy, (3 og) 4 er toppen(e) OBS!
data$S5Q5r3[data$S5Q5r3>5] <- NA
# moderat bekymret
data$S5Q5r3 <- recode(data$S5Q5r3, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)
### OBS!! Omvendt variabel! "Med tanke på potensielle trusler mot samhold 
  # i det norske samfunnet, hvor bekymret er du for økende sosiale forkjeller?"
  # FØR: 1-5 ikke bekymret i det hele tatt - ekstremt bekymret
  # NÅ: 1-5 Ekstremt bekymret - ikke bekymret i det hele tatt




#hist(data$S5Q5r4) #Vold og kriminalitet  -- litt høy - "Normalfordeling" med toppen på 4 OBS!
data$S5Q5r4[data$S5Q5r4>5] <- NA
# Noe høy - bekymret
#data$S5Q5r4 <- recode(data$S5Q5r4, `1` = 5, `2`= 4, `3` = 3, `4`= 2, `5` = 1)


#hist(data$S5Q5r5) #Sosiale medier
data$S5Q5r5[data$S5Q5r5>5] <- NA
# Omtrent normalfordelt

#hist(data$S5Q5r6) #Endring i folks verdier
data$S5Q5r6[data$S5Q5r6>5] <- NA
# Normalfordelt - nøytral

#hist(data$S5Q5r7) #Kjønnsroller og kjønnsidentitet
data$S5Q5r7[data$S5Q5r7>5] <- NA
# ikke bekymret

#hist(data$S5Q5r8) #Skiller mellom generasjonene
data$S5Q5r8[data$S5Q5r8>5] <- NA
# ikke bekymret

#hist(data$S5Q5r9) #Polarisering
data$S5Q5r9[data$S5Q5r9>5] <- NA
# Normalfordelt

#hist(data$S5Q5r10) #Kunstig intelligens (KI)
data$S5Q5r10[data$S5Q5r10>5] <- NA
# lite til moderat bekymret

#hist(data$S5Q5r11) #Falske nyheter, feilinformasjon og desinformasjon -- litt høy - "Normalfordeling" med toppen på 4
data$S5Q5r11[data$S5Q5r11>5] <- NA
# moderat til høyt - bekymret

#hist(data$S5Q5r12) #En ny pandemi (som Covid-19)
data$S5Q5r12[data$S5Q5r12>5] <- NA
#Lite bekymet - lav

#hist(data$S5Q5r13) #Terrorangrep i Norge
data$S5Q5r13[data$S5Q5r13>5] <- NA
# Lite bekymret

#hist(data$S5Q5r14) #Krigshandlinger på norske jord
data$S5Q5r14[data$S5Q5r14>5] <- NA
# Lite bekymret

#hist(data$S5Q5r15) #Naturkatastrofer (f.eks. storm, flom, skogbrann, skred, stormflo) 
data$S5Q5r15[data$S5Q5r15>5] <- NA
# Lite til moderat

#hist(data$S5Q5r16) #Geopolitiske endringer
data$S5Q5r16[data$S5Q5r16>5] <- NA
# nesten Normalfordelt lite til moderat bekymret

hist(data$S5Q5r17) #Sentralisering av tjenester
data$S5Q5r17[data$S5Q5r17>5] <- NA
print(data$S5Q5r17)
# lite til moderat bekymret 
sum(is.na(data$S5Q5r17))


##### DEL 6: NATURFARER OG KLIMAENDRINGER ####

#hist(data$S6Q3)
data$S6Q3[data$S6Q3==6] <- NA

print(data$S6Q4)
hist(data$S6Q4)
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
hist(data$S7Q2)
print(data$S7Q2)
data$S7Q2[data$S7Q2==6] <- NA
  so############################
 ### DEL 8: BAKGRUNNSDATA ###
############################

### S8Q1 - Hva er ditt kjønn? ###
#head(data$gender)

### S8Q2 (age) - Hvilket år er du født?/Hva er din alder? ###
      # (førstnevnte i survey, sistnevnte i data)
#is.na(data$age) # No missing values

### S8Q3 (educationLevel) - Hva er din høyeste fullførte utdannelse? ###
#head(data$educationLevel)
data$educationLevel[data$educationLevel>5] <- NA #98 = "Annet" er nå NA
#hist(data$educationLevel)
#print(data$educationLevel)


### S8Q4 (occupation) - Hvordan vil du beskrive din daglige situasjon? ###
    # (Dersom det er flere alternativ som 	passer, velger 
    # du det som ut fra din egen mening stemmer best.)

#head(data$occupation) # 99 = "vil ikke svare"
data$occupation[data$occupation>98] <- NA # 99 = "Vil ikke svare" 
#hist(data$occupation)
print(data$occupation)

### S8Q5 (profession) - Hva er ditt arbeidsområde? ###
#head(data$profession) # 99 = Ubesvart/vet ikke
data$profession[data$profession>98] <- NA 
#hist(data$profession)

### S8Q6 - Betrakter du deg selv som tilhørende noen 
      # bestemt religion eller trossamfunn? ###
#head(data$S8Q6) # ingen endring nødvendig
#hist(data$S8Q6) 

### S8Q7 (household_size) - Hvor mange personer er det i husstanden? ###
#head(data$household_size) # 90 = "Vil ikke svare"
data$household_size[data$household_size>89] <- NA
#summary(data$household_size)

### S8Q8 (household_children_u18) - Hvor mange personer er det i 
      # husstanden under 18 år? ###
#head(data$household_children_u18) # 90 = "Vil ikke svare"
data$household_children_u18[data$household_children_u18>89] <- NA
#summary(data$household_children_u18)

### S8Q9 (household_income) - Hva er husstandens bruttoinntekt (før skatt)? ###
data$household_income[data$household_income>89] <- NA 
    # 90 = "Vil ikke svare", 99 = "Vet ikke"


#### Ny fil ####
write_csv(data, "Master_1.csv")
write_dta(data, "Master.dta")


