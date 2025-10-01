#THise Code will get mark rates and LEagl-Sub-legal rates from raw interivews. 
#Author: Philip Lemp  
#Date: 1/13/2025

#Clear Environment 
rm(list=ls())
#Load Packages
library(tidyverse)
library(readxl)
library(lubridate)


#Read in the interivew data. 

Interivew_Data <- read_excel("Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Interview_Summary (Monday, January 13, 2025 10 22 AM).xlsx")



#Lets filter all of the Bio data
# We can do this by subarea, Taget species, and month. 
##For MSFs that end on July 14th, we will filter thos intierves in july speratly and add them in after. 
## We will do 2023 and 2024 separtly because the Reference Fisheries oparated at diferent times. 


####################
#2023
####################

#set the areas and moth that belwong to each refernce fishery 

Becher_2023              <- "20DB"

Becher_Months_2023       <- c(7)

Gulf_Saanich_2023        <- c("17J", "17K", "18A", "18B", "19A")

Gulf_Months_2023         <- c(5, 6, 7) 

Sechelt_2023             <- c("16N", "16P", "16J", "16B", "16K")

Sechelt_Months_2023      <- c(6, 7)

Victoria_Haro_2023       <- c("19B", "19C", "19D", "19E")

Vic_Months_2023          <- c(5)


#Filter for trips targeting CHinook 
#filter for trips that contian "Chinook" in the target 

Chinook_Interivews_2023 <- Interivew_Data %>%
  filter(grepl("Chinook", TARGET_SPECIES, ignore.case = TRUE), YEAR == 2023)

#change month colunm to numeric

Chinook_Interivews_2023$MONTH <- as.numeric(Chinook_Interivews_2023$MONTH)

# create day column 

Chinook_Interivews_2023 <- Chinook_Interivews_2023 %>% mutate(Day = day(DATE))


#becher
Becher_Catch_2023 <- Chinook_Interivews_2023 %>% filter(STATSUB %in% Becher_2023, MONTH %in% Becher_Months_2023) %>% 
  mutate(MSF ="Becher")

#Gulf Isalnds & Saanich Inlet
Gulf_Saanich_Catch_2023 <- Chinook_Interivews_2023 %>% filter(STATSUB %in% Gulf_Saanich_2023, MONTH %in% Gulf_Months_2023, !(MONTH == 7 & Day > 14)) %>%
  mutate(MSF ="Gulf_Saanich") #ends on JUly 14th

#Seachelt Inlet 
Sechelt_Catch_2023 <- Chinook_Interivews_2023 %>% filter(STATSUB %in% Sechelt_2023, MONTH %in% Sechelt_Months_2023, !(MONTH == 7 & Day > 14)) %>%
  mutate(MSF ="Sechelt") #ends on JUly 14th

#Victoria & Haro Strait
Vic_Catch_2023 <- Chinook_Interivews_2023 %>% filter(STATSUB %in% Victoria_Haro_2023, MONTH %in% Vic_Months_2023) %>%
  mutate(MSF ="Victoria_Haro")

#combine into one DF
Ref_Interivews_2023 <- rbind(Becher_Catch_2023, Gulf_Saanich_Catch_2023, Sechelt_Catch_2023, Vic_Catch_2023)



####################
#2024
####################

#set the areas and moth that belwong to each refernce fishery 

Becher_2024             <- "20DB"

Becher_Months_2024      <- c(4,5,6)

Gulf_Saanich_2024       <- c("17J", "17K", "18A", "18B", "19A")
 
Gulf_Months_2024        <- c(5, 6, 7)

Sechelt_2024            <- c("16N", "16P", "16J", "16B", "16K")

Sechelt_Months_2024     <- c(5, 6, 7)

Victoria_Haro_2024      <- c("19B", "19C", "19D", "19E")

Vic_Months_2024         <- c(4,5)

Bute_Toba_2024          <- c("15E", "15.6", "13L", "13.21") #note all hyphens are cahnged to decimal points. 

Bute_Toba_Months_2024   <- 6


#Filter for trips targeting CHinook 
#filter for trips that contian "Chinook" in the target 

Chinook_Interivews_2024 <- Interivew_Data %>%
  filter(grepl("Chinook", TARGET_SPECIES, ignore.case = TRUE), YEAR == 2024)

#change month colunm to numeric

Chinook_Interivews_2024$MONTH <- as.numeric(Chinook_Interivews_2024$MONTH)

# create day column 

Chinook_Interivews_2024 <- Chinook_Interivews_2024 %>% mutate(Day = day(DATE))


#becher
Becher_Catch_2024 <- Chinook_Interivews_2024 %>% filter(STATSUB %in% Becher_2024, MONTH %in% Becher_Months_2024) %>% 
  mutate(MSF ="Becher")

#Gulf Isalnds & Saanich Inlet
Gulf_Saanich_Catch_2024 <- Chinook_Interivews_2024 %>% filter(STATSUB %in% Gulf_Saanich_2024, MONTH %in% Gulf_Months_2024, !(MONTH == 7 & Day > 14)) %>%
  mutate(MSF ="Gulf_Saanich") #ends on JUly 14th

#Seachelt Inlet 
Sechelt_Catch_2024 <- Chinook_Interivews_2024 %>% filter(STATSUB %in% Sechelt_2024, MONTH %in% Sechelt_Months_2024, !(MONTH == 7 & Day > 14)) %>%
  mutate(MSF ="Sechelt") #ends on JUly 14th

#Victoria & Haro Strait
Vic_Catch_2024 <- Chinook_Interivews_2024 %>% filter(STATSUB %in% Victoria_Haro_2024, MONTH %in% Vic_Months_2024) %>%
  mutate(MSF ="Victoria_Haro")

#Bute & Toba
Bute_Toba_Catch_2024 <- Chinook_Interivews_2024 %>% filter(STATSUB %in% Bute_Toba_2024, MONTH %in% Bute_Toba_Months_2024, !(MONTH == 7 & Day > 14)) %>%
  mutate(MSF ="Bute_Toba")#ends on JUly 14th


#combine into one DF
Ref_Interivews_2024 <- rbind(Becher_Catch_2024, Gulf_Saanich_Catch_2024, Sechelt_Catch_2024, Vic_Catch_2024, Bute_Toba_Catch_2024)



#Rbind Ref inteives for 2023 and 2024 together
Ref_Interveiws_2324 <- rbind(Ref_Interivews_2023, Ref_Interivews_2024)

#save as CSV. 
write.csv(Ref_Interveiws_2324, file = "Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Prepped Data/Ref_Interveiws_2324.csv" )


















