#this code will get logbooks for the refernce fishery in 2023 and 2024. 
#Author: Philip Lemp  
#Date: 9/24/2025

#Clear Environment 
rm(list=ls())
#Load Packages
library(tidyverse)
library(readxl)
library(lubridate)


#Get log book data #NEED UDPATED LOG BOOK SUMARRY PULL. 
log <- read_excel("Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Logbook_Summary_(No_bio) (2023-2025).xlsx")


#remove reference fishery logs 
log <- log %>% filter(! BOOK_NUM %in% c("23-REF", "24-REF","25-REF" ))


#create and month, yearm and day column

log <- log %>% mutate(Year = year(LDATE), 
                      Month = month(LDATE), 
                      Day = day(LDATE))


####################
#2023
####################

#set the areas and moth that belong to each reference fishery 

Becher_2023              <- "20DB"

Becher_Months_2023      <- c(7)

Gulf_Saanich_2023        <- c("17J", "17K", "18A", "18B", "19A")

Gulf_Months_2023         <- c(5, 6, 7) 

Sechelt_2023             <- c("16N", "16P", "16J", "16B", "16K")

Sechelt_Months_2023      <- c(6, 7)

Victoria_Haro_2023       <- c("19B", "19C", "19D", "19E")

Vic_Months_2023          <- c(5)


#filter for trips that contian "Chinook" in the target 

Chinook_Logs_2023 <- log %>%
  filter(grepl("Chinook", SPP, ignore.case = TRUE), Year == 2023)



#becher
Becher_Catch_2023 <- Chinook_Logs_2023 %>% filter(SUBAREA_1 %in% Becher_2023, Month %in% Becher_Months_2023) %>% 
  mutate(MSF ="Becher")

#Gulf Isalnds & Saanich Inlet
Gulf_Saanich_Catch_2023 <- Chinook_Logs_2023 %>% filter(SUBAREA_1 %in% Gulf_Saanich_2023, Month %in% Gulf_Months_2023, !(Month == 7 & Day > 14)) %>%
  mutate(MSF ="Gulf_Saanich") #ends on JUly 14th

#Seachelt Inlet 
Sechelt_Catch_2023 <- Chinook_Logs_2023 %>% filter(SUBAREA_1 %in% Sechelt_2023, Month %in% Sechelt_Months_2023, !(Month == 7 & Day > 14)) %>%
  mutate(MSF ="Sechelt") #ends on JUly 14th

#Victoria & Haro Strait
Vic_Catch_2023 <- Chinook_Logs_2023 %>% filter(SUBAREA_1 %in% Victoria_Haro_2023, Month %in% Vic_Months_2023) %>%
  mutate(MSF ="Victoria_Haro")

#combine into one DF
Ref_Logs_2023 <- rbind(Becher_Catch_2023, Gulf_Saanich_Catch_2023, Sechelt_Catch_2023, Vic_Catch_2023)


####################
#2024
####################

#set the areas and moth that belong to each reference fishery 

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

#filter for trips that contain "Chinook" in the target species (SPP)

Chinook_Logs_2024 <- log %>%
  filter(grepl("Chinook", SPP, ignore.case = TRUE), Year == 2024)

#becher
Becher_Catch_2024 <- Chinook_Logs_2024 %>% filter(SUBAREA_1 %in% Becher_2024, Month %in% Becher_Months_2024) %>% 
  mutate(MSF ="Becher")

#Gulf Isalnds & Saanich Inlet
Gulf_Saanich_Catch_2024 <- Chinook_Logs_2024 %>% filter(SUBAREA_1 %in% Gulf_Saanich_2024, Month %in% Gulf_Months_2024, !(Month == 7 & Day > 14)) %>%
  mutate(MSF ="Gulf_Saanich") #ends on JUly 14th

#Seachelt Inlet 
Sechelt_Catch_2024 <- Chinook_Logs_2024 %>% filter(SUBAREA_1 %in% Sechelt_2024, Month %in% Sechelt_Months_2024, !(Month == 7 & Day > 14)) %>%
  mutate(MSF ="Sechelt") #ends on JUly 14th

#Victoria & Haro Strait
Vic_Catch_2024 <- Chinook_Logs_2024 %>% filter(SUBAREA_1 %in% Victoria_Haro_2024, Month %in% Vic_Months_2024) %>%
  mutate(MSF ="Victoria_Haro")

#combine into one DF
Ref_Logs_2024 <- rbind(Becher_Catch_2024, Becher_Catch_2024, Gulf_Saanich_Catch_2024, Sechelt_Catch_2024, Vic_Catch_2024)

####################
#2025
####################

#set the areas and moth that belong to each reference fishery 

Becher_2025             <- "20DB"

Becher_Months_2025      <- c(4,5,6)

Gulf_Saanich_2025       <- c("17J", "17K", "18A", "18B", "19A")

Gulf_Months_2025        <- c(5, 6, 7)

Sechelt_2025            <- c("16N", "16P", "16J", "16B", "16K")

Sechelt_Months_2025     <- c(4, 5, 6, 7)

Victoria_Haro_2025      <- c("19B", "19C", "19D", "19E")

Vic_Months_2025         <- c(4,5)

Bute_Toba_2025          <- c("15E", "15.6", "13L", "13.21") #note all hyphens are cahnged to decimal points. 

Bute_Toba_Months_2025   <- c(4, 5, 6, 7)


#filter for trips that contain "Chinook" in the target species (SPP)

Chinook_Logs_2025 <- log %>%
  filter(grepl("Chinook", SPP, ignore.case = TRUE), Year == 2025)

#becher
Becher_Catch_2025 <- Chinook_Logs_2025 %>% filter(SUBAREA_1 %in% Becher_2025, Month %in% Becher_Months_2025) %>% 
  mutate(MSF ="Becher")

#Gulf Isalnds & Saanich Inlet
Gulf_Saanich_Catch_2025 <- Chinook_Logs_2025 %>% filter(SUBAREA_1 %in% Gulf_Saanich_2025, Month %in% Gulf_Months_2025, !(Month == 7 & Day > 14)) %>%
  mutate(MSF ="Gulf_Saanich") #ends on JUly 14th

#Seachelt Inlet 
Sechelt_Catch_2025 <- Chinook_Logs_2025 %>% filter(SUBAREA_1 %in% Sechelt_2025, Month %in% Sechelt_Months_2025, !(Month == 7 & Day > 14)) %>%
  mutate(MSF ="Sechelt") #ends on JUly 14th

#Victoria & Haro Strait
Vic_Catch_2025 <- Chinook_Logs_2025 %>% filter(SUBAREA_1 %in% Victoria_Haro_2025, Month %in% Vic_Months_2025) %>%
  mutate(MSF ="Victoria_Haro")

#combine into one DF
Ref_Logs_2025 <- rbind(Becher_Catch_2025, Becher_Catch_2025, Gulf_Saanich_Catch_2025, Sechelt_Catch_2025, Vic_Catch_2025)

#Rbind Ref inteives for 2023 and 2024 together
Ref_Logs_232425 <- rbind(Ref_Logs_2023, Ref_Logs_2024, Ref_Logs_2025)

#save as CSV. 
write.csv(Ref_Logs_232425, file = "Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Prepped Data/Ref_Logs_232425.csv" )

Log_Summary <-  Ref_Logs_232425 %>% 
  mutate(YEAR =  format(LDATE, "%Y")) %>% 
  group_by(YEAR, MSF) %>% 
  summarise(n=n())


