#This code will Get catch for MSFs during the 2024 refernce fishery 
#Date: 9/24/2025
#Author: Philip Lemp 
rm(list=ls())
#Libraries and setup-----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(readxl)


options(tibble.width = Inf, digits = 3)

theme_set(theme_bw())

#Get Esitimets ---------------------------------------------------------------

ests <- read_excel("Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Catch_Estimates_-_Inseason (2023-2025).xlsx")

ests <- ests %>% filter(YEAR == 2025)

#subset necessary fields
#limit to Chinook
ests <- ests %>% 
  filter(SPECIES_TXT2 == "Chinook") %>% 
  rename(creelsub = AREA_OF_INTEREST,
         species = SPECIES_TXT2,
         disposition = TYPE,
         size_class = SUB_TYPE,
         size_class2 = FSIZE_DESC, #need this to pair in the SEs later. Multiple estimates for legal size fish..
         mark = MARKS_DESC,
         est = VAL,
         esttype = VALTYPE,
         year = YEAR,
         month = MONTH,
         daterange = DATESINC) %>%
  select(year,
         month,
         creelsub,
         species,
         disposition,
         size_class,
         size_class2,
         mark,
         est,
         esttype,
         daterange) %>% 
  mutate(creelsub = gsub("-", ".", creelsub), #removing the hyphens
         mark = gsub(" ", "_", mark))

nrow(ests) #20308


#Sorit inot MSFs and add MSF column 

# ID sub-areas with which MSF they belong to. 
#####THIS HAS BEEN CAHNGED TO ILNCUDE THE FULL msf OPENEING NOT JUST WHEN THE REFENCE FISHERY OPERATED

Becher             <- "20DB"

Becher_Months      <- c(4,5,6, 7)

Gulf_Saanich       <- c("17J", "17K", "18A", "18B", "19A")

Gulf_Months        <- c( 4, 5, 6, 7)

Sechelt            <- c("16N", "16P", "16J", "16B", "16K")

Sechelt_Months     <- c(4, 5, 6, 7)

Victoria_Haro      <- c("19B", "19C", "19D", "19E")

Vic_Months         <- c(4,5)

Bute_Toba          <- c("15E", "15.6", "13L", "13.21") #note all hyphens are cahnged to decimal points. 

Bute_Toba_Months   <- c(4, 5, 6, 7)



#becher
Becher_Catch <- ests %>% filter(creelsub %in% Becher, month %in% Becher_Months, species == "Chinook") %>% 
  mutate(MSF ="Becher")

#Gulf Isalnds & Saanich Inlet
Gulf_Saanich_Catch <- ests %>% filter(creelsub %in% Gulf_Saanich, month %in% Gulf_Months, species == "Chinook") %>%
  mutate(MSF ="Gulf_Saanich")

#Seachelt Inlet 
Sechelt_Catch <- ests %>% filter(creelsub %in% Sechelt, month %in% Sechelt_Months, species == "Chinook") %>%
  mutate(MSF ="Sechelt")

#Victoria & Haro Strait
Vic_Catch <- ests %>% filter(creelsub %in% Victoria_Haro, month %in% Vic_Months, species == "Chinook") %>%
  mutate(MSF ="Victoria_Haro")

#Bute & Toba
Bute_Toba_Catch <- ests %>% filter(creelsub %in% Bute_Toba, month %in% Bute_Toba_Months, species == "Chinook", year == 2025) %>%
  mutate(MSF ="Bute_Toba")


#combine into one 
est2 <- rbind(Becher_Catch, Gulf_Saanich_Catch, Sechelt_Catch, Vic_Catch, Bute_Toba_Catch)

temp <- est2 %>% 
  filter(month == 7) %>% 
  group_by(creelsub,
           daterange) %>% 
  summarise(count = n())


#remove estimates for July in that area 0114 for areas that end July 14. 
est3 <- est2 %>% 
  filter(!(creelsub %in% c("16N", "16P", "16J", "16B", "16K", "17J", "17K", "15E", "15.6", "13L", "13.21") 
           & month == 7 & daterange %in% c("1531", "0131")))

Est_Test <- est3 %>% distinct(MSF, month, creelsub,  daterange)


#import lob books

#Get Esitimets ---------------------------------------------------------------

log <- read_excel("Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Logbook_Summary_(No_bio) (2023-2025).xlsx")


# filter to July 1-14 for Chinook.
# format to match in-season report
# summarize to get total catch by 1/2 month
log2 <- log %>% 
  mutate(year = year(LDATE),
         month = month(LDATE),
         day = day(LDATE)) %>% 
  filter(month == 7,
         year == 2025,
         day <= 14, # here am filtering logbooks to only those in the first half of July
         HART_CD == "124") %>% 
  select(year,
         month,
         LDATE,
         HART_CD,
         SUBAREA_1,
         SPP,
         KEPT_W,
         KEPT_H,
         REL_W,
         REL_H,
         REL_U) %>% 
  pivot_longer(cols = KEPT_W:REL_U, names_to = "temp", values_to = "est" ) %>% 
  mutate(species =  "Chinook",
         disposition = case_when(str_detect(temp, "KEPT") ~ "Kept",
                                 TRUE ~ "Released"),
         size_class = case_when(str_detect(SPP, "Undersized") ~ "SUB-LEGAL",
                                TRUE ~ "LEGAL"),
         size_class2 = "temp",
         mark = case_when(str_detect(temp, "_H") ~ "Adipose_Marked",
                          str_detect(temp, "_W") ~ "Not_Adipose_Marked",
                          TRUE ~ "Not_Adipose_Checked"),
         esttype = "6",
         daterange = "0114",
         creelsub = SUBAREA_1) %>% 
  group_by(year,
           month,
           creelsub,
           species,
           disposition,
           size_class,
           size_class2,
           mark,
           esttype,
           daterange) %>% 
  summarise(est = sum(est)) %>% 
  filter(!(size_class == "SUB-LEGAL" & disposition == "Kept")) #We don't need the sub-legal kept catch which are all 0s. Their presence is artifact of data formatting for log inputs.
nrow(log2) #427


#limit to MSF that end  on July 14th and add the MSF column 

Log_17 <- log2 %>% filter(creelsub %in% c("17J", "17K")) %>%  mutate(MSF ="Gulf_Saanich")

Log_16 <- log2 %>% filter(creelsub %in% c("16N", "16P", "16J", "16B", "16K")) %>%  mutate(MSF ="Sechelt")

#ref fishery did not run in BUte and Toba in July
Log_13_15 <-  log2 %>% filter(creelsub %in% c("15E", "15.6", "13L", "13.21")) %>%  mutate(MSF ="Bute_Toba")

#combine 

log3 <- rbind(Log_13_15, Log_17, Log_16)


#Bind logbooks back to estimates-----------------------------------------------------------
est4 <- rbind(est3, log3)
nrow(est4) #1892


#summarize by year, month, creelsub, disposition, size_class, mark-------------------------------------------

est5 <-est4 %>% 
  mutate(esttype = case_when(esttype == 2 ~ "se",
                             TRUE ~ "estimate")) %>% 
  pivot_wider(names_from = esttype, values_from = est)


est5 <- est5 %>% 
  mutate(se = case_when(is.na(est5$se) == TRUE ~ 0,
                        TRUE ~ est5$se)) %>% 
  group_by(year,
           month,
           MSF,
           species,
           creelsub,
           disposition,
           size_class,
           mark,
           daterange) %>%
  summarise(est_sum = sum(estimate),
            se_sum = sqrt(sum(se^2)))

Est_Test2 <- est3 %>% distinct(MSF, year, month, creelsub,  daterange)

nrow(est5) #643

write.csv(est5, "Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Prepped Data/Catch_est_CreelVsRef(2025).csv")
