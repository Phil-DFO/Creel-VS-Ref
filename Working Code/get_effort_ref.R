#This code will Get catch for MSFs during the 2023 refernce fishery 
#Date: 10/2/2024
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

ests <- read_excel("Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Catch_Estimates_-_Inseason (Wednesday, October 2, 2024 2 15 PM).xlsx")


#subset necessary fields
#limit to Chinook
ests <- ests %>% 
  filter(SPECIES_TXT2 == "Boat Trips") %>% 
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

nrow(ests) #22971


#Sorit inot MSFs and add MSF column 

# ID sub-areas with which MSF they belong to. 

Becher             <- "20DB"

Becher_Months      <- c(6 , 7)

Gulf_Saanich       <- c("17J", "17K", "18A", "18B", "19A")

Gulf_Months        <- c(5, 6, 7)

Sechelt            <- c("16N", "16P", "16J", "16B", "16K")

Sechelt_Months     <- c(6, 7)

Victoria_Haro      <- c("19B", "19C", "19D", "19E")

Vic_Months         <- c(5)

Bute_Toba          <- c("15E", "15.6", "13L", "13.21") #note all hyphens are cahnged to decimal points. 

Bute_Toba_Months   <- c(6)


#becher
Becher_Catch <- ests %>% filter(creelsub %in% Becher, month %in% Becher_Months, species == "Boat Trips") %>% 
  mutate(MSF ="Becher")

#Gulf Isalnds & Saanich Inlet
Gulf_Saanich_Catch <- ests %>% filter(creelsub %in% Gulf_Saanich, month %in% Gulf_Months, species == "Boat Trips") %>%
  mutate(MSF ="Gulf_Saanich")

#Seachelt Inlet 
Sechelt_Catch <- ests %>% filter(creelsub %in% Sechelt, month %in% Sechelt_Months, species == "Boat Trips") %>%
  mutate(MSF ="Sechelt")

#Victoria & Haro Strait
Vic_Catch <- ests %>% filter(creelsub %in% Victoria_Haro, month %in% Vic_Months, species == "Boat Trips") %>%
  mutate(MSF ="Victoria_Haro")

#Bute & Toba
Bute_Toba_Catch <- ests %>% filter(creelsub %in% Bute_Toba, month %in% Bute_Toba_Months, species == "Boat Trips") %>%
  mutate(MSF ="Bute_Toba")

#combine into one 
est2 <- rbind(Becher_Catch, Gulf_Saanich_Catch, Sechelt_Catch, Vic_Catch )

temp <- est2 %>% 
  filter(month == 7) %>% 
  group_by(creelsub,
           daterange) %>% 
  summarise(count = n())

#remove estimates for July in that area 0114 for areas that end July 14.  
est3 <- est2 %>% 
  filter(!(creelsub %in% c("16N", "16P", "16J", "16B", "16K", "17J", "17K", "15E", "15.6", "13L", "13.21") & month == 7 & daterange %in% c("1531", "0131")))

#import lob books

#Get Esitimets ---------------------------------------------------------------

log <- read_excel("Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Logbook_Summary_(No_bio) (Wednesday, October 2, 2024 2 12 PM).xlsx")

log <- log %>% filter(!(BOOK_NUM %in% c("23-REF","24-REF"))) #remove reference fishery effort 

# filter to July 1-14 for Chinook.
# format to match in-season report
# summarize to get total catch by 1/2 month
log2 <- log %>% 
  mutate(year = year(LDATE),
         month = month(LDATE),
         day = day(LDATE)) %>% 
  filter(month == 7,
         day <= 14) %>% # here am filtering logbooks to only those in the first half of July
  select(year,
         month,
         LDATE,
         HART_CD,
         SUBAREA_1,
         BOOK_NUM, 
         PAGE_NUM,
  ) %>% 
  distinct(LDATE, year, month, SUBAREA_1, BOOK_NUM, PAGE_NUM) %>% 
  group_by(year, month, SUBAREA_1) %>% 
  summarise(Effort = n())
nrow(log2)#35

log2 <- log2 %>% rename(creelsub = SUBAREA_1)

#limit to MSF that end  on July 14th and add the MSF column 

Log_17 <- log2 %>% filter(creelsub %in% c("17J", "17K")) %>%  mutate(MSF ="Gulf_Saanich")


#combine 

log3 <- rbind(Log_17, Log_16)

#make columns for Log3 so it can join est 3

speices <- rep("Boat Trips", times = nrow(log3))
disposition <- rep("Effort", times = nrow(log3))
size_class <- rep("EFFORT" , times = nrow(log3) )
size_class2 <- rep(NA , times = nrow(log3) )
mark <- rep(NA , times = nrow(log3) )
esttype <- rep(6 , times = nrow(log3))
daterange <- rep("0114", times = nrow(log3))

log4 <- cbind(log3, speices, disposition, size_class, size_class2, mark, esttype, daterange )

colnames(log4) <- c("year", "month", "creelsub", "est", "MSF", "species", "disposition",   "size_class", "size_class2", "mark", "esttype", "daterange")
 
log4 <- log4 %>% arrange(year, month, creelsub, species, disposition, size_class, size_class2, mark, daterange )
#Bind logbooks back to estimates-----------------------------------------------------------
est4 <- rbind(est3, log4)
nrow(est4) #292


#summarize by year, month, creelsub, disposition, size_class, mark-------------------------------------------


est5 <- est4 %>% 
  mutate(esttype = case_when(esttype == 2 ~ "se",
                             TRUE ~ "estimate")) %>% 
  pivot_wider(names_from = esttype, values_from = est)

#simplify the columns

est5 <- est5 %>%
  mutate(
    estimate = map_dbl(estimate, ~ ifelse(length(.x) == 0, NA, .x[1])),
    se = map_dbl(se, ~ ifelse(length(.x) == 0, NA, .x[1]))
  )


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

nrow(est5) #196

#write.csv(est5, "Effort_est_CreelVsRef.csv")
