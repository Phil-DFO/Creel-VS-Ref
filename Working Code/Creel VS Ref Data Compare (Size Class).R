#COmpare Ratio of Legal to Sub-Legal Chiook from Creel and the reference Fishery  
#Date 10/4/2024
#Author: Phil


#SE Calculations --------------------------------------------
#To start with, if you simply want SE for the ref proportions, use the formula sq_root{ p(1-p)/n } where p is the proportion and n the total sample size. We can reassess later if this is ok
#--------------------------------------------------------------

#Clean envnmt
rm(list=ls())

#load packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)



#load data

Ref_Catch <- read.csv(here::here("data", "ref_fishery_2324_BioData_GSI.csv"))

Creel_Catch2023 <- read.csv(here::here("data", "Catch_est_CreelVsRef(2023).csv"))

Creel_Catch2024 <- read.csv(here::here("data", "Catch_est_CreelVsRef(2024).csv"))

Creel_Catch <- rbind(Creel_Catch2023, Creel_Catch2024)

#format and combine Ref data
#make 2024 data match 2023 format

#change Legnth from MM to CM for 2024 data

Ref_Catch <- Ref_Catch %>% mutate(LENGTH_CM = LENGTH_MM /10)

Ref_Catch <- Ref_Catch %>% select(YEAR, MONTH, AREA, 
                                  SUBAREA, ADIPOSE_FIN_CLIPPED, LENGTH_CM, 
                                  Size_Class)
Ref_data_names <-  c("Year", "Month", "PFMA", "Creel_Sub_Area", "Adipose_Fin_Clipped", 
                     "Length_(cm)", "Size_Class")  


colnames(Ref_Catch) <- Ref_data_names


#Add disposition column

Ref_Catch$Disposition <- rep("Released", times = nrow(Ref_Catch) )

#Summzrise ref catch 

Ref_Catch_Sum <- Ref_Catch %>% group_by(Year, Month, Creel_Sub_Area, Adipose_Fin_Clipped, Size_Class, Disposition) %>%
  summarise(Ref_Catch=n())

#set col names so we can left join the two data sets

colnames(Ref_Catch_Sum) <- c("Year", "Month", "Creel_Sub_Area", "Adipose_Fin_Clippped", "Size_Class", "Disposition","Referece_Fishery_Catch")


#Arrange Creel Catch est to have same columns and format
Creel_Catch_clean <- Creel_Catch %>% select(year,month, creelsub, mark, size_class, disposition,  est_sum, se_sum)

colnames(Creel_Catch_clean) <- c("Year", "Month", "Creel_Sub_Area", "Adipose_Fin_Clippped", "Size_Class", "Disposition",  "Creel_Catch_Estimate", "Creel_Catch_SE")

#COnver month # to month names
Creel_Catch_clean$Month <- month.name[as.numeric(Creel_Catch_clean$Month)]

#convert AdD clip column to Y, N "Unknonw" 
Creel_Catch_clean <- Creel_Catch_clean %>% mutate( Adipose_Fin_Clippped = case_when(
  Adipose_Fin_Clippped == "Adipose_Marked" ~ "Y",
  Adipose_Fin_Clippped == "Not_Adipose_Marked" ~ "N", 
  TRUE ~ "Unknown"
))
#convert Size class to same format
Creel_Catch_clean <- Creel_Catch_clean %>% mutate( Size_Class = case_when(
  Size_Class == "SUB-LEGAL" ~ "Sub-Legal",
  Size_Class == "LEGAL" ~ "Legal", 
  TRUE ~ "Unknown"
))


# Using base R merge function
combined_data <- merge(Creel_Catch_clean, Ref_Catch_Sum, 
                       by = c("Year", "Month", "Creel_Sub_Area", "Adipose_Fin_Clippped", "Size_Class", "Disposition"),
                       all.x = TRUE)  # all.x = TRUE performs a left join

#this removes reference fishery catch from the catch estimate. Creel SE is not affected as the SE of the creel estimate is calculated prior to log book catch (including ref catch) is added to the estimate
Creel_VS_Ref <- combined_data %>%  mutate(
  Creel_Catch_Estimate = ifelse(is.na(Referece_Fishery_Catch), 
                                Creel_Catch_Estimate,  # Keep original if Ref_Catch is NA
                                pmax(Creel_Catch_Estimate - Referece_Fishery_Catch, 0))  # Otherwise calculate
)

#write.csv(Creel_VS_Ref, file = "Creel_VS_Ref(2023&2024).csv")

# get proportions of total encounters by Legal Marked, Legal Unmarked, Sub-Legal. 

# get sample size per each Creel sub, Mont, Size Class

Creel_VS_Ref_counts <- Creel_VS_Ref %>% 
  group_by(Creel_Sub_Area, Month, Year) %>%
  mutate(Creel_Total = sum(Creel_Catch_Estimate, na.rm = T),
         Ref_Total = sum(Referece_Fishery_Catch, na.rm = T))

totals <-  Creel_VS_Ref %>% 
  group_by(Creel_Sub_Area, Month, Year) %>%
  summarise(Creel_Total = sum(Creel_Catch_Estimate, na.rm = T),
            Ref_Total = sum(Referece_Fishery_Catch, na.rm = T))

Legal <- Creel_VS_Ref_counts %>% group_by(Creel_Sub_Area, Month, Year) %>% 
  filter(Size_Class == "Legal") %>% summarise(creel_Legal = sum(Creel_Catch_Estimate, na.rm = T),
                                              ref_Legal   = sum(Referece_Fishery_Catch, na.rm = T),
                                              SE_Creel_Legal = sqrt(sum(Creel_Catch_SE^2)) )

Sub_Legal <- Creel_VS_Ref_counts %>% group_by(Creel_Sub_Area, Month, Year) %>%
  filter(Size_Class == "Sub-Legal") %>% 
  summarise(creel_Sub_Legal = sum(Creel_Catch_Estimate, na.rm = T),
            ref_Sub_Legal   = sum(Referece_Fishery_Catch, na.rm = T))


Size_Proportions <- right_join(Legal, Sub_Legal, by = c("Creel_Sub_Area", "Month", "Year"))


Size_Proportions_Month_Sub <- right_join(totals,Size_Proportions, 
                                          by = c("Creel_Sub_Area", "Month", "Year"))

Size_Proportions_Month_Sub <- Size_Proportions_Month_Sub %>%
  mutate(Creel_Prct_Legal = (creel_Legal/Creel_Total)*100,
         Creel_Prct_Sub_Legal = (creel_Sub_Legal/Creel_Total)*100,
         Ref_Prct_Legal = (ref_Legal/Ref_Total)*100, 
         Ref_Prct_Sub_Legal = (ref_Sub_Legal/Ref_Total)*100) %>% 
  select(Year, Month, Creel_Sub_Area, Creel_Total, creel_Legal, SE_Creel_Legal, 
         Creel_Prct_Legal,Creel_Prct_Sub_Legal, Ref_Total,
         ref_Legal, Ref_Prct_Legal, Ref_Prct_Sub_Legal)

Data_Test <- Size_Proportions_Month_Sub %>% distinct(Year, Month, Creel_Sub_Area) 
  

write.csv(Size_Proportions_Month_Sub, here::here("data", "Size_Proportions_Month_Sub.csv"), row.names = FALSE)
