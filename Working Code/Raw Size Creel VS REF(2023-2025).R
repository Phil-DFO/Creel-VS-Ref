
#Clean envnmt
rm(list=ls())

#load packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)


Interviews <- read.csv("Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Prepped Data/Ref_Interveiws_232425.csv")

Logs       <- read.csv("Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Prepped Data/Ref_Logs_232425.csv")

Ref_Catch  <- read.csv("Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/2025 MSF reference fishery/Analysis/Data/Ref_Data_232425.csv")


#format and combine Ref data
#make 2024 data match 2023 format

#change Legnth from MM to CM for 2024 data
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




#SUmmerie Raw interives and Logs
#first we areg going to combine them into the same df

#filter for Chinook 

Chinook_Logs <- Logs %>% filter(HART_CD == 124)

# we need to group by Month, and Sub_area and summarize legal to sub-legal fish

Logs_Sum_SizeClass <- Chinook_Logs %>%
  group_by(Year, Month, SUBAREA_1) %>%
  summarise(
    Sub_Legal = sum(
      case_when(
        SPP == "Chinook Undersized" ~ KEPT_W + KEPT_H + REL_W + REL_H + REL_U,
        TRUE ~ 0
      ),
      na.rm = TRUE
    ),
    Legal = sum(
      case_when(
        SPP != "Chinook Undersized" ~ KEPT_W + KEPT_H + REL_W + REL_H + REL_U,
        TRUE ~ 0
      ),
      na.rm = TRUE
    )
  )

colnames(Logs_Sum_SizeClass) <- c("Year", "Month", "Creel_Sub", "Sub_Legal", "Legal")


#now do the same for the interiviews

Interivews_Sum_SizeClass <- Interviews %>% group_by(YEAR, MONTH, STATSUB) %>% 
  summarise(Sub_Legal = sum(CN_RSL, na.rm = T), 
            Legal = sum(CN_AD_K, CN_AD_RL, CN_NM_K, CN_NM_RL, CN_UNK_K, CN_UNK_RL, na.rm = T))

colnames(Interivews_Sum_SizeClass) <- c("Year", "Month", "Creel_Sub", "Sub_Legal", "Legal")
#now combine the Logs and inteives into one DF 

Raw_Sizes <- rbind(Logs_Sum_SizeClass, Interivews_Sum_SizeClass)


Raw_Sizes_2 <- Raw_Sizes %>% group_by(Year, Month, Creel_Sub) %>% 
  summarise(Combined_Legal = sum(Legal, na.rm = T), 
            Combined_Sub_Legal = sum(Sub_Legal, na.rm = T))

#Converts month column to names. 

Raw_Sizes_2$Month <- month.name[Raw_Sizes_2$Month]



#Reference Fishery Data 

#Summzrise ref catch 

Ref_Catch_Sum <- Ref_Catch %>% group_by(Year, Month, Creel_Sub_Area, Size_Class) %>%
  summarise(Ref_Catch=n())

#set col names so we can left join the two data sets

colnames(Ref_Catch_Sum) <- c("Year", "Month", "Creel_Sub_Area", "Size_Class","Referece_Fishery_Catch")



#####
#Bring the Raw counts and reference fishery Data together

#Filter for only legal size fish and remove the size class column 

Ref_Catch <- Ref_Catch_Sum %>% 
  select("Year", "Month", "Creel_Sub_Area", "Size_Class", "Referece_Fishery_Catch")



#lets try Pivot Longer 

Raw_Counts_Long <- pivot_longer(Raw_Sizes_2, 
                                cols = c(Combined_Legal, Combined_Sub_Legal), 
                                values_to = "Catch")#it worked Wahoo


#change the name column to Y and N for marked or unmarked to Match Size Class column format
Raw_Counts_Long$name <- ifelse(Raw_Counts_Long$name == "Combined_Legal", "Legal", "Sub-Legal")

#Make Column Names match 
colnames(Raw_Counts_Long) <- c("Year", "Month", "Creel_Sub_Area", "Size_Class" ,"Creel_Catch")

#Merge the Creel raw catch and Ref catch together.

RawCreel_Ref_Catch <- merge(Raw_Counts_Long,Ref_Catch,
                           by = c("Year", "Month", "Creel_Sub_Area", "Size_Class"), 
                           all.x = TRUE) #performs a left join

#Get Sample Size per month, year, Size Class
RawCreel_VS_Ref_counts <- RawCreel_Ref_Catch %>% group_by(Creel_Sub_Area, Month, Year) %>%
  mutate(Creel_Total = sum(Creel_Catch, na.rm = T),
         Ref_Total = sum(Referece_Fishery_Catch, na.rm = T))

#Get totals sepratly 
totals <-  RawCreel_Ref_Catch  %>% 
  group_by(Creel_Sub_Area, Month, Year) %>%
  summarise(Creel_Total = sum(Creel_Catch, na.rm = T),
            Ref_Total = sum(Referece_Fishery_Catch, na.rm = T))


#get Legal Totals 
Legal <- RawCreel_Ref_Catch %>% group_by(Creel_Sub_Area, Month, Year) %>% 
  filter(Size_Class == "Legal") %>% summarise(creel_Legal = sum(Creel_Catch, na.rm = T),
                                              ref_Legal   = sum(Referece_Fishery_Catch, na.rm = T) )
#get Sub_Legal Totals 
Sub_Legal <- RawCreel_VS_Ref_counts %>% group_by(Creel_Sub_Area, Month, Year) %>%
  filter(Size_Class == "Sub-Legal") %>% 
  summarise(creel_Sub_Legal = sum(Creel_Catch, na.rm = T),
            ref_Sub_Legal   = sum(Referece_Fishery_Catch, na.rm = T))


Size_Proportions <- right_join(Legal, Sub_Legal, by = c("Creel_Sub_Area", "Month", "Year"))


Size_Proportions_Month_Sub <- right_join(totals,Size_Proportions, 
                                          by = c("Creel_Sub_Area", "Month", "Year"))

Size_Proportions_Month_Sub <- Size_Proportions_Month_Sub %>%
  mutate(Creel_Prct_Legal = (creel_Legal/Creel_Total)*100,
         Creel_Prct_Sub_Legal = (creel_Sub_Legal/Creel_Total)*100,
         Ref_Prct_Legal = (ref_Legal/Ref_Total)*100, 
         Ref_Prct_Sub_Legal = (ref_Sub_Legal/Ref_Total)*100) %>% 
  select(Year, Month, Creel_Sub_Area, Creel_Total, creel_Legal, 
         Creel_Prct_Legal,Creel_Prct_Sub_Legal, Ref_Total,
         ref_Legal, Ref_Prct_Legal, Ref_Prct_Sub_Legal)

Data_Test <- Size_Proportions_Month_Sub %>% distinct(Year, Month, Creel_Sub_Area) 

#write CSV. 

write.csv(Size_Proportions_Month_Sub, file = "Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Prepped Data/Raw_Size_Proportions_Month_Sub(2023-2025).csv")







