
#Clean environment 
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

# we neeed to group by Month, and Sub_area and summerize numbe fo legal fish Mark Rate

Logs_Sum_MarkRate <- Chinook_Logs %>% filter(!SPP == "Chinook Undersized") %>% group_by(Year, Month, SUBAREA_1) %>% 
  summarise(Marked = sum(KEPT_H, REL_H), 
            Unmarked = sum(KEPT_W, REL_W)) 

colnames(Logs_Sum_MarkRate) <- c("Year", "Month", "Creel_Sub", "Marked", "UnMakred")

Logs_Sum_SizeClass <- Chinook_Logs %>%
  group_by(Month, SUBAREA_1) %>%
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


#now do the same for the interiviews

Interivews_Sum_MarkRate <- Interviews %>% group_by(YEAR, MONTH, STATSUB) %>% 
  summarise(Marked = sum(CN_AD_K, CN_AD_RL), 
            Unmakred = sum(CN_NM_K, CN_NM_RL))

colnames(Interivews_Sum_MarkRate) <- c("Year", "Month", "Creel_Sub", "Marked", "UnMakred")
#now combine the Logs and inteives into one DF 

Raw_Marks <- rbind(Logs_Sum_MarkRate, Interivews_Sum_MarkRate)


Raw_Marks_2 <- Raw_Marks %>% group_by(Year, Month, Creel_Sub) %>% 
  summarise(Combined_Marked = sum(Marked), 
            Combined_Unmakred = sum(UnMakred))

#Converts month column to names. 

Raw_Marks_2$Month <- month.name[Raw_Marks_2$Month]



#Reference Fishery Data 

#Summzrise ref catch 

Ref_Catch_Sum <- Ref_Catch %>% group_by(Year, Month, Creel_Sub_Area, Adipose_Fin_Clipped, Size_Class) %>%
  summarise(Ref_Catch=n())

#set col names so we can left join the two data sets

colnames(Ref_Catch_Sum) <- c("Year", "Month", "Creel_Sub_Area", "Adipose_Fin_Clipped", "Size_Class","Referece_Fishery_Catch")



#####
#Bring the Raw counts and reference fishery Data together

#Filter for only legal size fish and remove the size class column 

Ref_Catch_Legal <- Ref_Catch_Sum %>% filter(Size_Class == "Legal", 
                                            Adipose_Fin_Clipped %in% c("Y", "N")  ) %>% #reomve any unk clip status
  select("Year", "Month", "Creel_Sub_Area", "Adipose_Fin_Clipped","Referece_Fishery_Catch")



#lets try Pivot Longer 

Raw_Counts_Long <- pivot_longer(Raw_Marks_2, 
                                cols = c(Combined_Marked, Combined_Unmakred), 
                                values_to = "Catch")#it worked Wahoo


#change the name column to Y and N for marked or unmarked to Match Adipose_Fin_Clipped column format
Raw_Counts_Long$name <- ifelse(Raw_Counts_Long$name == "Combined_Marked", "Y", "N")

#Make Column Names match 
colnames(Raw_Counts_Long) <- c("Year", "Month", "Creel_Sub_Area", "Adipose_Fin_Clipped","Creel_Catch")


#Merge the Creel raw catch and Ref catch together.

RawCreel_Ref_Catch <- merge(Raw_Counts_Long,Ref_Catch_Legal,
                           by = c("Year", "Month", "Creel_Sub_Area", "Adipose_Fin_Clipped"), 
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

#Get marked totals 
marked <- RawCreel_VS_Ref_counts %>% group_by(Creel_Sub_Area, Month, Year) %>% 
  filter(Adipose_Fin_Clipped == "Y") %>% 
  summarise(creel_marked = sum(Creel_Catch, na.rm = T),
            ref_marked = sum(Referece_Fishery_Catch, na.rm = T))
#Get unmarked totals
unmarked <- RawCreel_VS_Ref_counts %>% group_by(Creel_Sub_Area, Month, Year) %>% 
  filter(Adipose_Fin_Clipped == "N") %>% summarise(creel_unmarked = sum(Creel_Catch, na.rm = T),
                                                    ref_unmarked   = sum(Referece_Fishery_Catch, na.rm = T))

#combine total by mark status with gernal catch totals 
Mark_Proportions <- right_join(marked, unmarked, by = c("Creel_Sub_Area", "Month", "Year"))
Legal_Proportions_Month_Sub <- right_join(totals,Mark_Proportions,  by = c("Creel_Sub_Area", "Month", "Year"))

#calculate mark rates. 
Legal_Proportions_Month_Sub <- Legal_Proportions_Month_Sub %>%
  mutate(Creel_Prct_Marked = (creel_marked/Creel_Total)*100,
         Creel_Prct_Unmarked = (creel_unmarked/Creel_Total)*100,
         Ref_Prct_Marked = (ref_marked/Ref_Total)*100, 
         Ref_Prct_Unmarked = (ref_unmarked/Ref_Total)*100) %>% 
  select(Year, Month, Creel_Sub_Area, Creel_Total, creel_marked,
         Creel_Prct_Marked,Creel_Prct_Unmarked, Ref_Total,
         ref_marked, Ref_Prct_Marked, Ref_Prct_Unmarked)

Data_Test <- Legal_Proportions_Month_Sub %>% distinct(Year, Month, Creel_Sub_Area) 

#write CSV. 
write.csv(Legal_Proportions_Month_Sub, file = "Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Prepped Data/Raw_Legal_Proportions_Month_Sub(2023-2025).csv")




