#THis code will create plots compring size and mark status between cree and refernce fishery, this one looks at mSF as a whole not just by month 
#Author: Philip Lemp 
#Date: June 20,2025

rm(list=ls())

#Load Packages 
library(tidyverse)
library(rlang)

#load data

Mark_Rates <- read.csv("Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Prepped Data/Legal_Proportions_Month_Sub(2023&2024).csv")

Legal_Rates <- read.csv("Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Raw data/Prepped Data/Size_Proportions_Month_Sub.csv")




#First we must add MSF areas to the tables 

Becher             <- "20DB"

Bute               <- c("13L", "13.21")

Gulf_Saanich       <- c("17J", "17K", "18A", "18B", "19A")


Sechelt            <- c("16N", "16P", "16J", "16B", "16K")

Toba               <- c("15E","15.6")

Victoria_Haro      <- c("19B", "19C", "19D", "19E")

##Mark Rates##

#initiate MSF column 

Mark_Rates$MSF <- NA


#fill in MSF column 
for(i in 1:nrow(Mark_Rates)){
  if(Mark_Rates$Creel_Sub_Area[i] == Becher){
    Mark_Rates$MSF[i] <- "Becher"
  }else if(Mark_Rates$Creel_Sub_Area[i] %in% Bute){
    Mark_Rates$MSF[i] <- "Bute*"
  }else if(Mark_Rates$Creel_Sub_Area[i] %in% Gulf_Saanich){
    Mark_Rates$MSF[i] <- "Gulf_Saanich"
  }else if(Mark_Rates$Creel_Sub_Area[i] %in% Sechelt){
    Mark_Rates$MSF[i] <- "Sechelt"
  }else if(Mark_Rates$Creel_Sub_Area[i] %in% Toba){
    Mark_Rates$MSF[i] <- "Toba*"
  }else if(Mark_Rates$Creel_Sub_Area[i] %in% Victoria_Haro){
    Mark_Rates$MSF[i] <- "Victoria_Haro"
  }else{Mark_Rates$MSF[i] <- NA}
}

##Legal Rates 
Legal_Rates$MSF <- NA


#fill in MSF column 
for(i in 1:nrow(Legal_Rates)){
  if(Legal_Rates$Creel_Sub_Area[i] == Becher){
    Legal_Rates$MSF[i] <- "Becher"
  }else if(Legal_Rates$Creel_Sub_Area[i] %in% Bute){
    Legal_Rates$MSF[i] <- "Bute*"
  }else if(Legal_Rates$Creel_Sub_Area[i] %in% Gulf_Saanich){
    Legal_Rates$MSF[i] <- "Gulf_Saanich"
  }else if(Legal_Rates$Creel_Sub_Area[i] %in% Sechelt){
    Legal_Rates$MSF[i] <- "Sechelt"
  }else if(Legal_Rates$Creel_Sub_Area[i] %in% Toba){
    Legal_Rates$MSF[i] <- "Toba*"
  }else if(Legal_Rates$Creel_Sub_Area[i] %in% Victoria_Haro){
    Legal_Rates$MSF[i] <- "Victoria_Haro"
  }else{Legal_Rates$MSF[i] <- NA}
}


#Prep Mark Rate data for plotting
###########

#Group sum  by MSF, Month, Year

Mark_Rates_Sum <- Mark_Rates %>% 
  group_by(Year, MSF) %>% 
  summarise(Creel_Total     = sum(Creel_Total), 
            Creel_Marked    = sum (creel_marked), 
            SE_Creel_Marked = sqrt(sum(SE_Creel_Marked^2)), 
            Ref_Total       = sum(Ref_Total), 
            Ref_Marked      = sum(ref_marked))

#Catc Mark rate and SE
Mark_Rates_Sum2 <- Mark_Rates_Sum %>% 
  mutate(
    Creel_Mark_Rate    = (Creel_Marked / Creel_Total),
    Creel_UCL          = pmin(Creel_Mark_Rate + 1.96 * SE_Creel_Marked/ Creel_Total, 1),
    Creel_LCL          = pmax(Creel_Mark_Rate - 1.96 * SE_Creel_Marked/ Creel_Total, 0), 
    Ref_Mark_Rate      = (Ref_Marked / Ref_Total) * 100,
    Ref_Mark_Rate_SE   = sqrt(((Ref_Marked / Ref_Total) * (1 - (Ref_Marked / Ref_Total))) / Ref_Total) * 100, 
    Ref_UCL            = pmin(Ref_Mark_Rate + 1.96* Ref_Mark_Rate_SE, 100), 
    Ref_LCL            = pmax(Ref_Mark_Rate - 1.96* Ref_Mark_Rate_SE, 0)
  )

Mark_Rates_Sum2 <- Mark_Rates_Sum2 %>% 
  mutate(
    Creel_Mark_Rate = Creel_Mark_Rate * 100,
    Creel_UCL       = Creel_UCL * 100,
    Creel_LCL       = Creel_LCL * 100,
  )
  
#only keep, Year, MSF, Month Combos that Have a rate for both ref and creel 

Mark_Rates_Sum3 <- na.omit(Mark_Rates_Sum2)

#Make the data in long format

mark_rate_long <- Mark_Rates_Sum3 %>%
  select(Year, MSF,
         Creel_Mark_Rate, Creel_LCL, Creel_UCL,
         Ref_Mark_Rate, Ref_LCL, Ref_UCL) %>%
  pivot_longer(
    cols = c(Creel_Mark_Rate, Ref_Mark_Rate, Creel_LCL, Ref_LCL, Creel_UCL, Ref_UCL),
    names_to = c("Source", "Metric"),
    names_pattern = "(Creel|Ref)_(.*)",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Metric,
    values_from = Value
  )


# Optional: Make Month a factor to control order (e.g., May → June → July)
#mark_rate_long$Month <- factor(mark_rate_long$Month, levels = c( "April", "May", "June", "July"))

#Prep Legal Rate data for plotting
###########


#Group sum  by MSF, Month, Year

Legal_Rates_Sum <- Legal_Rates %>% 
  group_by(Year, MSF) %>% 
  summarise(Creel_Total    = sum(Creel_Total), 
            Creel_Legal    = sum (creel_Legal), 
            SE_Creel_Legal = sqrt(sum(SE_Creel_Legal^2)), 
            Ref_Total      = sum(Ref_Total), 
            Ref_Legal      = sum(ref_Legal))

#Calc Legal rate and SE
Legal_Rates_Sum2 <- Legal_Rates_Sum %>% 
  mutate(
    Creel_Legal_Rate    = (Creel_Legal / Creel_Total),
    Creel_UCL          = pmin(Creel_Legal_Rate + 1.96 * SE_Creel_Legal/ Creel_Total, 1),
    Creel_LCL          = pmax(Creel_Legal_Rate - 1.96 * SE_Creel_Legal/ Creel_Total, 0), 
    Ref_Legal_Rate      = (Ref_Legal / Ref_Total) * 100,
    Ref_Legal_Rate_SE   = sqrt(((Ref_Legal / Ref_Total) * (1 - (Ref_Legal / Ref_Total))) / Ref_Total) * 100, 
    Ref_UCL             = pmin(Ref_Legal_Rate + 1.96* Ref_Legal_Rate_SE, 100),
    Ref_LCL             = pmax(Ref_Legal_Rate - 1.96* Ref_Legal_Rate_SE, 0 ), 
  )


Legal_Rates_Sum2 <- Legal_Rates_Sum2 %>% 
  mutate(
    Creel_Legal_Rate = Creel_Legal_Rate * 100,
    Creel_UCL       = Creel_UCL * 100,
    Creel_LCL       = Creel_LCL * 100,
  )
#only keep, Year, MSF, Month Combos that Have a rate for both ref and creel 

Legal_Rates_Sum3 <- na.omit(Legal_Rates_Sum2)

#Make the data in long format

legal_rate_long <- Legal_Rates_Sum3 %>%
  select(Year, MSF,
         Creel_Legal_Rate, Creel_LCL, Creel_UCL,
         Ref_Legal_Rate, Ref_LCL, Ref_UCL) %>%
  pivot_longer(
    cols = c(Creel_Legal_Rate, Ref_Legal_Rate, Creel_LCL, Ref_LCL, Creel_UCL, Ref_UCL),
    names_to = c("Source", "Metric"),
    names_pattern = "(Creel|Ref)_(.*)",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Metric,
    values_from = Value
  )


"Compare_Plotting" <- function(Data, YEAR, Stat) {
  
  Data <- Data %>% filter(Year == YEAR)
  
  
  Y_Label <- ifelse(Stat == "Mark_Rate", "Mark Rate %", "Legal Rate %")
  
  Data <- Data %>%
    mutate(Rate = round(.data[[Stat]], 0), 
           Rate_Lable = paste0(as.character(Rate), "%"))
  

  
  Title <- ifelse(Stat == "Mark_Rate", paste0(YEAR, " Mark Rate Comparisons"), paste0(YEAR, " Legal Rate Comparisons"))
  
  # Plot
  Rate_Compare_Plot <- ggplot(Data, aes(x = MSF, y = Rate, fill = Source)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_text(aes(y = UCL + 2, label = Rate_Lable), vjust = 0, size = 3.5,
              position = position_dodge(width = 0.9))+
    geom_errorbar(
      aes(ymin = LCL, ymax = UCL),
      width = 0.2,
      position = position_dodge(width = 0.9)
    ) +
    labs(
      x = NULL,
      y = Y_Label, 
      title = Title
    ) +
    theme_minimal() +
    theme(
      axis.text.x =  element_text(size = 10),
      legend.position = "bottom"
    )+
    scale_y_continuous(breaks = seq(0,100, by = 25), 
                       limits = c(0, max(Data$UCL, na.rm = TRUE) * 1.2))+
    scale_x_discrete(labels = c(
      "Becher"        = "Beecher Bay",
      "Gulf_Saanich"  = "Gulf Islands\nSaanich Inlet", 
      "Sechelt"       = "Sechlet Inlet",
      "Victoria_Haro" = "Victoria &\nHaro Strait"
    ),
    na.translate = FALSE)+
     scale_fill_discrete(
      name = "Data Source",
      labels = c("Creel Survey", "Reference Fishery"), 
      na.translate = FALSE
    )
 
  
  return(Rate_Compare_Plot)
  
}



#Generate Mark Rate Plots 
Mark_Rate_Compare_2023 <- Compare_Plotting(Data = mark_rate_long, YEAR = 2023, Stat = "Mark_Rate")

ggsave(Mark_Rate_Compare_2023, file ="Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Images/Direct Compare/Mark_Rate_Compare_2023_Whole.jpeg", width = 7, height = 6, dpi = 300, units = "in")

Mark_Rate_Compare_2024 <- Compare_Plotting(Data = mark_rate_long, YEAR = 2024, Stat = "Mark_Rate")

ggsave(Mark_Rate_Compare_2024, file ="Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Images/Direct Compare/Mark_Rate_Compare_2024_Whole.jpeg", width = 7, height = 6, dpi = 300, units = "in")



#Generate Legal Rate Plot
Legal_Rate_Compare_2023 <- Compare_Plotting(Data = legal_rate_long, YEAR = 2023, Stat = "Legal_Rate")

ggsave(Legal_Rate_Compare_2023, file ="Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Images/Direct Compare/Legal_Rate_Compare_2023_Whole.jpeg", width = 7, height = 6, dpi = 300, units = "in")

Legal_Rate_Compare_2024 <- Compare_Plotting(Data = legal_rate_long, YEAR = 2024, Stat = "Legal_Rate")

ggsave(Legal_Rate_Compare_2024, file ="Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Images/Direct Compare/Legal_Rate_Compare_2024_Whole.jpeg", width = 7, height = 6, dpi = 300, units = "in")


#get Differnece in the legal and mark rates 

# Difference in legal rates 

Legal_Rate_Dif <- Legal_Rates_Sum3 %>% 
  group_by(Year, Month, MSF) %>% 
  mutate(Creel_Legal_Rate = round(Creel_Legal_Rate, digits = 0), 
         Ref_Legal_Rate   = round(Ref_Legal_Rate, digits = 0) ) %>% 
  summarise(pcnt_diff = Creel_Legal_Rate - Ref_Legal_Rate)

write.csv(Legal_Rate_Dif, file = "Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Images/Direct Compare/Legal_Rate_Dif_Whole.csv")


# Difference in mark rates 

Mark_Rate_Dif <- Mark_Rates_Sum3 %>% 
  group_by(Year, Month, MSF) %>% 
  mutate(Creel_Mark_Rate = round(Creel_Mark_Rate, digits = 0), 
         Ref_Mark_Rate   = round(Ref_Mark_Rate, digits = 0) ) %>% 
  summarise(pcnt_diff = Creel_Mark_Rate - Ref_Mark_Rate)

write.csv(Mark_Rate_Dif, file = "Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/Creel VS RF/Images/Direct Compare/Mark_Rate_Dif_Whole.csv")


