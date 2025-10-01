#clean environment
rm(list=ls())
#laod packegs
library(tidyverse)
library(viridis)
library(tidyr)

#load Data
ref_data <- read.csv("Y:/Fishery_Monitor/Mark Selective Fishery monitoring/MSF Reference Fisheries/2024 MSF reference fishery/Analysis/Data/ref_fishery_2324_BioData_GSI.csv")


Bute_Toba_Legal <- ref_data %>%
  filter(MSF %in% c("Bute", "Toba"), Size_Class == "Legal")



Lengths <- Bute_Toba_Legal$LENGTH_MM


Lengths_Slot <- Lengths[Lengths >= 620 & Lengths  <= 800]


Proption_Slot <- (length(Lengths_Slot)/length(Lengths))*100

#another way to do it
Proportion_Slot2 <- ref_data %>%
  filter(MSF %in% c("Bute", "Toba"), Size_Class == "Legal") %>%
  summarise(
    Proportion_Slot = mean(between(LENGTH_MM, 620, 800)) * 100, 
    SD_Slot = sd(between(LENGTH_MM, 620, 800)) * 100
  )
