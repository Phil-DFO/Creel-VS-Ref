#Tis Code will compare Creel and refernce fishery mark rate & legal rate with Confindce intervals 
#Author: Philip Lemp
#Date: 4/30/2025

rm(list = ls())

#load packages
library(here)
library(tidyverse)

#load Data
Mark_Rate_Data  <- read.csv(here :: here( "data", "Legal_Proportions_Month_Sub(2023&2024).csv"))

Legal_Rate_Data <-  read.csv(here :: here( "data", "Size_Proportions_Month_Sub.csv"))


#equation used to calc se of creel mark rate (nmakred +- 1.96*SEmarekd)/creel total






