# Load packages and data----
library(ggplot2)
library(ggpubr)
library(tidyverse)
# Allometric equations from Shirel's paper
# creating fucntions from Shirel's paper for MW (in kg) for engulfment capacity in liters for each species where we have a known length
Mass_SKR <- tribble(
  ~Species, ~slope,   ~intercept,
  "Minke",     1.8454,  2.1399,
  "Fin",     2.595,  1.28388,
  "Blue",     3.3346, 0.3129,
  "Humpback",     2.3373,  1.8535
)

#Brining in data for morphometrics and all flukebeat data 
morphometrics <- read_csv("10:2 Data Sheet For Hayden.csv") 
#All Data
d_all_swimming <- read_csv("10:2 Droned Tailbeats Info Hayden.csv") %>% 
  select(-(X15:X19))

#Separating max flukebeats from all data
d_max_swimming <- read_csv("10:2 AllWhaleMaxEffortBeats.csv") %>% 
  semi_join(d_all_swimming, by = colnames(d_all_swimming)[c(1:4, 7:8)]) %>% 
  mutate(effort_type = "Max")

#Separating normal fluekbeats from all data
d_reg_swimming <- d_all_swimming %>%  
  anti_join(d_max_swimming, by = colnames(d_all_swimming)[c(1:4, 7:8)]) %>% 
  mutate(effort_type = "Normal") 

#Combining normal and max data, this is the data file that we will use 
d_combine_swimming <- bind_rows(d_reg_swimming, d_max_swimming) %>% 
  left_join(Mass_SKR, by = "Species") %>% 
  mutate(Mass = (Length^slope)*10^intercept, 
         TPM = `Thrust Power`/Mass,
         Species = factor(Species, levels = c("Minke", "Humpback", "Fin", "Blue"))) %>% 
  left_join(morphometrics, by = c("Species", "Individual")) 


