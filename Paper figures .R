

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
morphometrics <- read_csv("10:2 Data Sheet For Hayden.csv") %>% 
  rename(Individual = `ID #`,
         ID = "Whale") 


#All Data
d_all_swimming <- read_csv("10:2 Droned Tailbeats Info Hayden.csv") %>% 
  left_join(select(morphometrics, Individual, ID), by = "Individual") %>% 
  select(-(X15:X19))


#Separating max flukebeats from all data
d_max_swimming <- read_csv("10:2 AllWhaleMaxEffortBeats.csv") %>% 
  left_join(select(morphometrics, Individual, ID), by = "Individual") %>% 
  semi_join(d_all_swimming, by = colnames(d_all_swimming)[c(1:4, 7:8, 15)]) %>% 
  mutate(effort_type = "Max")


#Separating normal fluekbeats from all data
d_reg_swimming <- d_all_swimming %>%  
  anti_join(d_max_swimming, by = colnames(d_all_swimming)[c(1:4, 7:8, 15)]) %>% 
  mutate(effort_type = "Normal") 


#Combining normal and max data, this is the data file that we will use 
d_combine_swimming <- bind_rows(d_reg_swimming, d_max_swimming) %>% 
  left_join(Mass_SKR, by = "Species") %>% 
  mutate(Mass = (Length^slope)*10^intercept, 
         TPM = `Thrust Power`/Mass,
         Species = factor(Species, levels = c("Minke", "Humpback", "Fin", "Blue"))) %>% 
  left_join(morphometrics, by = c("Species", "Individual", "ID")) 


#Summarizing d_max
 group_by(Species) %>% 
  summarize(Size = mean(Length)) %>% 
  arrange(Size)
d_max_swimming_summarized <- d_max_swimming %>%  
  group_by(Individual) %>% 
  summarize(mean_TPM = mean(TPM), 
            sd_TPM = sd(TPM),
            se_TPM = sd_TPM / sqrt(n()),
            mean_drag = mean(`Drag Coefficient`),
            sd_drag = sd(`Drag Coefficient`),
            se_drag = sd_drag / sqrt(n()),
            mean_Re = mean(`Reynolds Number`),
            sd_Re = sd(`Reynolds Number`),
            se_Re = sd_Re / sqrt(n()),
            mean_E = mean(Efficiency),
            sd_E = sd(Efficiency),
            se_E = sd_E / sqrt(n()),
            mean_speed = mean(Speed),
            sd_speed = sd(Speed),
            se_speed = sd_speed / sqrt(n()),
            Species = first(Species),
            Length = first(Length),
            Speed = first(Speed))

species_size <- d_reg_swimming %>% 
  group_by(Species) %>% 
  summarize(Size = mean(Length)) %>% 
  arrange(Size)
d_combine_swimming_summarized <- d_combine_swimming %>%  
  group_by(Individual, effort_type) %>% 
  summarize(mean_TPM = mean(TPM), 
            sd_TPM = sd(TPM),
            se_TPM = sd_TPM / sqrt(n()),
            mean_drag = mean(`Drag Coefficient`),
            sd_drag = sd(`Drag Coefficient`),
            se_drag = sd_drag / sqrt(n()),
            mean_Re = mean(`Reynolds Number`),
            sd_Re = sd(`Reynolds Number`),
            se_Re = sd_Re / sqrt(n()),
            mean_E = mean(Efficiency),
            sd_E = sd(Efficiency),
            se_E = sd_E / sqrt(n()),
            mean_speed = mean(Speed),
            sd_speed = sd(Speed),
            se_speed = sd_speed / sqrt(n()),
            Species = first(Species),
            Length = first(Length),
            Speed = first(Speed),
            FlukeArea = first(`Fluke Area (m)`)) %>% 
  ungroup %>% 
  mutate(Species = factor(Species, levels = species_size$Species),
         effort_type = factor(effort_type, levels = c("Normal", "Max")),
         Spec_Effort = paste(Species, effort_type,sep="-"),
         Spec_Effort = factor(Spec_Effort, levels = c("Minke-Normal", "Minke-Max", "Humpback-Normal", "Humpback-Max", 
                                                      "Fin-Normal", "Fin-Max", "Blue-Normal", "Blue-Max"))) %>% 
  filter(Species %in% c("Humpback", "Blue", "Minke")) #This selects only Blue, Humpback, and Minke

#Color Coding Species
pal <- c("Minke" = "firebrick3",  "Humpback" = "gray30", "Fin" = "chocolate3", "Blue" = "dodgerblue2")

