###############
#Code to combine csv files 
###############

library(tidyverse)

files <- dir("~/Desktop/Max Effort Droned Flukebeats Info/", full.names = TRUE, pattern = "*.csv") #Make sure to set working direction in files 
files

Max_Effort_Tailbeat_Full <- files %>% 
  map(read_csv, col_names = c("Frequency", "Depth", "Pitch", "Speed", "Individual", "Efficiency", "Thrust Power", "Drag Coefficient", "Reynolds Number")) %>% 
  reduce(rbind)
Max_Effort_Tailbeat_Full

write_csv(Max_Effort_Tailbeat_Full, "Max Effort Tailbeats Full.csv")

#Testing on one individual 
a <- read_csv("mn180831-30 thiswhalelungebeats.csv", 
              col_names = c("Frequency", "Depth", "Pitch", "Speed", "Individual", "Efficiency", "Thrust Power", "Drag Coefficient", "Reynolds Number"))

