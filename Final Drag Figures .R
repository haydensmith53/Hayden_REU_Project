####################
# Hayden's Figures
####################

#load packages----
library(tidyverse)
library(readxl)

# importing data----
d_master <- read_excel("Data Sheet with Fluke Area and Chord Length.xlsx")
d_matlab <-  read_excel("Good R MATLAB.xlsx")
# joining data frames
d_full <- left_join(d_master, d_matlab, by = c("ID #","Species", "Whale")) #ID 15 Fin Whale is not loading (NA)
##############
# Drag Figures
##############

# Figure Drag Coefficient vs. FLuke Area 
CdvFA <- ggplot(data = d_full, aes(log10(`Fluke Area (m)`), log10(`Average Drag Coefficient for Each Flukebeat`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
CdvFA

# Figure Drag Coefficient vs. Mass per Unit Length
CdvMpUL <- ggplot(data = d_full, aes(log10(`Mass per Unit Length`), log10(`Average Drag Coefficient for Each Flukebeat`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
CdvMpUL

# Figure Drag Coefficient vs. Total Length
CdvTL <- ggplot(data = d_full, aes(log10(`Total Length (m)`), log10(`Average Drag Coefficient for Each Flukebeat`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
CdvTL

# Figure Drag Coefficient vs. Maximum Diameter
CdvMD <- ggplot(data = d_full, aes(log10(`Maximum Diameter`), log10(`Average Drag Coefficient for Each Flukebeat`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
CdvMD

# Figure Drag Coefficient vs. Fineness Ratio
CdvFR <- ggplot(data = d_full, aes(log10(`Fineness Ratio`), log10(`Average Drag Coefficient for Each Flukebeat`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
CdvFR