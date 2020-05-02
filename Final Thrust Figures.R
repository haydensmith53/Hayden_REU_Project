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
# Thrust Figures
##############

# Figure Thrust vs. Fluke Area 
PtvFA <- ggplot(data = d_full, aes(log10(`Fluke Area (m)`), log10(`Average Thrust Power for Each Flukebeat (#)`) )) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(group = Species), method = 'lm')
PtvFA

# Figure Thrust vs. Mass per Unit Length 
PtvMpUL <- ggplot(data = d_full, aes(log10(`Mass per Unit Length`), log10(`Average Thrust Power for Each Flukebeat (#)`) )) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(group = Species), method = 'lm')
PtvMpUL

# Figure Thrust vs. Total Length
PtvTL <- ggplot(data = d_full, aes(log10(`Total Length (m)`), log10(`Average Thrust Power for Each Flukebeat (#)`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
PtvTL

# Figure Thrust vs. Maximum Diameter 
PtvMD <- ggplot(data = d_full, aes(log10(`Maximum Diameter`), log10(`Average Thrust Power for Each Flukebeat (#)`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
PtvMD

# Figure Thrust vs. Fineness Ratio
PtvFR <- ggplot(data = d_full, aes(log10(`Fineness Ratio`), log10(`Average Thrust Power for Each Flukebeat (#)`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
PtvFR