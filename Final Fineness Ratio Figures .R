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
# Fineness Ratio Figures
##############

# Figure Fineness Ratio vs. Fluke Area
FRvFA <- ggplot(data = d_full, aes(log10(`Fluke Area (m)`), log10(`Fineness Ratio`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
FRvFA

# Figure Fineness Ratio vs. Mass per Unit Length 
FRvMpUL <- ggplot(data = d_full, aes(log10(`Mass per Unit Length`), log10(`Fineness Ratio`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
FRvMpUL 

# Figure Fineness Ratio vs. Total Length 
FRvTL <- ggplot(data = d_full, aes(log10(`Total Length (m)`), log10(`Fineness Ratio`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
FRvTL

# Figure Fineness Ratio vs. Maximum Diameter 
FRvTL <- ggplot(data = d_full, aes(log10(`Maximum Diameter`), log10(`Fineness Ratio`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
FRvTL