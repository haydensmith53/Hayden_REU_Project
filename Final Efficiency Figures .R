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
# Efficiency Figures
##############

# Figure Efficiency vs. Fluke Area
EvFA <- ggplot(data = d_full, aes(log10(`Fluke Area (m)`), log10(`Average Individual Efficiency`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
EvFA

# Figure Efficiency vs. Mass per Unit Length
EvMpUL <- ggplot(data = d_full, aes(log10(`Mass per Unit Length`), log10(`Average Individual Efficiency`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
EvMpUL

# Figure Efficiency vs. Total Length
EvTL <- ggplot(data = d_full, aes(log10(`Total Length (m)`), log10(`Average Individual Efficiency`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
EvTL

# Figure Efficiency vs. Maximum Diameter 
EvMD <- ggplot(data = d_full, aes(log10(`Maximum Diameter`), log10(`Average Individual Efficiency`))) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = 'lm')
EvMD

# Figure Efficieny vs. Fineness Ratio
EvFR <- ggplot(data = d_full, aes(log10(`Fineness Ratio`), log10(`Average Individual Efficiency`))) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(group = Species), method = 'lm')
EvFR

mE <- lm(log10(`Average Drag Coefficient for Each Flukebeat`) ~ log10(`Fluke Area (m)`),
             data = filter(d_full, Species == "Humpback"))
summary(mFAvCd)