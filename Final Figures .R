####################
# Hayden's Figures
####################

#load packages----
library(tidyverse)
library(readxl)

#https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf
#good site ggplot guides

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