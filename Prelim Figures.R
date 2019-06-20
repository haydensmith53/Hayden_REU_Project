####################
# Hayden's prelim vis
####################

#load packages----
library(tidyverse)
library(readxl)

# importing data----

d_master <- read_excel("Data Sheet with Fluke Area and Chord Length.xlsx")
d_matlab <-  read_excel("Matlab Drag, Thrust, and Reynolds.xlsx")
 
# joining data frames
d_full <- left_join(d_master, d_matlab, by = c("ID #","Species", "Whale")) #ID 15 Fin Whale is not loading (NA)


# Prelim graphs ----
#TLvFA <- ggplot(data = d_full, aes(log10(`Total Length (m)`), log10(`Fluke Area (m)`) )) + 
 # geom_point(aes(color = Species)) + 
  #geom_smooth(aes(group = Species), method = "lm") 
#TLvFA

#TLvPt <- ggplot(data = d_full, aes(log10(`Total Length (m)`), log10(`Thrust`) )) + 
 # geom_point(aes(color = Species)) + 
  #geom_smooth(aes(group = Species), method = 'lm')
#TLvPt 

#TLvCd <- ggplot(data = d_full, aes(log10(`Total Length (m)`), log10(`Average Drag Coefficient for Each Flukebeat`) )) +
 # geom_point(aes(color = Species)) + 
  #geom_smooth(aes(group = Species), method = 'lm')
#TLvD

#FAvPt <- ggplot(data = d_full, aes(log10(`Fluke Area (m)`), log10(`Thrust`) )) + 
 # geom_point(aes(color = Species)) + 
  #geom_smooth(aes(group = Species), method = 'lm')
#FAvPt

#FAvCd <- ggplot(data = d_full, aes(log10(`Fluke Area (m)`), log10(`Average Drag Coefficient for Each Flukebeat`) )) +
 # geom_point(aes(color = Species)) +
  #geom_smooth(aes(group = Species), method = 'lm')
#FAvCd

#TLvRe <- ggplot(data = d_full, aes(log10(`Total Length (m)`), log10(`Reynolds Number`) )) +
 # geom_point(aes(color = Species)) + 
  #geom_smooth(aes(group = Species), method = 'lm')
#TLvRe

#MaxDvPt <- ggplot(data = d_full, aes(log10(`Maximum Diameter`), log10(`Thrust`) )) +
  #geom_point(aes(color = Species)) +
  #geom_smooth(aes(group = Species), method = 'lm')
#MaxDvPt

#MaxDvCd <- ggplot(data = d_full, aes(log10(`Maximum Diameter`), log10(`Average Drag Coefficient for Each Flukebeat`) )) +
 #geom_point(aes(color = Species)) +
# geom_smooth(aes(group = Species), method = 'lm')
#MaxDvCd

#MaxDvRe <- ggplot(data = d_full, aes(log10(`Maximum Diameter`), log10(`Reynolds Number`) )) +
  #geom_point(aes(color = Species)) +
  #geom_smooth(aes(group = Species), method = 'lm')
#MaxDvRe

MaxDvTL <- ggplot(data = d_full, aes(log10(`Maximum Diameter`), log10(`Total Length (m)`) )) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(group = Species), method = 'lm')
MaxDvTL
