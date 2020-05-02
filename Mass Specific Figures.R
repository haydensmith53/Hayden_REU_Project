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



ggplot(d_full, aes(log10(`Total Length (m).x`), log10(`Mass Specific Thrust`))) + geom_point() + geom_smooth(method = "lm", se =FALSE) +facet_wrap(~ Species)
ms_thrust_lm <- lm(log10(d_full$`Mass Specific Thrust`) ~ log10(d_full$`Total Length (m).x`)) 
summary(ms_thrust_lm)
d_full %>% group_by(Species) %>% group_map(~ lm(log10(`Mass Specific Thrust`) ~ log10(`Total Length (m).x`), data = .x))
thrust_lms <- d_full %>% filter(Species == "Blue") %>% lm(log10(`Mass Specific Thrust`) ~ log10(`Total Length (m).x`), data = .)
thrust_lms
thrust_bw <- thrust_lms
summary(thrust_bw)
thrust_mn <- d_full %>% filter(Species == "Humpback") %>% lm(log10(`Mass Specific Thrust`) ~ log10(`Total Length (m).x`), data = .)
summary(thrust_mn)
