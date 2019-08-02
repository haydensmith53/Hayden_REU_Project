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

morphometrics <- read_csv("Copy of Data Sheet For Hayden.csv") 
#All Data
d_all_swimming <- read_csv("Copy of Droned Tailbeats Info Hayden.csv") %>% 
  select(-(X15:X19))

#Separating max flukebeats from all data
d_max_swimming <- read_csv("Max Effort Droned Tailbeats FINAL.csv") %>% 
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
         Species = as_factor(Species, levels = c("Minke", "Humpback", "Fin", "Blue"))) %>% 
  left_join(morphometrics, by = c("Species", "Individual")) 


species_size <- d_reg_swimming %>% 
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
  mutate(Species = factor(Species, levels = species_size$Species),
         effort_type = factor(effort_type),
         effort_type = arrange(-desc(effort_type))) %>% 
  filter(Species %in% c("Humpback", "Blue")) #This selects only Blue and Humpback

#Color Coding Species
pal <- c("Minke" = "firebrick3",  "Humpback" = "gray30", "Fin" = "chocolate3", "Blue" = "dodgerblue2")

#################
# Mass-specific Thrust Figures
#################

# TPM vs.FA
TPMvFA <- ggplot(d_reg_swimming_summarized, aes(FlukeArea, mean_TPM)) +
  geom_smooth(method = 'lm') + 
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM, 
                      color = Species),
                  position = position_jitter(width = 0.05), size = 0.5) +
  scale_color_manual(values = pal) + 
  labs(x = bquote("Fluke area"~m^2),
       y = 'Mean mass-specific thrust (N/kg)') +
  theme_classic(base_size = 14)
  #theme(legend.position = "none")
print(TPMvFA + ggtitle("                   Mass-Specific Thrust vs. Fluke Area"))

stats <- lm(mean_TPM ~ FlukeArea, data = d_reg_swimming_summarized)
summary(stats)

# TPM vs. Species
TPMvSpecies <- ggplot(d_reg_swimming_summarized, 
       aes(Species, mean_TPM, color = Species)) + 
  geom_boxplot(notch = TRUE,
               outlier.shape = NA) + 
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM),
                  position = position_jitter(width = 0.25), size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Mass Specific Thrust (N/kg)') +
  theme_classic(base_size = 14)
  #theme(legend.position = "none") 
print(TPMvSpecies + ggtitle("                              Mass-Specific Thrust Across Species"))

# TPM vs. Speed
TPMvSpeed <- ggplot(d_reg_swimming_summarized, aes(Speed, mean_TPM)) +
  geom_smooth(method = 'lm') + 
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM, 
                      color = Species),
                  position = position_jitter(width = 0.05), size = 0.5) +
  scale_color_manual(values = pal) + 
  labs(x = ('Speed (m/s)'),
       y = 'Mean mass-specific thrust (N/kg)') +
  theme_classic(base_size = 14)
#theme(legend.position = "none"))
print(TPMvSpeed + ggtitle("                         Mass-Specific Thrust vs. Speed"))
stats1 <- lm(mean_TPM ~ Speed, data = d_reg_swimming_summarized)
summary(stats1)

##############
# Normal swimming kinematics
##############
#Thrust
Pt <- d_combine_swimming_summarized %>% 
  mutate(Spec_Effort = paste(Species, effort_type,sep="-")) %>% 
  group_by(Species, effort_type) %>% 
  ggplot(aes(Species, mean_TPM, color = Species, shape = effort_type)) + 
  geom_boxplot(aes(group = Spec_Effort),
               notch = TRUE,
               outlier.shape = NA) + 
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM),
                  position = position_jitterdodge(), size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Mass Specific Thrust (N/kg)') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
Pt

# Drag
Cd <- ggplot(d_reg_swimming_summarized, 
             aes(Species, mean_drag, color = Species)) + 
  geom_boxplot(notch = TRUE,
               outlier.shape = NA) + 
  geom_pointrange(aes(ymin = mean_drag - se_drag, 
                      ymax = mean_drag + se_drag),
                  position = position_jitter(width = 0.25), size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Drag Coefficient') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")
Cd

# Reynolds Number
Re <- ggplot(d_reg_swimming_summarized, 
             aes(Species, mean_Re, color = Species)) + 
  geom_boxplot(notch = TRUE,
               outlier.shape = NA) + 
  geom_pointrange(aes(ymin = mean_Re - se_Re, 
                      ymax = mean_Re + se_Re),
                  position = position_jitter(width = 0.25), size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Reynolds Number') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
Re

# Efficiency
E <- ggplot(d_reg_swimming_summarized, 
            aes(Species, mean_E, color = Species)) + 
  geom_boxplot(notch = TRUE,
               outlier.shape = NA) + 
  geom_pointrange(aes(ymin = mean_E - se_E, 
                      ymax = mean_E + se_E),
                  position = position_jitter(width = 0.25), size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Propulsive Efficiency') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
E

normal_swimming_plot <- ggarrange(Pt, Cd, Re, E, 
                                  ncol = 2, nrow = 2,  
                                  labels = c("A", "B", "C", "D"))
annotate_figure(normal_swimming_plot,
                top = text_grob("Normal Swimming",
                                face = "bold", size = 14))

ggplot(d_reg_swimming_summarized, aes(Length, mean_E, color = Species)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

#############
# Max Effort Swimming Kinematics
#############



#Thrust
maxPt <- ggplot(d_max_swimming_summarized, 
                aes(Species, mean_TPM, color = Species)) + 
  geom_boxplot(notch = TRUE,
               outlier.shape = NA) + 
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM),
                  position = position_jitter(width = 0.25), size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Mass Specific Thrust (N/kg)') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
maxPt

# Drag
maxCd <- ggplot(d_max_swimming_summarized, 
                aes(Species, mean_drag, color = Species)) + 
  geom_boxplot(notch = TRUE,
               outlier.shape = NA) + 
  geom_pointrange(aes(ymin = mean_drag - se_drag, 
                      ymax = mean_drag + se_drag),
                  position = position_jitter(width = 0.25), size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Drag Coefficient') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
maxCd

# Reynolds Number
maxRe <- ggplot(d_max_swimming_summarized, 
                aes(Species, mean_Re, color = Species)) + 
  geom_boxplot(notch = TRUE,
               outlier.shape = NA) + 
  geom_pointrange(aes(ymin = mean_Re - se_Re, 
                      ymax = mean_Re + se_Re),
                  position = position_jitter(width = 0.25), size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Reynolds Number') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
maxRe

# Efficiency
maxE <- ggplot(d_max_swimming_summarized, 
               aes(Species, mean_E, color = Species)) + 
  geom_boxplot(notch = TRUE,
               outlier.shape = NA) + 
  geom_pointrange(aes(ymin = mean_E - se_E, 
                      ymax = mean_E + se_E),
                  position = position_jitter(width = 0.25), size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Propulsive Efficiency') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
maxE

max_swimming_plot <- ggarrange(maxPt, maxCd, maxRe, maxE, 
                               ncol = 2, nrow = 2,  
                               labels = c("A", "B", "C", "D"))
annotate_figure(max_swimming_plot,
                top = text_grob("Max Effort Swimming",
                                face = "bold", size = 14))


###############
#Propulsive Efficiency
###############

# PE vs. TL
PEvTLL <- ggplot(d_reg_swimming_summarized, aes(Length, mean_E)) +
  geom_smooth(method = 'lm') + 
  geom_pointrange(aes(ymin = mean_E - se_E, 
                      ymax = mean_E + se_E, 
                      color = Species),
                  position = position_jitter(width = 0.05), size = 0.5) +
  scale_color_manual(values = pal) + 
  labs(x = ("Total Length (m)"),
       y = "Propulsive Efficiency") +
  theme_classic(base_size = 14)
#theme(legend.position = "none")
print(PEvTLL + ggtitle("                   Propulsive Efficiency vs. Total Length"))
stats2 <- lm(mean_E ~ Length, data = d_reg_swimming_summarized)
summary(stats2)

normalSpeed <- ggplot(d_reg_swimming_summarized, 
                      aes(mean_speed, mean_E, color = Species)) + 
  geom_boxplot(notch = TRUE,
               outlier.shape = NA) + 
  geom_pointrange(aes(ymin = mean_E - se_E, 
                      ymax = mean_E + se_E),
                  position = position_jitter(width = 0.25), size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Speed (m/s)",
       y = 'Propulsive Efficiency') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
normalSpeed

SpeciesSpeed <- ggplot(d_reg_swimming, aes(Speed, Efficiency, color = Species, group = Individual)) +
  #geom_point() + 
  geom_smooth() +
  facet_wrap(~Species)
annotate_figure(SpeciesSpeed,
                top = text_grob("Propulsive Efficiency vs. Speed",
                                face = "bold", size = 14))



#PEvTL <- ggplot(d_reg_swimming_summarized, 
#                       aes(Length, mean_E, color = Species)) + 
#   geom_boxplot(notch = TRUE,
#                outlier.shape = NA) + 
#   geom_pointrange(aes(ymin = mean_E - se_E, 
#                       ymax = mean_E + se_E),
#                   position = position_jitter(width = 0.25), size = .4) +
#   scale_color_manual(values = pal) + 
#   labs(x = "Total Length (m)",
#        y = 'Propulsive Efficiency') +
#   theme_classic(base_size = 14) +
#   theme(legend.position = "none") 
# PEvTL



