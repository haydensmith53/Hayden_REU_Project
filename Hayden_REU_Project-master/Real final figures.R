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
         Species = factor(Species, levels = c("Minke", "Humpback", "Fin", "Blue"))) %>% 
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
  ungroup %>% 
  mutate(Species = factor(Species, levels = species_size$Species),
         effort_type = factor(effort_type, levels = c("Normal", "Max")),
         Spec_Effort = paste(Species, effort_type,sep="-"),
         Spec_Effort = factor(Spec_Effort, levels = c("Minke-Normal", "Minke-Max", "Humpback-Normal", "Humpback-Max", 
                                                      "Fin-Normal", "Fin-Max", "Blue-Normal", "Blue-Max"))) %>% 
  filter(Species %in% c("Humpback", "Blue")) #This selects only Blue and Humpback

#Color Coding Species
pal <- c("Minke" = "firebrick3",  "Humpback" = "gray30", "Fin" = "chocolate3", "Blue" = "dodgerblue2")

#Color Coding Effort
#pal <- c("Max" = "red3", "Normal"  = "royalblue2")


#################
# Mass-specific Thrust Figures ----
#################

# TPM vs.FA
TPMvFA <- ggplot(d_combine_swimming_summarized, aes(FlukeArea, mean_TPM)) +
  geom_smooth(method = 'lm', aes(group = Species), color = 'gray50') + 
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM, 
                      shape = effort_type,
                      color = Species),
                  position = position_jitter(width = 0.05), size = 0.5) +
  scale_color_manual(values = pal) + 
  labs(x = ('Fluke Area' ~m^2),
       y = ('Mean mass-specific thrust (N/kg)'),
       shape = "Effort type",
       color = ("Species")) + 
  ggtitle("Mass-Specific Thrust vs. Fluke Area") +
  theme_classic(base_size = 14) + 
  theme(plot.title = element_text(hjust = 0.5))
TPMvFA

statsmn <- lm(mean_TPM ~ FlukeArea, 
              data = filter(d_combine_swimming_summarized, Species == "Humpback"))
summary(statsmn)

statsbw <- lm(mean_TPM ~ FlukeArea, 
              data = filter(d_combine_swimming_summarized, Species == "Blue"))
summary(statsbw)

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
TPMvSpeed <- ggplot(d_combine_swimming_summarized, 
                    aes(mean_speed, mean_TPM, color = effort_type)) +
  geom_smooth(method = 'lm') + 
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM),
                  position = position_jitter(width = 0.05), size = 0.5) +
  scale_color_manual(values = pal) + 
  facet_wrap(.~Species) +
  labs(x = ('Speed (m/s)'),
       y = 'Mean mass-specific thrust (N/kg)', 
       color = "Effort type") +
  ggtitle("Mass-Specific Thrust vs. Speed") +
  theme_classic(base_size = 14) + 
theme(plot.title = element_text(hjust = 0.5))
TPMvSpeed

stats1 <- lm(mean_TPM ~ mean_speed, data = d_combine_swimming_summarized)
summary(stats1)

##############
# Normal swimming kinematics, four-part figure ----
##############
#Thrust
Pt <- ggplot(d_combine_swimming_summarized,
         aes(Species, mean_TPM, color = effort_type)) + 
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM,
                      shape = effort_type),
                  position = position_jitterdodge(jitter.width = 0.6), 
                  alpha = 0.6, 
                  size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Mean Mass-Specific Thrust (N/kg)', 
       color = "Effort type", 
       shape = "Effort type") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
Pt

# Drag
Cd <- ggplot(d_combine_swimming_summarized, 
             aes(Species, mean_drag, color = effort_type)) + 
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) +   
  geom_pointrange(aes(ymin = mean_drag - se_drag, 
                      ymax = mean_drag + se_drag, 
                      shape = effort_type),
                  position = position_jitterdodge(jitter.width = 0.6), 
                  alpha = 0.6, 
                  size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Drag Coefficient') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")
Cd

# Reynolds Number
Re <- ggplot(d_combine_swimming_summarized, 
             aes(Species, mean_Re,color = effort_type)) + 
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_pointrange(aes(ymin = mean_Re - se_Re, 
                      ymax = mean_Re + se_Re,
                      shape = effort_type),
                  position = position_jitterdodge(jitter.width = 0.6),
                  alpha = 0.6, 
                  size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Reynolds Number') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
Re


# Efficiency
E <- ggplot(d_combine_swimming_summarized, 
            aes(Species, mean_E, color = effort_type)) + 
  geom_boxplot(outlier.shape = NA,
               show.legend = FALSE) + 
  geom_pointrange(aes(ymin = mean_E - se_E, 
                      ymax = mean_E + se_E, 
                      shape = effort_type),
                  position = position_jitterdodge(jitter.width = 0.6), 
                  alpha = 0.6, 
                  size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Species",
       y = 'Propulsive Efficiency') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
E

Mn_data <- filter(d_combine_swimming_summarized, Species == "Humpback") %>% 
  spread(effort_type, mean_E)
t.test(Mn_data$Normal, Mn_data$Max, na.rm = TRUE)

Bw_data <- filter(d_combine_swimming_summarized, Species == "Blue") %>% 
  spread(effort_type, mean_E)
t.test(Bw_data$Normal, Bw_data$Max, na.rm = TRUE)

All_data <- spread(d_combine_swimming_summarized, Species, mean_E)
t.test(All_data$Humpback, All_data$Blue, na.rm = TRUE)

normal_swimming_plot <- ggarrange(Pt, Cd, Re, E, 
                                  ncol = 2, nrow = 2,  
                                  labels = c("A", "B", "C", "D"),
                                  common.legend = TRUE)

# annotate_figure(normal_swimming_plot,
#                 top = text_grob("Normal Swimming",
#                                 face = "bold", size = 14))

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
PEvTL <- ggplot(d_combine_swimming_summarized, aes(Length, mean_E)) +
  geom_smooth(method = 'lm', aes(group = Species), color = 'gray50') + 
  geom_pointrange(aes(ymin = mean_E - se_E, 
                      ymax = mean_E + se_E, 
                      #shape = effort_type,
                      color = Species),
                  position = position_jitter(width = 0.05), size = 0.5) +
  scale_color_manual(values = pal) + 
#  facet_wrap(~Species, scales = "free_x") +
  labs(x = ("Total Length (m)"),
       y = "Propulsive Efficiency",
       #shape = "Effort Type",
       color = "Species") + 
  ggtitle("Propulsive Efficiency vs.Total Length") +
  theme_classic(base_size = 14) + 
  theme(plot.title = element_text(hjust = 0.5))
PEvTL
stats2 <- lm(mean_E ~ Length, data = d_combine_swimming_summarized)
summary(stats2)

statsmn <- lm(mean_E ~ Length, 
              data = filter(d_combine_swimming_summarized, Species == "Humpback"))
summary(statsmn)

statsbw <- lm(mean_E ~ Length, 
              data = filter(d_combine_swimming_summarized, Species == "Blue"))
summary(statsbw)

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
  facet_wrap(~Species) +
  labs(x = "Speed (m/s)",
       y = "Propulsive Efficiency")
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

Pt <- ggplot(d_combine_swimming_summarized,
             aes(Length, mean_TPM, color = Species)) + 
  geom_smooth(method = 'lm') + 
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM),
                  position = position_jitter(width = 0.05), size = 0.5) +
  scale_color_manual(values = pal) +
  labs(x = ('Total Length (m)'),
       y = 'Mean mass-specific thrust (N/kg)', 
       color = "Effort type") +
  ggtitle("Mass-Specific Thrust vs. Speed") +
  theme_classic(base_size = 14) + 
  theme(plot.title = element_text(hjust = 0.5))
Pt

TPMvsTL <- ggplot(d_combine_swimming_summarized, aes(Length, mean_TPM)) +
  geom_smooth(method = 'lm', aes(group = Species), color = 'gray50') +
  geom_pointrange(aes(ymin = mean_TPM - se_TPM,
                      ymax = mean_TPM + se_TPM,
                      shape = effort_type,
                      color = Species),
                  position = position_jitter(width= 0.5), size = 0.5) +
  scale_color_manual(values = pal) + 
  labs(x = ('Total Length (m)'),
       y = ('Mean mass-specific thrust (N/kg)'),
       shape = "Effort type",
       color = ("Species")) + 
       ggtitle("Mass-Specific Thrust vs. Total Length") +
  theme_classic(base_size = 14) + 
  theme(plot.title = element_text(hjust = 0.5))
TPMvsTL

statsmn <- lm(mean_TPM ~ Length, 
              data = filter(d_combine_swimming_summarized, Species == "Humpback"))
summary(statsmn)

statsbw <- lm(mean_TPM ~ Length, 
              data = filter(d_combine_swimming_summarized, Species == "Blue"))
summary(statsbw)


