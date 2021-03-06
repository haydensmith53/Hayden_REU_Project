# Load packages and data----
library(ggplot2)
library(ggpubr)
# Allometric equations from Shirel's paper
# creating fucntions from Shirel's paper for MW (in kg) for engulfment capacity in liters for each species where we have a known length
Mass_SKR <- tribble(
  ~Species, ~slope,   ~intercept,
  "Minke",     1.8454,  2.1399,
  "Fin",     2.595,  1.28388,
  "Blue",     3.3346, 0.3129,
  "Humpback",     2.3373,  1.8535
)

d_reg_swimming <- read_csv("Copy of Droned Tailbeats Info Hayden.csv") %>% 
  left_join(Mass_SKR, by = "Species") %>% 
  mutate(Mass = (Length^slope)*10^intercept, 
         TPM = `Thrust Power`/Mass)

d_reg_swimming_summarized <- d_reg_swimming %>%  
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
            

#Thrust
Pt <- ggplot(d_reg_swimming_summarized, 
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

# Propulsive Efficiency vs. Speed (Max)
normalSpeed <- ggplot(d_reg_swimming_summarized, 
                   aes(mean_speed, mean_E, color = Species)) + 
  geom_boxplot(notch = FALSE,
               outlier.shape = NA) + 
  geom_pointrange(aes(xmin = mean_speed - se_speed,
                      xmax = mean_speed + se_speed,
                      ymin = mean_E - se_E, 
                      ymax = mean_E + se_E),
                  position = position_jitter(width = 0.25), size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Speed",
       y = 'Propulsive Efficiency') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
normalSpeed


###########
# Max Effort, had to set notch = FALSE (instead of TRUE)
###########
d_max_swimming <- read_csv("Droned Max Effort Tailbeats.csv") %>% 
left_join(Mass_SKR, by = "Species") %>% 
  mutate(Mass = (Length^slope)*10^intercept, 
         TPM = `Thrust Power`/Mass) %>% 
  filter(Species %in% c("Blue", "Humpback"))

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

#Thrust
maxPt <- ggplot(d_max_swimming_summarized, 
             aes(Species, mean_TPM, color = Species)) + 
  geom_boxplot(notch = FALSE,
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
  geom_boxplot(notch = FALSE,
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
  geom_boxplot(notch = FALSE,
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
  geom_boxplot(notch = FALSE,
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

ggplot(d_max_swimming_summarized, aes(Length, mean_E, color = Species)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# Propulsive Efficiency vs. Speed (Max)
maxSpeed <- ggplot(d_max_swimming_summarized, 
                aes(mean_speed, mean_E, color = Species)) + 
  geom_boxplot(notch = FALSE,
               outlier.shape = NA) + 
  geom_pointrange(aes(xmin = mean_speed - se_speed,
                      xmax = mean_speed + se_speed,
                      ymin = mean_E - se_E, 
                      ymax = mean_E + se_E),
                  position = position_jitter(width = 0.25), size = .4) +
  scale_color_manual(values = pal) + 
  labs(x = "Speed",
       y = 'Propulsive Efficiency') +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 
maxSpeed
