#### Load packages and data ####
library(cowplot)
library(ggplot2)
library(ggpubr)
library(tidyverse)

# Abbreviate a binomial e.g. Balaenoptera musculus -> B. musculus
abbr_binom <- function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}

#### Shirel's Allometric Eqs ####
# creating fucntions from Shirel's paper for MW (in kg) for engulfment capacity in liters for each species where we have a known length
Mass_SKR <- tribble(
  ~Species, ~slope,   ~intercept,
  "Balaenoptera bonaerensis",     1.8454,  2.1399,
  "Balaenoptera physalus",     2.595,  1.28388,
  "Balaenoptera musculus",     3.3346, 0.3129,
  "Megaptera novaeangliae",     2.3373,  1.8535
)

#### Flukebeat and Morphometric Data #### 
morphometrics <- read_csv("10_14 Data Sheet For Hayden.csv") %>% 
  rename(Individual = `ID #`,
         ID = "Whale",
         `Common name` = Species) %>% 
  mutate(Species = factor(case_when(
           `Common name` == "Blue" ~ "Balaenoptera musculus",
           `Common name` == "Fin" ~ "Balaenoptera physalus",
           `Common name` == "Humpback" ~ "Megaptera novaeangliae",
           `Common name` == "Minke" ~ "Balaenoptera bonaerensis")))

# All Data
d_all_swimming <- read_csv("10_2 Droned Tailbeats Info Hayden.csv") %>% 
  left_join(select(morphometrics, Individual, ID), by = "Individual") %>% 
  select(-(X15:X19)) %>% 
  rename(`Common name` = Species) %>% 
  mutate(Species = factor(case_when(
    `Common name` == "Blue" ~ "Balaenoptera musculus",
    `Common name` == "Fin" ~ "Balaenoptera physalus",
    `Common name` == "Humpback" ~ "Megaptera novaeangliae",
    `Common name` == "Minke" ~ "Balaenoptera bonaerensis")))

# Separating max flukebeats from all data
d_max_swimming <- read_csv("10_2 AllWhaleMaxEffortBeats.csv") %>% 
  rename(`Common name` = Species) %>% 
  mutate(Species = factor(case_when(
    `Common name` == "Blue" ~ "Balaenoptera musculus",
    `Common name` == "Fin" ~ "Balaenoptera physalus",
    `Common name` == "Humpback" ~ "Megaptera novaeangliae",
    `Common name` == "Minke" ~ "Balaenoptera bonaerensis"))) %>% 
  left_join(select(morphometrics, Individual, ID), by = "Individual") %>% 
  semi_join(d_all_swimming, by = colnames(d_all_swimming)[c(1:4, 7:8, 15)]) %>% 
  mutate(effort_type = "Max") %>%
  left_join(Mass_SKR, by = "Species") %>% 
  mutate(Mass = (Length^slope)*10^intercept, 
         TPM = `Thrust Power`/Mass)

# Separating normal fluekbeats from all data
d_reg_swimming <- d_all_swimming %>%  
  anti_join(d_max_swimming, by = colnames(d_all_swimming)[c(1:4, 7:8, 15)]) %>% 
  mutate(effort_type = "All") %>%
  left_join(Mass_SKR, by = "Species") %>% 
  mutate(Mass = (Length^slope)*10^intercept, 
         TPM = `Thrust Power`/Mass) %>% 
  left_join(select(morphometrics, -`Common name`), by = c("Species", "Individual", "ID")) 

# Combining normal and max data, this is the data file that we will use 
d_combine_swimming <- bind_rows(d_reg_swimming, d_max_swimming)

# Summarizing d_max_swimming
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

# Summarizing d_combine_swimming
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
            Speed = first(Speed)) %>% 
  ungroup %>% 
  left_join(select(morphometrics, -`Common name`), by = c("Species", "Individual")) %>% 
  mutate(Species = factor(Species, levels = species_size$Species)) %>%
  mutate(effort_type = replace(effort_type, effort_type == "All", "Normal"))

###########################
#### Graphs Start Here ####
###########################

#### Color Palette ####
pal <- c("B. bonaerensis" = "firebrick3",  "M. novaeangliae" = "gray30",  "B. musculus" = "dodgerblue2")

#### MST ~ U, A, L, A/L ####
# (kinematics and morphology) 
# (Just normal, no max effort)
normal_effort <- d_combine_swimming_summarized %>% 
  filter(effort_type == "Normal",
         Species != "Balaenoptera physalus") %>% 
  mutate(sp_abbr = abbr_binom(Species))
fig3_U <- ggplot(normal_effort, aes(mean_speed, mean_TPM)) +
  geom_point(aes(color = sp_abbr)) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = pal) +
  expand_limits(y = 0) +
  labs(x = "Speed (m s-1)",
       y = "Mass-Specific Thrust (N kg-1)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
fig3_A <- ggplot(normal_effort, aes(`Fluke Area (m)`, mean_TPM)) +
  geom_point(aes(color = sp_abbr)) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = pal) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 12) +
  theme(axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank())
fig3_L <- ggplot(normal_effort, aes(`Total Length (m)`, mean_TPM)) +
  geom_point(aes(color = sp_abbr)) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = pal) +
  expand_limits(y = 0) +
  labs(y = "Mass-Specific Thrust (N kg-1)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
fig3_AL <- ggplot(normal_effort, aes(`FA/L`, mean_TPM)) +
  geom_point(aes(color = sp_abbr)) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = pal) +
  expand_limits(y = 0) +
  labs(x = "Fluke Area / Length (m)") +
  theme_minimal(base_size = 12) +
  theme(axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank())
# Combine into one figure
fig3 <- plot_grid(fig3_U, fig3_A, fig3_L, fig3_AL,
                  nrow = 2,
                  align = "h",
                  rel_widths = c(1.1, 0.9),
                  labels = "AUTO")
ggsave("figs/fig3.pdf", height = 90, width = 90, units = "mm", dpi = 300)

## TODO change text and point sizes, fit linear mixed effects model, get prediction intervals from lme

#### MST ~ L, max vs normal ####
combined_effort <- d_combine_swimming_summarized %>% 
  filter(Species != "Balaenoptera physalus") %>% 
  mutate(sp_abbr = abbr_binom(Species))
# Remove outliers from figure
fig4 <- combined_effort %>% 
  filter(mean_TPM < 1.6) %>% 
  ggplot(aes(`Total Length (m)`, mean_TPM, color = effort_type)) +
  geom_point(aes(shape = sp_abbr)) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c(Normal = "blue", Max = "red")) +
  labs(y = "Mass-Specific Thrust (N kg-1)") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
ggsave("figs/fig4.pdf", height = 90, width = 90, units = "mm", dpi = 300)

## TODO fit LME, get prediction intervals, change text and point sizes

#### Prop Eff ~ U, L ####
fig5_U <- normal_effort %>% 
  ggplot(aes(mean_speed, mean_E)) +
  geom_point(aes(color = sp_abbr)) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = pal) +
  labs(x = "Speed (m s-1)",
       y = "Propulsive Efficiency") +
  expand_limits(y = c(0.75, 1)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
fig5_L <- normal_effort %>% 
  ggplot(aes(`Total Length (m)`, mean_E)) +
  geom_point(aes(color = sp_abbr)) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = pal) +
  expand_limits(y = c(0.75, 1)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank())
fig5 <- plot_grid(fig5_U, fig5_L,
                  nrow = 1,
                  align = "h",
                  rel_widths = c(1.1, 0.9),
                  labels = "AUTO")
ggsave("figs/fig5.pdf", height = 90, width = 90, units = "mm", dpi = 300)

## TODO LME, play with sizes/dimensions

#### Drag ~ L w/ CFD ####
potvin_cfd <- read_csv("potvin_cfd.csv")
fig7 <- ggplot(normal_effort, aes(`Total Length (m)`, mean_drag)) +
  geom_point(aes(color = sp_abbr)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(color = sp_abbr), potvin_cfd, size = 3, shape = 17) +
  scale_color_manual(values = pal) +
  labs(y = "Drag Coefficient") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
ggsave("figs/fig7.pdf", height = 90, width = 90, units = "mm", dpi = 300)

## TODO y'know

######
#Normal and Maximum Effort Swimming Four part figure
######

#Thrust
Pt <- ggplot(filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"),
             aes(fct_reorder(abbr_binom(Species), mean_TPM), mean_TPM)) + 
  geom_boxplot(aes(color = effort_type),
               outlier.shape = NA,
               show.legend = FALSE) +
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM,
                      shape = abbr_binom(effort_type),
                      color = Species),
                  position = position_jitterdodge(jitter.width = 0.6), 
                  alpha = 0.8, 
                  size = 1.5) +
  labs(x = "Species",
       y = 'Mean Mass-Specific Thrust (N/kg)',
       color = "Species",
       shape = "Effort type") +
  theme_classic(base_size = 32) +
  theme(axis.text.x = element_text(face = "italic"),
        legend.position = "none")  + 
  scale_shape_discrete(guide = guide_legend(label.theme = element_text(angle = 0, face = "italic")))
Pt

# Drag
Cd <- ggplot(filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"),
             aes(fct_reorder(abbr_binom(Species), mean_drag), mean_drag)) + 
  geom_boxplot(aes(color = effort_type),
               outlier.shape = NA,
               show.legend = FALSE) +
  geom_pointrange(aes(ymin = mean_drag -se_drag, 
                      ymax = mean_drag + se_drag,
                      shape = abbr_binom(effort_type),
                      color = Species),
                  position = position_jitterdodge(jitter.width = 0.6), 
                  alpha = 0.8, 
                  size = 1.5) +
  labs(x = "Species",
       y = 'Drag coefficient',
       color = "Species",
       shape = "Effort type") +
  theme_classic(base_size = 32) +
  theme(axis.text.x = element_text(face = "italic"),
        legend.position = "none") + 
  scale_shape_discrete(guide = guide_legend(label.theme = element_text(angle = 0, face = "italic")))
Cd

# Reynolds Number
Re <- ggplot(filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"),
             aes(fct_reorder(abbr_binom(Species), mean_Re), mean_Re)) + 
  geom_boxplot(aes(color = effort_type),
               outlier.shape = NA,
               show.legend = FALSE) + 
  geom_pointrange(aes(ymin = mean_Re - se_Re, 
                      ymax = mean_Re + se_Re,
                      shape = abbr_binom(effort_type),
                      color = Species),
                  position = position_jitterdodge(jitter.width = 0.6), 
                  alpha = 0.8, 
                  size = 1.5) +
  labs(x = "Species",
       y = 'Reynolds number',
       color = "Species",
       shape = "Effort type") +
  theme_classic(base_size = 32) +
  theme(axis.text.x = element_text(face = "italic"),
        legend.position = "none") + 
  scale_shape_discrete(guide = guide_legend(label.theme = element_text(angle = 0, face = "italic")))
Re

# Efficiency
E <- ggplot(filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"),
            aes(fct_relevel(abbr_binom(Species), "B. bonaerensis", "M. novaeangliae"), mean_E)) + 
  geom_boxplot(aes(color = effort_type),
               outlier.shape = NA,
               show.legend = FALSE) +
  geom_pointrange(aes(ymin = mean_E - se_E, 
                      ymax = mean_E + se_E,
                      shape = abbr_binom(effort_type),
                      color = Species),
                  position = position_jitterdodge(jitter.width = 0.6), 
                  alpha = 0.8, 
                  size = 1.5) +
  labs(x = "Species",
       y = 'Propulsive Efficiency',
       color = "Species",
       shape = "Effort type") +
  theme_classic(base_size = 32) +
  theme(axis.text.x = element_text(face = "italic"),
        legend.position = "none") + 
  scale_shape_discrete(guide = guide_legend(label.theme = element_text(angle = 0, face = "italic")))
E

#Combine for four parts
Swimming_plot <- ggarrange(Pt, Cd, Re, E, 
        ncol = 2, nrow = 2,  
        labels = c("A", "B", "C", "D"))
Swimming_plot

dev.copy2pdf(file="Swimming_plot.pdf", width=26, height=20)

############
#Thrust Per Unit Mass Plots
############

#TPM vs. Speed
TPMvSpeed <- ggplot(filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"), 
                    aes(mean_speed, mean_TPM,
                        color = Species)) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_smooth(aes(x = mean_speed, y = mean_TPM), method = 'lm', se = TRUE, color = '#000000') +
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM,
                      shape = effort_type,
                      color = Species),
                  alpha = 0.6, 
                  size = 1.5) +
  labs(x = ('Swimming Speed (m/s)'),
       y = 'Mass-specific thrust (N/kg)', 
       shape = "Effort type",
       color = "Species") +
  theme_classic(base_size = 32) + 
  scale_shape_discrete(guide = guide_legend(label.theme = element_text(angle = 0, face = "italic")))
TPMvSpeed

dev.copy2pdf(file="TPMvSpeed.pdf", width=26, height=16)

#TPM vs. Total Length
TPMvTL <- ggplot(filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"), 
                    aes(Length, mean_TPM,
                        color = Species)) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_smooth(aes(x = Length, y = mean_TPM), method = 'lm', se = TRUE, color = '#000000') +
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM,
                      shape = effort_type,
                      color = Species), 
                  alpha = 0.6, 
                  size = 1.5) +
  labs(x = ('Total Length (m)'),
       y = 'Mass-specific thrust (N/kg)', 
       shape = "Effort type",
       color = "Species") +
  theme_classic(base_size = 32) + 
  scale_shape_discrete(guide = guide_legend(label.theme = element_text(angle = 0, face = "italic")))
TPMvTL

dev.copy2pdf(file="TPMvTL.pdf", width=26, height=16)

#TPM vs. Fluke Area 
TPMvFA <- ggplot(filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"),  
                 aes(`Fluke Area (m)`, mean_TPM,
                     color = Species)) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_smooth(aes(x = `Fluke Area (m)`, y = mean_TPM), method = 'lm', se = TRUE, color = '#000000') +
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM,
                      shape = effort_type,
                      color = Species),
                  alpha = 0.6, 
                  size = 1.5) +
  labs(x = ('Fluke Area' ~m^2),
       y = ('Mass-specific thrust (N/kg)'),
       shape = "Species",
       color = "Effort type") + 
  theme_classic(base_size = 32) + 
  scale_shape_discrete(guide = guide_legend(label.theme = element_text(angle = 0, face = "italic")))
TPMvFA

dev.copy2pdf(file="TPMvFA.pdf", width=26, height=16)

#TPM vs. Fluke Area/Total Length 
TPMvFA.TL <- ggplot(filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"),  
                 aes(`FA/L`, mean_TPM,
                     color = Species)) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_smooth(aes(x = `FA/L`, y = mean_TPM), method = 'lm', se = TRUE, color = '#000000') +
  geom_pointrange(aes(ymin = mean_TPM - se_TPM, 
                      ymax = mean_TPM + se_TPM,
                      shape = effort_type,
                      color = Species),
                  alpha = 0.6, 
                  size = 1.5) +
  labs(x = ('Fluke Area/Total Length (m)'),
       y = ('Mass-specific thrust (N/kg)'),
       shape = "Species",
       color = "Effort type") + 
  theme_classic(base_size = 32) + 
  scale_shape_discrete(guide = guide_legend(label.theme = element_text(angle = 0, face = "italic")))
TPMvFA.TL

dev.copy2pdf(file="TPMvFATL.pdf", width=26, height=16)

TPM_plot <- ggarrange(TPMvSpeed, TPMvTL, TPMvFA.TL,
                           ncol = 2, nrow = 2,
                           labels = c("A", "B", "C"),
                           common.legend = TRUE)
TPM_plot

dev.copy2pdf(file="TPM_Plot.pdf", width=18, height=18)

############
#Propulsive Efficiency graphs
############

#Propulsive Efficiency vs. Total Length
PEvTL <- ggplot(filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"),  
                aes(Length, mean_E,
                    color = Species)) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_smooth(aes(x = Length, y = mean_E), method = 'lm', se = TRUE, color = '#000000') +
  geom_pointrange(aes(ymin = mean_E - se_E, 
                      ymax = mean_E + se_E,
                      shape = effort_type,
                      color = Species),
                  alpha = 0.6, 
                  size = 1.5) +
  labs(x = ('Total length (m)'),
       y = ('Propulsive efficiency'),
       shape = "Effort type",
       color = "Species") + 
  theme_classic(base_size = 32) + 
  scale_shape_discrete(guide = guide_legend(label.theme = element_text(angle = 0, face = "italic")))
PEvTL 

dev.copy2pdf(file="PEvTL.pdf", width=26, height=16)


#Propulsive Efficiency vs. Speed
PEvSpeed <- ggplot(filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"),  
                   aes(Speed, mean_E,
                       color = Species)) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_smooth(aes(x = Speed, y = mean_E), method = 'lm', se = TRUE, color = '#000000') +
  geom_pointrange(aes(ymin = mean_E - se_E, 
                      ymax = mean_E + se_E,
                      shape = effort_type,
                      color = Species),
                  alpha = 0.6, 
                  size = 1.5) +
  labs(x = ('Speed (m/s)'),
       y = ('Propulsive efficiency'),
       shape = "Effort type",
       color = "Species") + 
  theme_classic(base_size = 32) + 
  scale_shape_discrete(guide = guide_legend(label.theme = element_text(angle = 0, face = "italic")))
PEvSpeed

dev.copy2pdf(file="PEvSpeed.pdf", width=26, height=16)

PE_plot <- ggarrange(PEvSpeed, PEvTL, 
                           ncol = 2, nrow = 2,  
                           labels = c("A", "B"),
                           common.legend = TRUE)
PE_plot

dev.copy2pdf(file="PE_Plot.pdf", width=18, height=18)
