#### Load packages and data ####
library(cowplot)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)

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
         TPM = `Thrust Power`/Mass,
         Speed_BL = Speed/Length)

# Separating normal fluekbeats from all data ----
d_reg_swimming <- d_all_swimming %>%  
  anti_join(d_max_swimming, by = colnames(d_all_swimming)[c(1:4, 7:8, 15)]) %>% 
  mutate(effort_type = "All") %>%
  left_join(Mass_SKR, by = "Species") %>% 
  mutate(Mass = (Length^slope)*10^intercept, 
         TPM = `Thrust Power`/Mass,
         Speed_BL = Speed/Length) %>% 
  left_join(select(morphometrics, -`Common name`), by = c("Species", "Individual", "ID")) 

# Combining normal and max data, this is the data file that we will use 
d_combine_swimming <- bind_rows(d_reg_swimming, d_max_swimming)

# Summarizing d_max_swimming ----
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


#### Graphs Start Here ####


#### Color Palette ####
pal <- c("B. bonaerensis" = "#009E73",  "M. novaeangliae" = "#D55E00",  "B. musculus" = "#0072B2")

#### MST ~ U, A, L, A/L ####
# (kinematics and morphology) 
# (Just normal, no max effort)
normal_effort <- d_combine_swimming_summarized %>% 
  filter(effort_type == "Normal",
         Species != "Balaenoptera physalus") %>% 
  mutate(sp_abbr = abbr_binom(Species))
fig3_U <- ggplot(normal_effort, aes(mean_speed, mean_TPM)) +
  geom_point(aes(color = sp_abbr), size = 0.5) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = pal) +
  expand_limits(y = 0) +
  labs(x = bquote('Speed'~(m~s^-1)),
       y = bquote('Mass-Specific Thrust'~(N~kg^-1))) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
fig3_U

# Stats
# basic linear regression
statsfig3U <- lm(mean_TPM ~ mean_speed, 
              data = d_combine_swimming_summarized)
summary(statsfig3U)

# Generalized linear mixed model
GLMMfig3U_mean <- lmer(log(mean_TPM) ~ mean_speed + (1|Species), 
                      data = d_combine_swimming_summarized)
summary(GLMMfig3U_mean)
r.squaredGLMM(GLMMfig3U_mean)


# this model takes forever to run so I ditched it for now
# GLMMfig3U_raw <- lmer(log(TPM) ~ Speed + (ID|`Common name`), 
#                   data = d_reg_swimming)
# summary(GLMMfig3U_raw)


fig3_A <- ggplot(normal_effort, aes(`Fluke Area (m)`, mean_TPM)) +
  geom_point(aes(color = sp_abbr), size = 0.5) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = pal) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 8) +
  theme(axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank())
fig3_A

# Stats
# basic linear regression
statsfig3A <- lm(mean_TPM ~ `Fluke Area (m)`, 
                 data = d_combine_swimming_summarized)
summary(statsfig3A)

# Generalized linear mixed model
GLMMfig3A_mean <- lmer(log(mean_TPM) ~ `Fluke Area (m)` + (1|Species), 
                       data = d_combine_swimming_summarized)
summary(GLMMfig3A_mean)
r.squaredGLMM(GLMMfig3A_mean)


fig3_L <- ggplot(normal_effort, aes(`Total Length (m)`, mean_TPM)) +
  geom_point(aes(color = sp_abbr), size = 0.5) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = pal) +
  expand_limits(y = 0) +
  labs(y = bquote('Mass-Specific Thrust'~(N~kg^-1))) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
fig3_L

# Stats
# basic linear regression
statsfig3L <- lm(mean_TPM ~ `Total Length (m)`, 
                 data = d_combine_swimming_summarized)
summary(statsfig3L)

# Generalized linear mixed model
GLMMfig3L_mean <- lmer(log(mean_TPM) ~ `Total Length (m)` + (1|Species), 
                       data = d_combine_swimming_summarized)
summary(GLMMfig3L_mean)
r.squaredGLMM(GLMMfig3L_mean)


fig3_AL <- ggplot(normal_effort, aes(`FA/L`, mean_TPM)) +
  geom_point(aes(color = sp_abbr), size = 0.5) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = pal) +
  expand_limits(y = 0) +
  labs(x = "Fluke Area / Length (m)") +
  theme_minimal(base_size = 8) +
  theme(axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank())
fig3_AL

# Stats
# basic linear regression
statsfig3AL <- lm(mean_TPM ~ `FA/L`, 
                 data = d_combine_swimming_summarized)
summary(statsfig3AL)

# Generalized linear mixed model
GLMMfig3AL_mean <- lmer(log(mean_TPM) ~ `FA/L` + (1|Species), 
                       data = d_combine_swimming_summarized)
summary(GLMMfig3AL_mean)
r.squaredGLMM(GLMMfig3AL_mean)


# Combine into one figure
fig3 <- plot_grid(fig3_U, fig3_A, fig3_L, fig3_AL,
                  nrow = 2,
                  align = "h",
                  rel_widths = c(1.1, 0.9),
                  labels = NULL)
fig3
ggsave("figs/fig3.pdf", height = 90, width = 90, units = "mm", dpi = 300)

## TODO change text and point sizes, fit linear mixed effects model, get prediction intervals from lme

#### MST ~ L, max vs normal ####
combined_effort <- d_combine_swimming_summarized %>% 
  filter(Species != "Balaenoptera physalus") %>% 
  mutate(sp_abbr = abbr_binom(Species))
# Remove outliers from figure
fig4 <- combined_effort %>% 
  filter(mean_TPM < 1.6) %>% 
  ggplot(aes(`Total Length (m)`, mean_TPM, color = sp_abbr)) +
  geom_point(aes(shape = effort_type)) +
  geom_smooth(method = "lm", aes(color = effort_type, linetype = effort_type)) +
  scale_color_manual(values = c(`B. bonaerensis` = "#009E73",
                                `M. novaeangliae` = "#D55E00",
                                `B. musculus` = "#0072B2",
                                Normal = "black", Max = "black")) +
  scale_shape_manual(values = c(Normal = 16, Max = 1)) +
  scale_linetype_manual(values = c(Normal = "solid", Max = "longdash")) +
  labs(x = bquote('Total Length (m)'),
       y = bquote('Mass-Specific Thrust'~(N~kg^-1))) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
ggsave("figs/fig4.pdf", height = 90, width = 90, units = "mm", dpi = 300)
fig4

# Stats
# basic linear regression
stats4max <- lm(mean_TPM ~ `Total Length (m)`, 
              data = filter(d_combine_swimming_summarized, effort_type == "Max"))
summary(stats4max)


# Generalized linear mixed model
GLMM4max_mean <- lmer(log(mean_TPM) ~ `Total Length (m)` + (1|Species), 
                      data = filter(d_combine_swimming_summarized, effort_type == "Max"))
summary(GLMM4max_mean)
r.squaredGLMM(GLMM4max_mean)


# Stats
# basic linear regression
stats4normal <- lm(mean_TPM ~ `Total Length (m)`, 
                data = filter(d_combine_swimming_summarized, effort_type == "Normal"))
summary(stats4normal)


# Generalized linear mixed model
GLMM4normal_mean <- lmer(log(mean_TPM) ~ `Total Length (m)` + (1|Species), 
                      data = filter(d_combine_swimming_summarized, effort_type == "Normal"))
summary(GLMM4normal_mean)
r.squaredGLMM(GLMM4normal_mean)

## TODO fit LME, get prediction intervals, change text and point sizes

#### Prop Eff ~ U, L ####
fig5_U <- normal_effort %>% 
  ggplot(aes(mean_speed, mean_E)) +
  geom_point(aes(color = sp_abbr)) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = pal) +
  labs(x = bquote('Speed'~(m~s^-1)),
       y = "Propulsive Efficiency") +
  expand_limits(y = c(0.75, 1)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
fig5_U

# Stats
# basic linear regression
statsfig5U <- lm(mean_E ~ mean_speed, 
                  data = normal_effort)
summary(statsfig5U)

# Generalized linear mixed model
GLMM4fig5U_mean <- lmer(mean_E ~ mean_speed + (1|Species), 
                         data = normal_effort)
summary(GLMM4fig5U_mean)
r.squaredGLMM(GLMM4fig5U_mean)


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
fig5_L

# Stats
# basic linear regression
statsfig5L <- lm(mean_E ~ `Total Length (m)`, 
                 data = normal_effort)
summary(statsfig5L)


# Generalized linear mixed model
GLMM4fig5L_mean <- lmer(mean_E ~ `Total Length (m)` + (1|Species), 
                        data = normal_effort)
summary(GLMM4fig5L_mean)
r.squaredGLMM(GLMM4fig5L_mean)


fig5 <- plot_grid(fig5_U, fig5_L,
                  nrow = 1,
                  align = "h",
                  labels = NULL)
ggsave("figs/fig5.pdf", height = 90, width = 180, units = "mm", dpi = 300)
fig5
## TODO LME, play with sizes/dimensions

#### Prop Eff ~ U (Smoothed Lines + Fish 1998 Data) ####
fish_prop_eff <- read_csv("fish_prop_eff.csv")
flukebeats_no_fins <- d_reg_swimming %>% 
  filter(Species != "Balaenoptera physalus")
fig6 <- ggplot(flukebeats_no_fins, aes(Speed, Efficiency)) +
  geom_smooth(aes(color = Species), se = FALSE) +
  geom_smooth(aes(color = Species), fish_prop_eff, se = FALSE, linetype = 2) +
  labs(x = bquote('Speed'~(m~s^-1)),
       y = "Propulsive Efficiency") +
  theme_minimal()
  #theme(legend.position = "none",
   #     panel.grid.minor = element_blank())
fig6
ggsave("figs/fig6.pdf", height = 90, width = 180, units = "mm", dpi = 300)

## TODO Talk to Max and figure out how this could be made better. Figure out what's going on with the blues at high speeds.

#### Drag ~ L w/ CFD ####
potvin_cfd <- read_csv("Hayden_REU_Project-master/potvin_cfd.csv")
fig7 <- ggplot(normal_effort, aes(`Total Length (m)`, mean_drag)) +
  geom_point(aes(color = sp_abbr)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(color = sp_abbr), potvin_cfd, size = 2, shape = 15) +
  geom_smooth(method = "lm", color = "black", data = potvin_cfd, linetype = 2) +
  scale_color_manual(values = pal) +
  labs(y = "Drag Coefficient") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
ggsave("figs/fig7.pdf", height = 90, width = 90, units = "mm", dpi = 300)
fig7

# Stats
# basic linear regression
stats7Emp <- lm(mean_drag ~ `Total Length (m)`, 
              data = normal_effort)
summary(stats7Emp)

# Generalized linear mixed model
GLMM4fig7Emp_mean <- lmer(log(mean_drag) ~ `Total Length (m)` + (1|Species), 
                        data = normal_effort)
summary(GLMM4fig7Emp_mean)
r.squaredGLMM(GLMM4fig7Emp_mean)


# Stats
# basic linear regression
stats7Jean <- lm(mean_drag ~ `Total Length (m)`, 
             data = potvin_cfd)
summary(stats7Jean)

# Generalized linear mixed model
GLMM4fig7Jean_mean <- lmer(log(mean_drag) ~ `Total Length (m)` + (1|sp_abbr), 
                           data = potvin_cfd)
summary(GLMM4fig7Jean_mean)
r.squaredGLMM(GLMM4fig7Jean_mean)

## TODO y'know

#### Prop Eff ~ RE (Smoothed Lines + Fish 1998 Data) ####
fish_prop_eff <- read_csv("fish_prop_eff.csv")
flukebeats_no_fins <- d_reg_swimming %>% 
  filter(Species != "Balaenoptera physalus")
figtest1 <- ggplot(flukebeats_no_fins, aes(`Reynolds Number`, Efficiency)) +
  geom_smooth(aes(color = Species), se = FALSE) +
  geom_smooth(aes(color = Species), fish_prop_eff, se = FALSE, linetype = 2) +
  labs(x = "Reynolds Number",
       y = "Propulsive Efficiency") +
  theme_minimal()
#theme(legend.position = "none",
#     panel.grid.minor = element_blank())
ggsave("figs/figtest1.pdf", height = 90, width = 180, units = "mm", dpi = 300)
figtest1
#### Max Prop Eff ~ Max RE ####
fish_prop_eff_sum <- fish_prop_eff %>%
  group_by(Species) %>%
  summarize(`Max Eff` = max(Efficiency)) 
fish_prop_RE_sum <- fish_prop_eff %>%
  group_by(Species) %>%
  summarize(`Max RE` = max(`Reynolds Number`)) 
fish_prop_sums <- merge(fish_prop_eff_sum, fish_prop_RE_sum) %>%
  rename(Individual = `Species`)
sum_effs <- flukebeats_no_fins %>%
  group_by(Individual) %>%
  summarize(`Max Eff` = max(Efficiency))
sum_REs <- flukebeats_no_fins %>%
  group_by(Individual) %>%
  summarize(`Max RE` = max(`Reynolds Number`)) 
sum_effs_REs <- merge(sum_effs, sum_REs)
figtest2 <- ggplot(sum_effs_REs, aes(`Max RE`, `Max Eff`)) +
  geom_point(aes(color = Individual), size = 2) +
  geom_point(data = fish_prop_sums, size = 3, shape = 1) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Max Reynolds Number",
       y = "Max Propulsive Efficiency") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
figtest2
ggsave("figs/figtest2.pdf", height = 90, width = 90, units = "mm", dpi = 300)
