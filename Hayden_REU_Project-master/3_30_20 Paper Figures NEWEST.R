#### Install necessary packages ####
install.packages("lme4")
install.packages("lmerTest")
install.packages("MuMin")

#### Load packages and data ####
library(cowplot)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)

# Custom functions ----

# Standard error function
SE = function(x){sd(x)/sqrt(sum(!is.na(x)))}

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
pal2 <- c("Human" = "#59A14F", "Fish" = "E15759", "Pinniped" = "#79706E", "Sirenian" = "#B6992D", "Odontocete" = "#B07AA1", "Mysticete" = "#4E79A7")

#### MST ~ U ####
# (kinematics and morphology) 
# (Just normal, no max effort)
normal_effort <- d_combine_swimming_summarized %>% 
  filter(effort_type == "Normal",
         Species != "Balaenoptera physalus") %>% 
  mutate(sp_abbr = abbr_binom(Species))
fig3 <- ggplot(normal_effort, aes(mean_speed, mean_TPM)) +
  geom_point(aes(color = sp_abbr), size = 10) +
  geom_smooth(method = "lm", color = "black", size = 3) +
  scale_color_manual(values = pal) +
  expand_limits(y = 0) +
  labs(x = bquote('Swim Speed'~(m~s^-1)),
       y = bquote('Mass-Specific Thrust'~(N~kg^-1))) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 40),
        axis.title = element_text(size = 48),
        legend.position = "none",
        panel.grid.minor = element_blank())
ggsave("figs/fig3.pdf", height = 480, width = 480, units = "mm", dpi = 300)
fig3

# Stats

# basic linear regression
statsfig3 <- lm(log(mean_TPM) ~ mean_speed,    # CHECK OUT HOW ESTIMATE AND INTERCEPT CHANGES WHEN YOU LOG (OR NOT) THE RESPONSE (Y) VARIABLE
                 data = filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"))
summary(statsfig3)

# Generalized linear mixed model
GLMMfig3_mean <- lmer(log(mean_TPM) ~ mean_speed + (1|Species), 
                       data = filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"))
summary(GLMMfig3_mean)
r.squaredGLMM(GLMMfig3_mean)


#### MST ~ L ####
# (kinematics and morphology) 
# (Normal and Max)
combined_effort <- d_combine_swimming_summarized %>% 
  filter(Species != "Balaenoptera physalus") %>% 
  mutate(sp_abbr = abbr_binom(Species))

fig4 <- combined_effort %>% 
  filter(mean_TPM < 1.6) %>% 
  ggplot(aes(`Total Length (m)`, mean_TPM, color = sp_abbr)) +
  geom_point(aes(shape = effort_type), size = 10) +
  geom_smooth(method = "lm", aes(color = effort_type, linetype = effort_type), size = 3) +
  scale_color_manual(values = c(`B. bonaerensis` = "#009E73",
                                `M. novaeangliae` = "#D55E00",
                                `B. musculus` = "#0072B2",
                                Normal = "black", Max = "black")) +
  scale_shape_manual(values = c(Normal = 16, Max = 10)) +
  scale_linetype_manual(values = c(Normal = "solid", Max = "longdash")) +
  labs(x = bquote('Total Length (m)'),
       y = bquote('Mass-Specific Thrust'~(N~kg^-1))) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 40),
        axis.title = element_text(size = 48),
        legend.position = "none",
        panel.grid.minor = element_blank())
ggsave("figs/fig4.pdf", height = 480, width = 480, units = "mm", dpi = 300)
fig4

# Stats
# basic linear regression
stats4max <- lm(mean_TPM ~ `Total Length (m)`, 
                data = filter(d_combine_swimming_summarized, 
                              effort_type == "Max", Species != "Balaenoptera physalus"))
summary(stats4max)

# Generalized linear mixed model
GLMM4Lmax_mean <- lmer(log(mean_TPM) ~ `Total Length (m)` + (1|Species), 
                      data = filter(d_combine_swimming_summarized, 
                                    effort_type == "Max", Species != "Balaenoptera physalus"))
summary(GLMM4max_mean)
r.squaredGLMM(GLMM4max_mean)

# Stats
# basic linear regression
stats4normal <- lm(mean_TPM ~ `Total Length (m)`, 
                   data = filter(d_combine_swimming_summarized, 
                                 effort_type == "Max", Species != "Balaenoptera physalus"))
summary(stats4normal)

# Generalized linear mixed model
GLMM4normal_mean <- lmer(log(mean_TPM) ~ `Total Length (m)` + (1|Species), 
                         data = filter(d_combine_swimming_summarized, 
                                       effort_type == "Normal", Species != "Balaenoptera physalus"))
summary(GLMM4normal_mean)
r.squaredGLMM(GLMM4normal_mean)

## TODO fit LME, get prediction intervals, change text and point sizes

#### MST ~ AL ####
# (kinematics and morphology) 
# (Normal and Max)
fig5 <- ggplot(normal_effort, aes(`FA/L`, mean_TPM)) +
  geom_point(aes(color = sp_abbr), size = 10) +
  geom_smooth(method = "lm", color = "black", size = 3) +
  scale_color_manual(values = pal) +
  labs(x = "Fluke Area / Total Length",
       y = bquote('Mass-Specific Thrust'~(N~kg^-1))) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 40),
        axis.title = element_text(size = 48),
        legend.position = "none",
        panel.grid.minor = element_blank())
ggsave("figs/fig5.pdf", height = 480, width = 480, units = "mm", dpi = 300)
fig5

# Stats
# basic linear regression
statsfig5 <- lm(mean_TPM ~ `FA/L`, 
                  data = filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"))
summary(statsfig5)

# Generalized linear mixed model
GLMMfig5_mean <- lmer(log(mean_TPM) ~ `FA/L` + (1|Species), 
                        data = filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"))
summary(GLMMfig5_mean)
r.squaredGLMM(GLMMfig5_mean)


## TODO change text and point sizes, fit linear mixed effects model, get prediction intervals from lme


#### Hoerner Model Comparison ####
fig6 <- normal_effort %>% 
  ggplot() +
  geom_point(aes(x = mean_Re, y = mean_drag, color = sp_abbr), size = 10) +
  geom_smooth(method = "lm", aes(x = `Reynolds number`, y = `drag coefficient C_D`), color = "black", linetype = 2, size = 3) +
  geom_smooth(method = "lm", aes(x = mean_Re, y = mean_drag), color = "black", size = 3) +
  scale_color_manual(values = c(`B. bonaerensis` = "#009E73",
                                `M. novaeangliae` = "#D55E00",
                                `B. musculus` = "#0072B2",
                                Normal = "black", Max = "black")) +
  scale_shape_manual(values = c(Normal = 16, Max = 1)) +
  scale_linetype_manual(values = c(Normal = "solid", Max = "longdash")) +
  ylim(0, 0.05) +
  labs(x = bquote('Reynolds Number'),
       y = bquote('Drag Coefficient')) +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 40),
        axis.title = element_text(size = 48),
        legend.position = "none",
        panel.grid.minor = element_blank())
ggsave("figs/fig6.pdf", height = 480, width = 480, units = "mm", dpi = 300)
fig6


#### Prop Eff ~ U, L ####
fig7_U <- normal_effort %>% 
  ggplot(aes(mean_speed, mean_E)) +
  geom_point(aes(color = sp_abbr), size = 10) +
  geom_smooth(method = "lm", color = "black", size = 3) +
  scale_color_manual(values = pal) +
  labs(x = bquote('Speed'~(m~s^-1)),
       y = "Propulsive Efficiency") +
  expand_limits(y = c(0.75, 1)) +
  theme_classic() +
  theme(axis.text = element_text(size = 40),
        axis.title = element_text(size = 48),
        legend.position = "none",
        panel.grid.minor = element_blank())
fig7_U

# Stats
# basic linear regression
statsfig7U <- lm(mean_E ~ mean_speed, 
                 data = filter(normal_effort, Species != "Balaenoptera physalus"))
summary(statsfig7U)

# Generalized linear mixed model
GLMM4fig7U_mean <- lmer(mean_E ~ mean_speed + (1|Species), 
                        data = filter(normal_effort, Species != "Balaenoptera physalus"))
summary(GLMM4fig7U_mean)
r.squaredGLMM(GLMM4fig7U_mean)


fig7_L <- normal_effort %>% 
  ggplot(aes(`Total Length (m)`, mean_E)) +
  geom_point(aes(color = sp_abbr), size = 10) +
  geom_smooth(method = "lm", color = "black", size = 3) +
  scale_color_manual(values = pal) +
  expand_limits(y = c(0.75, 1)) +
  theme_classic() +
  theme(axis.text = element_text(size = 40),
        axis.title = element_text(size = 48),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank())
fig7_L

# Stats
# basic linear regression
statsfig7L <- lm(mean_E ~ `Total Length (m)`, 
                 data = filter(normal_effort, Species != "Balaenoptera physalus"))
summary(statsfig7L)


# Generalized linear mixed model
GLMM4fig7L_mean <- lmer(mean_E ~ `Total Length (m)` + (1|Species), 
                        data = filter(normal_effort, Species != "Balaenoptera physalus"))
summary(GLMM4fig7L_mean)
r.squaredGLMM(GLMM4fig7L_mean)


fig7 <- plot_grid(fig7_U, fig7_L,
                  nrow = 1,
                  align = "h",
                  labels = NULL)
ggsave("figs/fig7.pdf", height = 480, width = 960, units = "mm", dpi = 300)
fig7

## TODO LME, play with sizes/dimensions



#### Prop Eff ~ L w/ Other Species ####
fish_prop_eff <- read_csv("Propulsive Eff All Species.csv")
fig8 <- ggplot(fish_prop_eff, aes(`Total Length (m)`, `Prop Eff (Max)`)) +
  geom_point(aes(color = Group, shape = `Type of Swimming`), size = 10) +
  expand_limits(y = c(0, 1)) +
  labs(x = "Total Length (m)",
       y = "Propulsive Efficiency") +
  theme_classic() +
  theme(axis.text = element_text(size = 40),
        axis.title = element_text(size = 48),
        legend.position = "none",
        panel.grid.minor = element_blank())
ggsave("figs/fig8.pdf", height = 480, width = 480, units = "mm", dpi = 300)
fig8



#### Extra/Test Figures (Not Used In Paper) ####

#### Extra - MST ~ A ####
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
                 data = filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"))
summary(statsfig3A)

# Generalized linear mixed model
GLMMfig3A_mean <- lmer(log(mean_TPM) ~ `Fluke Area (m)` + (1|Species), 
                       data = filter(d_combine_swimming_summarized, Species != "Balaenoptera physalus"))
summary(GLMMfig3A_mean)
r.squaredGLMM(GLMMfig3A_mean)

#### Extra - Drag ~ L w/ CFD ####
potvin_cfd <- read_csv("potvin_cfd.csv")
fig6 <- ggplot(normal_effort, aes(`Total Length (m)`, mean_drag)) +
  geom_point(aes(color = sp_abbr)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(aes(color = sp_abbr), potvin_cfd, size = 2, shape = 15) +
  geom_smooth(method = "lm", color = "black", data = potvin_cfd, linetype = 2) +
  scale_color_manual(values = pal) +
  labs(y = "Drag Coefficient") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
ggsave("figs/fig6.pdf", height = 90, width = 90, units = "mm", dpi = 300)
fig6

# Stats
# basic linear regression
stats6Emp <- lm(mean_drag ~ `Total Length (m)`, 
                data = filter(normal_effort, Species != "Balaenoptera physalus"))
summary(stats6Emp)

# Generalized linear mixed model
GLMM4fig6Emp_mean <- lmer(log(mean_drag) ~ `Total Length (m)` + (1|Species), 
                          data = filter(normal_effort, Species != "Balaenoptera physalus"))
summary(GLMM4fig6Emp_mean)
r.squaredGLMM(GLMM4fig6Emp_mean)

# Stats
# basic linear regression
stats6Jean <- lm(mean_drag ~ `Total Length (m)`, 
                 data = potvin_cfd)
summary(stats6Jean)

# Generalized linear mixed model
GLMM4fig6Jean_mean <- lmer(log(mean_drag) ~ `Total Length (m)` + (1|sp_abbr), 
                           data = potvin_cfd)
summary(GLMM4fig6Jean_mean)
r.squaredGLMM(GLMM4fig6Jean_mean)

## TODO y'know


#### Extra - Prop Eff ~ U (Smoothed Lines + Fish 1998 Data) ####
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

#### Extra - Prop Eff ~ RE (Smoothed Lines + Fish 1998 Data) ####
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

#### Extra - Max Prop Eff ~ Max RE ####
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