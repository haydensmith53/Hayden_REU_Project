#######
#Rain Cloud Plots 
######
### This script creates an R function to generate raincloud plots, then simulates
### data for plots. If using for your own data, you only need lines 1-80.
### It relies largely on code previously written by David Robinson
### (https://gist.github.com/dgrtwo/eb7750e74997891d7c20)
### and the package ggplot2 by Hadley Wickham

# Check if required packages are installed ----
packages <- c("cowplot", "readr", "ggplot2", "dplyr", "lavaan", "smooth", "Hmisc")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

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
            Species = first(Species),
            Length = first(Length))

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



          annotate_figure('Normal Swimming'))




# aes(x=Species,y=`Thrust Power`, color = Species, fill = Species)) +
#   geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2) +
#   geom_point(position = position_jitter(width = .15), size = .25) +
#   scale_color_manual(values = pal) + 
#   scale_fill_manual(values = pal) +
#   ylab('Thrust power output (Watts)')+ xlab('Species') +
#   coord_flip()+ theme(legend.position = "none") + theme_classic()


d_max_swimming <- read_csv("Droned Max Effort Tailbeats.csv")

# Defining the geom_flat_violin function ----
# Note: the below code modifies the
# existing github page by removing a parenthesis in line 50

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(
                ymin = min(y),
                ymax = max(y),
                xmin = x,
                xmax = x + width / 2
              )
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x)
            )
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(
              plyr::arrange(transform(data, x = xminv), y),
              plyr::arrange(transform(data, x = xmaxv), -y)
            )
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1, ])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(
            weight = 1, colour = "grey20", fill = "white", size = 0.5,
            alpha = NA, linetype = "solid"
          ),
          
          required_aes = c("x", "y")
  )


ggarrange(mean_score_hist,                                        # First row with scatter plot
          ggarrange(dens_by_region, dens_by_year, dens_by_GT, 
                    ncol = 3, labels = c("B", "C", "D")), # Second row with box and dot plots
          nrow = 2, labels = "A" ) 

fct_reorder(GearType_general, mean_criteria, .desc = TRUE)


pal <- c("Minke" = "firebrick3",  "Humpback" = "gray30", "Fin" = "chocolate3", "Blue" = "dodgerblue2")

# Testing out a plot for normal swimming

# Thrust
p_raincloud_thrust <- ggplot(data = d_reg_swimming,
                             aes(x=Species,y=`Thrust Power`, color = Species, fill = Species)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2) +
  geom_point(position = position_jitter(width = .15), size = .25) +
  scale_color_manual(values = pal) + 
  scale_fill_manual(values = pal) +
  ylab('Thrust power output (Watts)')+ xlab('Species') +
  coord_flip()+ theme(legend.position = "none") + theme_classic()
#  ggtitle('Thrust Power Output for Each Flukbeat During Normal Swimming')
p_raincloud_thrust

# Drag
p_raincloud_drag <- ggplot(data = d_reg_swimming,aes(x=Species,y=`Drag Coefficient`, fill = Species))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
  geom_point(position = position_jitter(width = .15), size = .25) +
  
  ylab('Score')+xlab('Group')+coord_flip()+theme_classic()+guides(fill = FALSE)+
  ggtitle('Drag Coefficient for Each Flukbeat During Normal Swimming')
p_raincloud_drag

# Reynolds Number
p_raincloud_Re <- ggplot(data = d_reg_swimming,aes(x=Species,y=`Reynolds Number`, fill = Species))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
  geom_point(position = position_jitter(width = .15), size = .25) +
  
  ylab('Score')+xlab('Group')+coord_flip()+theme_classic()+guides(fill = FALSE)+
  ggtitle('Reynolds Number While Fluking During Normal Swimming')
p_raincloud_Re

# Efficiency
p_raincloud_E <- ggplot(data = d_reg_swimming,aes(x=Species,y=`Efficiency`, fill = Species))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
  geom_point(position = position_jitter(width = .15), size = .25) +
  
  ylab('Score')+xlab('Group')+coord_flip()+theme_classic()+guides(fill = FALSE)+
  ggtitle('Efficiency for Each Flukbeat During Normal Swimming')
p_raincloud_E



## Testing out a plot for max effort swimming

# Thrust
p_raincloud_maxthrust <- ggplot(data = d_max_swimming,aes(x=Species,y=`Thrust Power`, fill = Species))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
  geom_point(position = position_jitter(width = .15), size = .25) +
  
  ylab('Score')+xlab('Group')+coord_flip()+theme_classic()+guides(fill = FALSE)+
  ggtitle('Thrust Power Output for Each Flukbeat During Max Effort Swimming')
p_raincloud_maxthrust

# Drag
p_raincloud_maxdrag <- ggplot(data = d_max_swimming,aes(x=Species,y=`Drag Coefficient`, fill = Species))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
  geom_point(position = position_jitter(width = .15), size = .25) +
  
  ylab('Score')+xlab('Group')+coord_flip()+theme_classic()+guides(fill = FALSE)+
  ggtitle('Drag Coefficient for Each Flukbeat During Max Effort Swimming')
p_raincloud_maxdrag

# Reynolds Number
p_raincloud_maxRe <- ggplot(data = d_max_swimming,aes(x=Species,y=`Reynolds Number`, fill = Species))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
  geom_point(position = position_jitter(width = .15), size = .25) +
  
  ylab('Score')+xlab('Group')+coord_flip()+theme_classic()+guides(fill = FALSE)+
  ggtitle('Reynolds Number While Fluking During Max Effort Swimming')
p_raincloud_maxRe

# Efficiency
p_raincloud_maxE <- ggplot(data = d_max_swimming,aes(x=Species,y=`Efficiency`, fill = Species))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
  geom_point(position = position_jitter(width = .15), size = .25) +
  
  ylab('Score')+xlab('Group')+coord_flip()+theme_classic()+guides(fill = FALSE)+
  ggtitle('Efficiency for Each Flukbeat')
p_raincloud_maxE


