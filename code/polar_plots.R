list.of.packages <- c("tidyverse", "xts", "lubridate", "forecast", "Hmisc") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")

load("polar_plots.RData")

theme_heart <- function(base_size = 20, base_family = "Helvetica",
                        base_line_size = base_size / 42,
                        base_rect_size = base_size / 42) {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.ticks      = element_blank(),
      axis.text       = element_text(size = 16,color=blue_heart),
      axis.title = element_text(color = blue_heart),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.title = element_blank(),
      panel.background   = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.title = element_text(color = blue_heart, face = "bold"),
      plot.background   = element_blank(),
      
      complete = TRUE
    )
}

pal_heart = c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#de2d26','#a50f15')
blue_heart <- "#3F69AA"

pal_ts = c('#fee5d9', #part of the current palette
           '#fcbba1', '#fc9272',
           '#FF6666', 
           '#fb6a4a','#de2d26','#a50f15', 
           'darkred',
           '#660000', 
           '#330000')

scale_color_ts_heart <- function(){
  
  structure(list(
    scale_color_manual(values = pal_ts)
  ))
}

# PLOT THE HEART ATTACK TIME SERIES IN POLAR FORM
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "BRFSS_PolarPlot_HeartAttacks.png",width = 1000, height = 1000)
gg_ha = ggseasonplot(ha.ts, polar = TRUE) +
  geom_line(size = 1.25) +
  ggtitle("BRFSS Survey Data 2008 - 2017,\nMississippi Respondents\nwith Heart Attacks") +
  theme_heart() +
  labs(y = "% of Respondents with Heart Attacks", 
       caption = "https://www.cdc.gov/brfss/") +
  scale_color_ts_heart()

print(gg_ha)
dev.off()

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "BRFSS_PolarPlot_AnginaCoronaries.png",width = 1000, height = 1000)
gg_angc = ggseasonplot(angc.ts, polar = TRUE) +
  geom_line(size = 1.25) +
  ggtitle("BRFSS Survey Data 2008 - 2017,\nMississippi Respondents\nwith Angina Coronaries") +
  theme_heart() +
  labs(y = "% of Respondents with Angina Coronaries", 
       caption = "https://www.cdc.gov/brfss/") +
  scale_color_ts_heart()

print(gg_angc)
dev.off()