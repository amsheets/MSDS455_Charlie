#Package imports - If packages are not installed, will install packages.

list.of.packages <- c('usmap','ggplot2')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


## theme

# https://material.io/design/color/the-color-system.html#tools-for-picking-colors
pal_heart <- c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#de2d26','#a50f15')
blue_heart <- "#3F69AA"

############# color pieces!
scale_fill_heart <- function(){
  
  structure(list(
    scale_fill_manual(values=pal_heart)
  ))
}

scale_color_discrete_heart <- function(){
  
  structure(list(
    scale_color_manual(values=pal_heart)
  ))
}

scale_color_continuous_heart <- function(){
  
  structure(list(
    scale_color_gradientn(colours = pal_heart)
  ))
}
##################################

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

# set working directory
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")

death <- read.csv('slide5_HD_death.csv', header = TRUE,stringsAsFactors=FALSE)

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")

png(file = "slide_5_HD_Deaths_by_State.png",width = 1000, height = 1000)
p <- plot_usmap(data = death, values = "value", lines = "black") + 
  scale_fill_continuous(name = "Number per 100,000 population (3-year average)", low="#ffe5e5",high="darkred",label = scales::comma) + 
  theme(legend.position = "right") + labs(title = "Age-adjusted number of deaths due to all cardiovascular diseases by State") +
  theme(plot.title = element_text(color = blue_heart, face = "bold",size=20)) +
  theme(legend.title= element_text(color=blue_heart,size = 16))

print(p)
dev.off()

##Try at the county level
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")
death <- read.csv('slide5_Heart_Disease_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv', header = TRUE,stringsAsFactors=FALSE)
colnames(death)[18] <- "fips"

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")

png(file = "slide_5_HD_Deaths_by_County.png",width = 1000, height = 1000)
p <- plot_usmap(region='counties',data = death, values = "Data_Value", lines = "black") + 
  scale_fill_continuous(name = "Number per 100,000 population (3-year average)", low="#ff9999",high="darkred",label = scales::comma) + 
  theme(legend.position = "right") + labs(title = "Age-adjusted number of deaths due to all cardiovascular diseases by State") +
  theme(plot.title = element_text(color = blue_heart, face = "bold")) +
  theme(legend.title= element_text(color=blue_heart))

print(p)
dev.off()
