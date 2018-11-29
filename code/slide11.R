#Install Packages
list.of.packages <- c("ggplot2", "ggthemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## theme

# https://material.io/design/color/the-color-system.html#tools-for-picking-colors
pal_heart <- c("#ff0000", "#7f0000", "#ff6666",  "#ffb2b2", "#E9967A","#A52A2A")
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

#Use stringr to modify titles 
library(stringr)
# Set working directory
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")
data <- read.table(file="mortality_reduction.csv",sep=",",stringsAsFactors=FALSE,header=TRUE,quote="'")

# Simple bar graph
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_11_mortality_reduction.png",width = 1000, height = 1000) 
ggplot_object <- ggplot(data=data,
                        aes(x=Behavior, y=Mortality)) +
  geom_bar(aes(fill = Behavior), position = "dodge", stat="identity") +
  theme_heart() +
  scale_fill_heart() +
  ggtitle("Reduced Mortality by Lifestyle Behavior Modification") +
  xlab("Behavior Modification") + 
  ylab("Percent Reduction in Mortality") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position="none")
print(ggplot_object)
dev.off()
