#Install Packages
list.of.packages <- c("ggplot2", "ggthemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## theme

# https://material.io/design/color/the-color-system.html#tools-for-picking-colors
pal_heart <- c("#FF8A80", "#D50000", "#FF1744",  "#FF5252", "#B71C1C","#7f0000")
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

# Create fake data to recreate graph (http://www.thederrick.com/news/features/experts-heart-healthy-diet-as-effective-as-statins/article_aa659f95-386b-5249-b7ae-1b06ad201b32.html)
diet_data <- data.frame(replacement = c("Polyunsaturated Fats",
                                        "Monounsaturated Fats",
                                        "Complex Carbs",
                                        "Simple Carbs"),
                        reduction = c(50, 30, 18, 0))

# Simple bar graph
b <- ggplot(diet_data, aes(x = reorder(replacement, -reduction), y = reduction)) + 
  xlab("Food Replacement") +
  ylab("Percent Reduction") +
  geom_bar(stat = "identity", fill = c("#FF8A80", 
                                       "#D50000", 
                                       "#FF1744",
                                       "#B71C1C"))
b + scale_color_discrete_heart() + 
  theme_heart() + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  ggtitle("Reduced Probability of Developing Heart Disease by \n Substituting Saturated Fats for Other Food Types") 


