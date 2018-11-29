#Install Packages if they don't exist, otherwise load
list.of.packages <- c("ggplot2", "ggthemes","tidyverse","extrafont")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# A) FONTS ====
font_import()
#fonts()
# This will show more detailed information about fonts
#fonttable()
# ++++++++++++++++++++++++++++


# 1. Colors ####

pal_heart <- c("#ff0000", "#7f0000", "#ff6666",  "#ffb2b2", "#E9967A","#A52A2A")
blue_heart <- "#3F69AA"

scale_fill_heart <- function(){
  structure(list(
    scale_fill_manual(values = pal_heart)
  ))
}

scale_color_discrete_heart <- function(){
  structure(list(
    scale_color_manual(values = pal_heart)
  ))
}

scale_color_continuous_heart <- function(){
  structure(list(
    scale_color_gradientn(colours = pal_heart)
  ))
}
# 2. Themes #####

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

# 3. Data Manipulation ####
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")
cause_data = read.csv("slide3_leading_causes_death_us_2016.csv", header = TRUE,
                      stringsAsFactors = FALSE)

#cause_80_16_data = read.csv("Cause_80_16.csv", header = TRUE,
#                           stringsAsFactors = FALSE)

names(cause_data) = c("causes", "deaths")
cause_data.1 = cause_data[1:10, ]
cause_data.1$causes = factor(cause_data.1$causes)
levels(cause_data.1$causes)
# [1] "Accidents (unintentional injuries)"          "Alzheimer`s disease"                         "Cerebrovascular diseases"                   
# [4] "Chronic lower respiratory diseases"          "Diabetes mellitus"                           "Heart disease"                              
# [7] "Influenza and pneumonia"                     "Intentional self-harm (suicide)"             "Malignant neoplasms"                        
# [10] "Nephritis, nephrotic syndrome and nephrosis"

# cause_data.2$causes = factor(cause_data.1$causes, 
#                       levels = cause_data.1$causes[order(cause_data.1$deaths)])
# [1] "Nephritis, nephrotic syndrome and nephrosis" "Influenza and pneumonia"                     "Intentional self-harm (suicide)"            
# [4] "Diabetes mellitus"                           "Alzheimer`s disease"                         "Cerebrovascular diseases"                   
# [7] "Chronic lower respiratory diseases"          "Accidents (unintentional injuries)"          "Malignant neoplasms"                        
# [10] "Heart disease"
# THE ABOVE SORTED FROM LOWEST TO HIGHEST, I WANT TO GO FROM HIGHEST TO LOWEST

cause_data.1$causes = factor(cause_data.1$causes, 
                             levels = cause_data.1[order(cause_data.1$deaths, decreasing = TRUE), "causes"])
# levels(cause_data.2$causes)
# [1] "Heart disease"                               "Malignant neoplasms"                         "Accidents (unintentional injuries)"         
# [4] "Chronic lower respiratory diseases"          "Cerebrovascular diseases"                    "Alzheimer`s disease"                        
# [7] "Diabetes mellitus"                           "Influenza and pneumonia"                     "Intentional self-harm (suicide)"            
# [10] "Nephritis, nephrotic syndrome and nephrosis"

#I arrnged the factors in the order I want, but then
# coord_flip() does away with that by reversing the order
# , so I have to add the scale_x_discrete
# to flip back to the order I want
#

#Function to make the title wrap since it's so long
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_3_heart_disease_prevalence.png",width = 1000, height = 1000) 
ggplot_object = ggplot(cause_data.1, aes(x = causes,y = deaths)) +
  geom_bar(stat = "identity",fill = pal_heart[1] ) +
  coord_flip() +  
  scale_x_discrete(limits = rev(levels(cause_data.1$causes))) +
  theme_heart() +
  ggtitle(wrapper("Rates of the 10 leading causes of death in the United States in 2016 (per 100,000 population)",width=55)) +
  ylab("Deaths per 100,000") + 
  xlab("Cause of Death") +
  theme(legend.position="none")

print(ggplot_object)
dev.off()

# 4. CREATE PDF PLOT ======
# pdf("slide3.pdf", height = 5, width = 5)
# graphics.off()