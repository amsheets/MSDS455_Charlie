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

# set working directory
setwd("C:/Users/ljvan/OneDrive/Documents/MSPA/MSDS 455/Heart Disease Group Project/")

data <- read.table(file="Heart Disease Deaths by Race and Gender.csv",sep=",",stringsAsFactors=FALSE,header=TRUE,quote="'")

plot_data <- reshape(data,
                     varying = c("All", "Female" , "Male"),
                     v.names = "Percent",
                     idvar = "Race",
                     times = c("All", "Female" , "Male"),
                     direction="long")


#plot_data <- reshape(data,
#                     varying = c("total", "age_18_34" , "age_35_44" , "age_45_54" , "age_55_64" , "age_65_plus"),
#                     v.names = "Percent",
#                     idvar = "factor",
#                     times = c("total", "age_18_34" , "age_35_44" , "age_45_54" , "age_55_64" , "age_65_plus"),
#                     direction="long")

rownames(plot_data) <- NULL

plot_data$time <- factor(plot_data$time , labels=c('All','Female','Male'))


#Function to make the title wrap since it's so long
#wrapper <- function(x, ...) 
#{
#  paste(strwrap(x, ...), collapse = "\n")
#}

png(file = "slide_4_HD Deaths by Race and Gender.png",width = 1000, height = 1000)
ggplot_object <- ggplot(data=plot_data,
                        aes(x=Race, y=Percent)) +
  geom_bar(aes(fill = time), position = "dodge", stat="identity") +
  theme_heart() +
  scale_fill_heart() +
  ggtitle("% of Heart Disease Deaths by Race and Gender as of 2008") +  
  xlab("Race") + 
  ylab("Percentage of Heart Disease Deaths") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(ggplot_object)
dev.off()