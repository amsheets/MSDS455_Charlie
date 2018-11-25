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
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")

data <- read.table(file="slide6_lifestyle-habits-of-us-adults-diagnosed-with-cardiovascular-conditions-2018-by-age.csv",sep=",",stringsAsFactors=FALSE,header=TRUE,quote="'")

plot_data <- reshape(data,
                         varying = c("total", "age_18_34" , "age_35_44" , "age_45_54" , "age_55_64" , "age_65_plus"),
                         v.names = "Percent",
                         idvar = "factor",
                         times = c("total", "age_18_34" , "age_35_44" , "age_45_54" , "age_55_64" , "age_65_plus"),
                         direction="long")

rownames(plot_data) <- NULL

plot_data$time <- factor(plot_data$time , labels=c('18-34 year olds','35-44 year olds','45-54 year olds','55-64 year olds','65+ year olds','Overall'))

##Bar graph to illustrate mean rating by grad year/program

#Function to make the title wrap since it's so long
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_6_causes_of_HD.png",width = 1000, height = 1000))  
ggplot_object <- ggplot(data=plot_data,
                        aes(x=factor, y=Percent)) +
  geom_bar(aes(fill = time), position = "dodge", stat="identity") +
  theme_heart() +
  scale_fill_heart() +
  ggtitle(wrapper("Percentage of U.S. adults with cardiovascular conditions that had select lifestyle habits as of 2018, by age",width=55)) +
  xlab("Lifestyle Habit") + 
  ylab("Percentage of Adult Population") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(ggplot_object)
dev.off()

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")
data <- read.table(file="slide7_Prevention_w_Exercise.csv",sep=",",stringsAsFactors=FALSE,header=TRUE,quote="'")

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_7_prevention_w_exercise.png",width = 1000, height = 1000))  
ggplot_object <- ggplot(data=data,
                        aes(x=Gender, y=CHD_Reduction_leisure)) +
  geom_bar(aes(fill = Gender), position = "dodge", stat="identity") +
  theme_heart() +
  scale_fill_heart() +
  ggtitle(wrapper("Percentage Reduction in Coronary Heart Disease Events Due to Leisure Time Physical Activity",width=55)) +
  xlab("Gender") + 
  ylab("Percent Reduction") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position="none")
print(ggplot_object)
dev.off()
