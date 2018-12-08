##############################
#     THEME & PACKAGES      #
##############################

#Install Packages if they don't exist, otherwise load
list.of.packages <- c("ggplot2", "ggthemes","tidyverse","extrafont", "choroplethrMaps","RColorBrewer", "scales","lubridate","stringr",
                      "tidyverse", "xts", "lubridate", "forecast", "Hmisc") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# FONTS ====
font_import()

# Colors ####

pal_heart <- c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#de2d26','#a50f15')
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
#Themes #####

theme_heart <- function(base_size = 30, base_family = "Helvetica",
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
      axis.text       = element_text(size = 25,color=blue_heart),
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

##############################
#          SLIDE 3           #
##############################

# 3. Data Manipulation ####
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")
cause_data = read.csv("slide3_leading_causes_death_us_2016.csv", header = TRUE,
                      stringsAsFactors = FALSE)

names(cause_data) = c("causes", "deaths")
cause_data.1 = cause_data[1:10, ]
cause_data.1$causes = factor(cause_data.1$causes)
levels(cause_data.1$causes)

cause_data.1$causes = factor(cause_data.1$causes, 
                             levels = cause_data.1[order(cause_data.1$deaths, decreasing = TRUE), "causes"])

#Function to make the title wrap since it's so long
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_3_heart_disease_prevalence.png",width = 1200, height = 1000) 
ggplot_object = ggplot(cause_data.1, aes(x = causes,y = deaths)) +
  geom_bar(color="black",stat = "identity",fill = pal_heart[5] ) +
  coord_flip() +  
  scale_x_discrete(limits = rev(levels(cause_data.1$causes))) +
  theme_heart() +
  ggtitle(wrapper("Rates of the 10 leading causes of death in the United States in 2016 (per 100,000 population)",width=40)) +
  ylab("Deaths per 100,000") + 
  xlab("Cause of Death") +
  theme(legend.position="none")

print(ggplot_object)
dev.off()

##############################
#          SLIDE 4           #
##############################

# set working directory
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")

data <- read.table(file="Heart Disease Deaths by Race and Gender.csv",sep=",",stringsAsFactors=FALSE,header=TRUE,quote="'")

plot_data <- reshape(data,
                     varying = c("All", "Female" , "Male"),
                     v.names = "Percent",
                     idvar = "Race",
                     times = c("All", "Female" , "Male"),
                     direction="long")

rownames(plot_data) <- NULL

plot_data$time <- factor(plot_data$time , labels=c('All','Female','Male'))

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_4_HD_Deaths_by_Race_and_Gender.png",width = 1000, height = 1000)
ggplot_object <- ggplot(data=plot_data,
                        aes(x=Race, y=Percent)) +
  geom_bar(aes(fill = time), color="black",position = "dodge", stat="identity") +
  theme_heart() +
  scale_fill_manual(values=pal_heart[c(2,4,6)]) +
  ggtitle("% of Heart Disease Deaths by Race and Gender as of 2016") +  
  xlab("Race") + 
  ylab("Percentage of Heart Disease Deaths") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(ggplot_object)
dev.off()

##############################
#          SLIDE 5           #
##############################

# Set working directory
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")

#Load in data
heart <- read.csv("slide5_HD_death.csv", header = TRUE) 

#Aggregate means per state
aggr.heart <- aggregate(Data_Value ~ LocationAbbr, data=heart, mean, stringsAsFactors=FALSE)

#Change column names to region and value for choropleth
colnames(aggr.heart)[1] <- "region"
colnames(aggr.heart)[2] <- "value"

#Remove US, puerto rico, american samoa, the mariana islands, the virgin islands, and guam 
aggr.heart <- aggr.heart[-c(49,43,4,28,52,13), ]


####### We need to convert the state abbreviations to state names for choroplethr. This is a function to convert state codes to state names courtesy of https://favorableoutcomes.wordpress.com/2012/10/19/create-an-r-function-to-convert-state-codes-to-full-state-name/

#'x' is the column of a data.frame that holds 2 digit state codes
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AS", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MP", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","american samoa","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia", "guam",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mariana islands","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

#Use the function
aggr.heart$region <- stateFromLower(aggr.heart$region)

#Change aggr.heart$region to character
aggr.heart$region <- as.character(aggr.heart$region)

# Set visualization directory
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_5_HD_by_State.png",width = 1000, height = 500) 

#Create choropleth map
c <- StateChoropleth$new(aggr.heart)

c$add_state_outline = TRUE
#Add title
c$title <- "Age-adjusted number of deaths due to all cardiovasulcar diseases by State"
#Add color options
c$ggplot_scale <- scale_fill_continuous(name="Number per 100,000 population (3-year average)",
                                        low="#fee5d9",
                                        high="#a50f15")
#Use continuous values
c$set_num_colors(1)
#Remove state labels
c$show_labels = FALSE
#Render map and add color for titles
c$render() + theme(plot.title = element_text(color = "#3F69AA",size = 25),
                   legend.title = element_text(color = "#3F69AA",size=20))
dev.off()

##############################
#          SLIDE 8           #
##############################

#Need to add one more color to palette
pal_heart <- c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#de2d26','#D3D3D3')

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

plot_data$time <- factor(plot_data$time , labels=c('18-34','35-44','45-54','55-64','65+','Overall'))


##Bar graph to illustrate mean rating by grad year/program

#Function to make the title wrap since it's so long
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_6_causes_of_HD.png",width = 1000, height = 1000)
ggplot_object <- ggplot(data=plot_data,
                        aes(x=factor, y=Percent)) +
  geom_bar(aes(fill = time),colour="black", position = "dodge", stat="identity") +
  theme_heart() +
  scale_fill_manual(name="Age, in years",values=pal_heart) +
  ggtitle(wrapper("Percentage of U.S. adults with cardiovascular conditions that had select lifestyle habits as of 2018, by age",width=55)) +
  xlab("Lifestyle Habit") + 
  ylab("Percentage of Adult Population") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(ggplot_object)
dev.off()

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")
data <- read.table(file="slide7_Prevention_w_Exercise.csv",sep=",",stringsAsFactors=FALSE,header=TRUE,quote="'")

##############################
#          SLIDE 9           #
##############################

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_7_prevention_w_exercise.png",width = 1000, height = 1000) 
ggplot_object <- ggplot(data=data,
                        aes(x=Gender, y=CHD_Reduction_leisure)) +
  geom_bar(aes(fill = Gender), color='black', position = "dodge", stat="identity") +
  theme_heart() +
  scale_fill_manual(values=pal_heart[c(2,6)]) +
  ggtitle(wrapper("Percentage Reduction in Coronary Heart Disease Events Due to Leisure Time Physical Activity",width=55)) +
  xlab("Gender") + 
  ylab("Percent Reduction") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position="none")
print(ggplot_object)
dev.off()

##############################
#          SLIDE 11         #
##############################

#Reset heart palette
pal_heart <- c('#a50f15','#fb6a4a','#fc9272','#a50f15','#fb6a4a','#fc9272')

##Using this example code: http://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/

# set working directory
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")

df <- read.table(file="slide8_smoking.txt",sep="\t",header=TRUE,quote="")

df$start = mdy(df$start)
df$text = factor(df$text,levels = levels(df$text)[c(6,3,1:2,4:5)])

positions <- c(0.5, -0.5, 0.75, -0.75, 0.3, -0.5)
directions <- c(1, -1)

line_pos <- data.frame(
  "start"=unique(df$start),
  "position"=rep(positions, length.out=length(unique(df$start))),
  "direction"=rep(directions, length.out=length(unique(df$start)))
)

df <- merge(x=df, y=line_pos, by="start", all = TRUE)
df <- df[with(df, order(start)), ]

text_offset <- 0.05

df$month_count <- ave(df$start==df$start, df$start, FUN=cumsum)
df$text_position <- (df$month_count * text_offset * df$direction) + df$position
head(df)

month_buffer <- 2

month_date_range <- seq(min(df$start) - months(month_buffer), max(df$start) + months(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

year_date_range <- seq(min(df$start) - years(1), max(df$start) + years(10), by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)

df$w = strwidth(df$text, 'inches') + 0.25
df$h = strheight(df$text, 'inches') + 0.25

df$box_ymin <- df$text_position - (df$h/4)
df$box_ymax <- df$text_position + (df$h/4)

#### PLOT ####
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_8_smoking.png",width = 2500, height = 1600) 
timeline_plot<-ggplot(df,aes(x=start,y=0, col=text))
timeline_plot<-timeline_plot+labs(col="Milestones")
timeline_plot<-timeline_plot+scale_color_manual(values=rep(blue_heart,6), labels=c("Instantly","6 months","1 year","5 years","10 years","15 years","Placeholder"), drop = FALSE)
timeline_plot<-timeline_plot+theme_classic()

# Plot horizontal black line for timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                                        color = blue_heart, size=0.3)

# Plot vertical segment lines for milestones
timeline_plot<-timeline_plot+geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=start), color=blue_heart, size=2,linetype=2)

# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=15,color=pal_heart)

# Don't show axes, remove legend
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "none")

# Show text for each month
#timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=2.5,vjust=0.5, color='black', angle=90)
# Show year text
timeline_plot<-timeline_plot+geom_text(data=year_df, aes(x=year_date_range,y=-0.05,label=year_format, fontface="bold",angle = 45),size=10, color=blue_heart)

#Create a box for text
timeline_plot<-timeline_plot+geom_rect(data = df, mapping=aes(xmin = start- years(2), xmax = start+ years(10), ymin = box_ymin, ymax = box_ymax),fill = pal_heart,color="black")

# Show text for each milestone
timeline_plot<-timeline_plot+geom_text(aes(y=text_position,hjust=0.1,label=stringr::str_wrap(text, 50)),size=10,fontface="bold",color="white")
print(timeline_plot)
dev.off()

##############################
#          SLIDE 13          #
##############################

pal_heart <- c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#de2d26','#a50f15')

# Set working directory
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")

# Create fake data to recreate graph (http://www.thederrick.com/news/features/experts-heart-healthy-diet-as-effective-as-statins/article_aa659f95-386b-5249-b7ae-1b06ad201b32.html)
diet_data <- data.frame(replacement = c("Polyunsaturated Fats",
                                        "Monounsaturated Fats",
                                        "Complex Carbs",
                                        "Simple Carbs"),
                        reduction = c(50, 30, 18, 0))

# Simple bar graph
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_9_diet_and_heartdisease.png",width = 1000, height = 1000) 
b <- ggplot(diet_data, aes(x = reorder(replacement, -reduction), y = reduction)) + 
  xlab("Food Replacement") +
  ylab("Percent Reduction") +
  geom_bar(color="black",stat = "identity", fill = pal_heart[c(2,4,5,6)] ) +
  scale_color_discrete_heart() + 
  theme_heart() + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  ggtitle("Reduced Probability of Developing Heart Disease by \n Substituting Saturated Fats for Other Food Types") 
print(b)
dev.off()

##############################
#          SLIDE 15          #
##############################

# Set working directory
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")

# read in csv file
heart_disease_deaths <- 
  read.csv(file = "deaths-by-heart-diseases.csv", 
           stringsAsFactors = FALSE)

#check data
str(heart_disease_deaths)

# Create Plot
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_10_HD_deaths_over_time.png",width = 1000, height = 500) 
p <- ggplot(data=heart_disease_deaths,aes(x=Year,y=Death.rate.per.100.000.population))+
  geom_line(size=1.1, color = "#de2d26")+
  guides(linetype=F)+
  scale_x_continuous(breaks = seq(1950, 2016, by = 5))+
  labs(x="Year",y="Death Rate per 100,000 Population") +
  scale_color_discrete_heart() + 
  theme_heart() + 
  ggtitle("Deaths by heart diseases in the U.S. from 1950 to 2016")
print(p)
dev.off()

##############################
#          SLIDE 16          #
##############################

# Set working directory
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")
data <- read.table(file="mortality_reduction.csv",sep=",",stringsAsFactors=FALSE,header=TRUE,quote="'")

# Simple bar graph
setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/visualizations")
png(file = "slide_11_mortality_reduction.png",width = 1000, height = 1000) 
ggplot_object <- ggplot(data=data,
                        aes(x=Behavior, y=Mortality)) +
  geom_bar(aes(fill = Behavior), color="black",position = "dodge", stat="identity") +
  theme_heart() +
  scale_fill_manual(values=pal_heart[c(2,4,6)]) +
  ggtitle("Reduced Mortality by Lifestyle Behavior Modification") +
  xlab("Behavior Modification") + 
  ylab("Percent Reduction in Mortality") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position="none")
print(ggplot_object)
dev.off()

##############################
#          SLIDE X          #
##############################

setwd("~/Documents/GradSchool/MSDS455/git_repos/MSDS455_Charlie/data")

load("polar_plots.RData")


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