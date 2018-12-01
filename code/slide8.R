#Install Packages
list.of.packages <- c("ggplot2", "scales","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

pal_heart <- c('#a50f15','#fb6a4a','#fc9272','#a50f15','#fb6a4a','#fc9272')
blue_heart <- "#3F69AA"

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
