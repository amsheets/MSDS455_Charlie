#Install Packages
list.of.packages <- c("ggplot2", "choroplethrMaps","RColorBrewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

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
png(file = "slide_5_HD_by_State.png",width = 1000, height = 1000) 

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
c$render() + theme(plot.title = element_text(color = "#3F69AA"),
                   legend.title = element_text(color = "#3F69AA"))
dev.off()
