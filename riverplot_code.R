##Idea from https://coolbutuseless.github.io/package/devoutsvg/articles/svg-with-gradient-fill.html adapted to plot year over year river flow data

##Load and install packages
install.packages("here")
install.packages("data.table")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dyplr")
install.packages("ggplot2")
install.packages("ggridges")
install.packages("devtools")
install.packages("scales")
install.packages("dataRetrieval")



devtools::install_github("coolbutuseless/lofi")      
devtools::install_github("coolbutuseless/minisvg")   
devtools::install_github("coolbutuseless/devout")    
devtools::install_github("coolbutuseless/devoutsvg") 
devtools::install_github("coolbutuseless/poissoned") 
devtools::install_github("coolbutuseless/svgpatternsimple") 
devtools::install_github("coolbutuseless/svgpatternusgs") 

#----------------------side note to get Rtools40 to work ----------------------------------------------------

writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

#------------------------------------------------------------------------------------------------------------
library(devtools)
library(here)
library(lubridate)
library(data.table)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(dplyr)
library(scales)
library(svgpatternusgs)
library(dataRetrieval)

##Packages specific for this

library(lofi)
library(minisvg)
library(devout)
library(devoutsvg)
library(svgpatternsimple)
library(poissoned)


#-------------------------------------------------------------INPUT USGS Station Number data-------------------------
##Enter parameters  
siteNumber <- "14105700" 
Beginyear<- 1995
Endyear<-2018
Beginmonthday <- "01-01"
Endmonthday <- "12-1"


#Sets values for graph 
RiverInfo <- readNWISsite(siteNumber)
ChartTitle <- RiverInfo$station_nm
SubTitle <- paste("Cubic Feet Per Second (CFS)", Beginyear, "to", Endyear)
Caption <- paste("Source:", RiverInfo$agency_cd," ","Site Number", siteNumber)
FileName <- sub("(\\w+\\s+\\w+).*", "\\1", RiverInfo$station_nm)
parameterCd <- "00060"

#Raw daily data:
riverdata <- readNWISdv(siteNumber,parameterCd, paste(Beginyear,"-",Beginmonthday, sep=""), paste(Endyear,"-",Endmonthday, sep=""))

#Select Columns
prdata <- riverdata %>% select(3, 4) 
names(prdata)[1] <- "timestamp"
names(prdata)[2] <- "cfs"
prdata$timestamp <- as.Date(prdata$timestamp)


#Grab the maximum value each day
prdata_hour <- prdata %>% mutate(year = year(timestamp), month = month(timestamp), day = day(timestamp)) %>% 
		group_by(year,month, day) %>% slice(which.max(cfs))%>%
		group_by(year)%>% mutate(grouped_id = row_number())%>% 
		mutate(Year = yday(timestamp))%>% mutate(flag = 1)


##Get the Highest and Lowest value for each year
prdata_hour_label_sub <- prdata_hour %>% group_by(year) %>%  mutate(label = as.numeric(ifelse(cfs == max(cfs), max(cfs), ifelse(cfs == min(cfs), min(cfs), "")))) %>% distinct(label, .keep_all=T) %>% select(year, label, grouped_id) %>% filter(!is.na(label)) 

prdata_hour_label <- left_join(prdata_hour,prdata_hour_label_sub) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create SVG gradient pattern definition
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gradient_pattern <- svgpatternsimple::create_pattern_gradient(
  id      = "p1",      # HTML/SVG id to assign to this pattern
  angle   = 90,        # Direction of the gradient
  colour1 = "#c9f4de",   # Starting color
  colour2 = "#4169e1"  # Final color
)



#visualise color if wanted
#gradient_pattern$show()

my_pattern_list <- list(
  `#000001` = list(fill = gradient_pattern)
)
#create rivergraph
svgout(filename = paste(FileName,".svg"), pattern_list=my_pattern_list, width=11, height=17)
ggplot(prdata_hour_label, aes(x=Year, y=-year, height=cfs, group=year, label = label, fill=as.factor(flag)))+
  geom_density_ridges(stat="identity", alpha=1, scale=1, colour=NA, fill='#000001')+
  geom_text(aes(label=scales::comma(label, accuracy = 1)), size = 7, nudge_y = .75)+
  theme_classic(base_size = 22)+
  scale_y_continuous(breaks=c(-Beginyear:-Endyear), labels=c(Beginyear:Endyear), name="", position="right")+
  scale_x_continuous(breaks=c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360), name= "",labels = c("Jan", "Feb", "Mar", "Apr", "May", "June","July","Aug", "Sep", "Oct", "Nov", "Dec", ""))+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(), text=element_text(family="Georgia"))+
  labs(title=RiverInfo$station_nm, 
       subtitle=SubTitle, 
       caption=Caption)
invisible(dev.off())




  


