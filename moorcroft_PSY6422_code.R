## This code has been designed to extract data on the provisional woodland area within each country of the United Kingdom from 1998-2023, calculate the
## percentage increase over time, and visualise the overall growth of woodland area in the UK from 1998-2023 both in total amount and proportional increase

# Should the packages need to be installed:

#libraries<-c("tidyverse","cowplot", "ggplot2", "readODS", "here", "rlang", "dplyr")
#install.packages(libraries, repos="http://cran.rstudio.com")

# Loading of necessary packages:

library(tidyverse)
library(ggplot2)
library(readODS)
library(here)
library(rlang)
library(dplyr)
library(cowplot)

# Extraction of raw data from the ODS file

sheets<-c("England","Wales","Scotland","Northern Ireland")
countries_list<-list()
for (i in seq_along(sheets)){
  countries<-sheets[i]
  pathway<-paste0(here("raw_data", "area-timeseries-15jun23.ods"))
  countries_list[[countries]]<-read_ods(pathway, sheet = i+3)
} # this loop extracts all of the raw data and collates it into a data-frame

raw_extracted_data<-data.frame(countries_list)
rm(list=setdiff(ls(), "raw_extracted_data")) # removes now unnecessary variables to clean environment

# Glimpse of the raw data

raw_extracted_data

# Processing and cleaning of data

processed_extracted_data<-raw_extracted_data[-c(1:3),] # removes unnecessary text
names(processed_extracted_data)<-as.matrix(processed_extracted_data[1,]) # labels each column by their original titles
processed_extracted_data<-processed_extracted_data[-1,] # removes text names from the data

# Glimpse of the processed data

processed_extracted_data

# Extracts the Years and Amount of Woodland Area for each country

year_ending_March_31st<-as.numeric(processed_extracted_data$`Year ending 31 March`) # The Year
woodland_area<-select(processed_extracted_data, ends_with(" total (thousand ha)")) # Amount of Woodland Area
country_names<-c("England", "Wales", "Scotland", "Northern Ireland") # Country Names

for (i in country_names){
  assign(i, as.numeric(woodland_area[[paste0(i, " total (thousand ha)")]]))
} # converts data to numeric for visualisation

# Tabulates change in woodland area per country from 1998 to 2023

woodland_area_by_country<-data.frame(England,Wales,Scotland,`Northern Ireland`)
woodland_area_by_country<-woodland_area_by_country%>%
  rename(Northern_Ireland=`Northern.Ireland`)
table(is.na(woodland_area_by_country)) #no missing data!
country_names<-c("England", "Wales", "Scotland", "Northern_Ireland")

# Calculates percentage increase in woodland area per country from 1998 to 2023

percentage_results<-data.frame(country=character(),percentage_increase=numeric(),stringsAsFactors=FALSE) # creates an empty data-frame
for(country in country_names){
  min_v<-min(woodland_area_by_country[[country]])
  max_v<-max(woodland_area_by_country[[country]])
  percentage_increase<-(((max_v-min_v)/min_v)*100)
  percentage_results<-rbind(percentage_results, data.frame(country=country,percentage_increase=percentage_increase))
} # calculates the percentage increase from 1998 to 2023 for each country
percentage_results[1:4,2]<-round(percentage_results[1:4,2],2) # rounds the data to 2 decimal places

# Shows the data to allow a visual check for the validity and credibility of the results 

woodland_area_by_country
percentage_results

# Removes all the unnecessary variables, and creates a final data-frame amenable to the upcoming visualisation

rm(list=setdiff(ls(),c("processed_extracted_data","woodland_area_by_country","country_names","year_ending_March_31st", "plot","percentage_results","fig_path")))
woodland_growth_over_time<-data.frame(
  country=c(rep("England",26),rep("Wales",26),rep("Scotland",26),rep("Northern Ireland",26)),
  woodland=c(woodland_area_by_country$England,woodland_area_by_country$Wales,
             woodland_area_by_country$Scotland,woodland_area_by_country$Northern_Ireland),
  year=c(year_ending_March_31st)
)

# Creates the mapping for the visualisation, and the figure path for saving it

mapping<-aes(x=year,y=woodland,colour=country)
fig_path<-here("figs")

# Visualisation of the growth of woodland area within the United Kingdom, from 1998 to 2023

FinalPlot<-woodland_growth_over_time %>%
  ggplot(mapping=mapping)+
  geom_smooth(method="gam")+
  labs(x="Year commencing from March 31st",
       y="Woodland area (in thousand hectares)",
       colour="Country",
       title="The Growth of Woodland Area within the United Kingdom",
       subtitle="As annotated with percentage increase from 1998-2023",
       caption="Data retrieved from: Forest Research, 2023")+
  annotate("label",x=2010,y=1210,label=paste(percentage_results[1,2],"%"),colour="#EE0000",size=3,fontface="bold")+
  annotate("label",x=2010,y=375,label=paste(percentage_results[2,2],"%"),colour="#00CD00",size=3,fontface="bold")+
  annotate("label",x=2010,y=1450,label=paste(percentage_results[3,2],"%"),colour="#0000CD",size=3,fontface="bold")+
  annotate("label",x=2010,y=175,label=paste(percentage_results[4,2],"%"),colour="#FFA500",size=3,fontface="bold")+
  scale_colour_manual(values=c(England="#EE0000",Wales="#00CD00",
                               Scotland="#0000CD",`Northern Ireland`="#FFA500"))+
  scale_x_continuous(breaks=seq(1998,2023,5))+
  scale_y_continuous(breaks=seq(0,1500,150))+
  theme(panel.border=element_rect(colour="#8B7355",fill=NA,linewidth=2),
        panel.grid.minor=element_line(colour="#CAFF70",linewidth=0.5),
        panel.grid.major=element_line(colour="#CAFF70",linewidth=0.7),
        panel.background=element_rect(fill="#FFFFF0"),
        plot.subtitle=element_text(face="italic"),
        legend.box.background=element_rect(fill="black"))

FinalPlot

# Saves the plot

filename<-paste("The Growth of Woodland Area in the UK from 1998 to 2023.png",sep="")
ggsave(file.path(fig_path,filename),plot=FinalPlot,width=7,height=6.37)
