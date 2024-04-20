## This code has been designed to extract data on the provisional woodland area within each country of the United Kingdom (UK) from 1998-2023, calculate the
## percentage increase for each country over this time period, then visualise the growth of woodland area in the UK from 1998-2023 both in terms of numeric 
## and proportional changes. This facilitates more appropriate comparisons of woodland development in the UK than simply changes from base amount in 1998.

## Should packages need to be installed, remove '#' and run:

#libraries<-c("tidyverse", "cowplot", "magick", "readODS", "here")
#install.packages(libraries, repos="http://cran.rstudio.com")

## NECESSARY PACKAGES:

library(here)
library(readODS)
library(tidyverse)
library(cowplot)
library(magick)

## DATA PREPARATION:
# Extraction of raw data from ODS file

sheets<-c("England","Wales","Scotland","Northern Ireland")
countries_list<-list()
for (i in seq_along(sheets)){
  countries<-sheets[i]
  pathway<-paste0(here("raw_data", "area-timeseries-15jun23.ods"))
  countries_list[[countries]]<-read_ods(pathway, sheet = i+3)
} # this loop extracts all of the raw data from the country-specific sheets
raw_extracted_data<-data.frame(countries_list) # which is then put into a dataframe

rm(list=setdiff(ls(), "raw_extracted_data")) # removes unnecessary variables to clean environment

# Glimpse of the raw data

raw_extracted_data

# Processing and cleaning of data

processed_extracted_data<-raw_extracted_data[-c(1:3),] # removes unnecessary text
names(processed_extracted_data)<-as.matrix(processed_extracted_data[1,]) # labels each column by their original titles
processed_extracted_data<-processed_extracted_data[-1,] # removes text names from the data

# Glimpse of the processed data

processed_extracted_data

## DATA EXTRACTION: 
# Extraction of necessary variables from processed dataframe

year_ending_March_31st<-as.numeric(processed_extracted_data$`Year ending 31 March`) # The Year
woodland_area<-select(processed_extracted_data, ends_with(" total (thousand ha)")) # Amount of Woodland Area
country_names<-c("England", "Wales", "Scotland", "Northern Ireland") # Country Names

for (country in country_names){
  assign(country, as.numeric(woodland_area[[paste0(country, " total (thousand ha)")]]))
} # creates numeric variables for the woodland area of each country, per year
woodland_area<-data.frame(England,Wales,Scotland,`Northern Ireland`) # which is then put back into the dataframe
table(is.na(woodland_area)) # checks for any missing data - none!

woodland_area<-woodland_area%>%
  rename(Northern_Ireland=`Northern.Ireland`)
country_names<-c("England", "Wales", "Scotland", "Northern_Ireland") # corrects for interaction issues caused by spacing of `Northern Ireland`

# Calculation of proportional change in woodland area per country

percentage_results<-data.frame(country=character(),percentage_increase=numeric(),stringsAsFactors=FALSE) # creates an empty dataframe
for(country in country_names){
  min_v<-min(woodland_area[[country]])
  max_v<-max(woodland_area[[country]])
  percentage_increase<-(((max_v-min_v)/min_v)*100)
  percentage_results<-rbind(percentage_results, data.frame(country=country,percentage_increase=percentage_increase))
} # this loop calculates the percentage increase in woodland area of each country from 1998-2023
percentage_results[1:4,2]<-round(percentage_results[1:4,2],2) # then rounds the data to 2 decimal places

## PRELIMINARY DATA CHECK: 

woodland_area
percentage_results

## VISUALISATION:

rm(list=setdiff(ls(),c("woodland_area","year_ending_March_31st","country_names","percentage_results"))) # removes all unnecessary variables

woodland_growth_over_time<-data.frame(
  country=c(rep("England",26),rep("Wales",26),rep("Scotland",26),rep("Northern Ireland",26)),
  woodland=c(woodland_area$England,woodland_area$Wales,
             woodland_area$Scotland,woodland_area$Northern_Ireland),
  year=c(year_ending_March_31st)
) # creates a final dataframe amenable to the upcoming visualisation

mapping<-aes(x=year,y=woodland,colour=country) # creates the mapping for the visualisation
fig_path<-here("figs") # creates the necessary path for saving the figure
logo_file<-paste0(here("logo","Picture1.jpg")) # creates the path for applying the logo

# Creates a plot mapping the amount of woodland area development from 1998-2023, as divisable by country, and as annotated with percentage increase

FinalPlot<-woodland_growth_over_time %>%
  ggplot(mapping=mapping)+
  geom_smooth(method="gam")+
  labs(x="Year (commencing from March 31st)",
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
        axis.line=element_line(linewidth=2,colour="#8B7355"),
        plot.title=element_text(face="bold"),
        plot.subtitle=element_text(face="italic"),
        text=element_text(family="serif"),
        legend.title=element_text(face="bold"),
        legend.box.background=element_rect(colour="#8B7355"),
        legend.box.margin=margin(1,1,1,1),
        legend.key=element_rect(colour="#8B7355"))

# Adds the official logo to sit alongside the data source

FinalPlot<-ggdraw(FinalPlot)+
  draw_image(logo_file, scale=.2,x=1,hjust=1,halign=1,valign=0)

## Visualisation of the growth of woodland area within the United Kingdom, from 1998 to 2023

FinalPlot

## SAVES THE PLOT

filename<-paste("The Growth of Woodland Area in the UK from 1998 to 2023.png",sep="")
ggsave(file.path(fig_path,filename),plot=FinalPlot,width=7,height=6.37)
