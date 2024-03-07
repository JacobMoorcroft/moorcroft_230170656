## this code has been designed to extract data on the provisional public and private woodland area of each 
## country within the UK, and visually display it in proportion to the overall woodland area across the UK

# installs and loads the necessary library(s)

libraries<-c("tidyverse", "ggplot2", "readODS", "here", "rlang", "dplyr")
install.packages(libraries, repos="http://cran.rstudio.com")
library(tidyverse)
library(ggplot2)
library(readODS)
library(here)
library(rlang)
library(dplyr)

# extracts all of the raw data from the ODS and collates it into a single data-frame, following which all 
# created variables besides the total raw data are removed to clean the global environment

sheets<-c("England","Wales","Scotland","Northern Ireland","the UK")
countries_list<-list()

for (i in seq_along(sheets)){
  countries<-sheets[i]
  pathway<-paste0(here("raw_data", "area-timeseries-15jun23.ods"))
  countries_list[[countries]]<-read_ods(pathway, sheet = i+3)
}
raw_extracted_data<-data.frame(countries_list)
rm(list=setdiff(ls(), "raw_extracted_data"))

## provides a glimpse of the current state of the raw, unprocessed data

head(raw_extracted_data)

# begins to process the raw data by cleaning it of unnecessary descriptives, then labelling each column by 
# the titles which were assigned to them within the original file, before removing the names as a row in the data

processed_extracted_data<-raw_extracted_data[-c(1:3),] 
names(processed_extracted_data)<-as.matrix(processed_extracted_data[1,])
processed_extracted_data<-processed_extracted_data[-1,]

# extracts the total woodland area per country, from the UK as a whole, and the range of years from 1998-2023, 
# transforming them all into numerical variables which can be used for the upcoming visualisation

year_ending_March_31st<-as.numeric(processed_extracted_data$`Year ending 31 March`)
woodland_area<-select(processed_extracted_data, ends_with(" total (thousand ha)"))
country_names<-c("England", "Wales", "Scotland", "Northern Ireland", "UK")

for (i in country_names){
  assign(i, as.numeric(woodland_area[[paste0(i, " total (thousand ha)")]]))
}

## renames and moves the processed data on the woodland area across the years into a final data-frame

woodland_area_by_country<-data.frame(England,Wales,Scotland,`Northern Ireland`,UK)
rm(list=setdiff(ls(),c("woodland_area_by_country","year_ending_March_31st")))
woodland_area_by_country<-woodland_area_by_country%>%
  rename(Northern_Ireland=`Northern.Ireland`,
         the_United_Kingdom=UK)
view(woodland_area_by_country)

## initial scatterplots can be formed to get a basic understanding of the visual relationship between year and 
## woodland area, with all countries showing a progressive, albeit not always linear, increase in area over time

pointcolour="brown"
pointsize=1.5
fig_path<-here("figs")

country_names<-c("England", "Wales", "Scotland", "Northern_Ireland", "the_United_Kingdom")
for(country in country_names){
  plot<-ggplot(woodland_area_by_country,aes(year_ending_March_31st,!!sym(country)))+
    geom_point(colour=pointcolour,size=pointsize)+
    labs(x="Year commencing 31st March",y="Woodland Area (in thousand hectares)")+
    ggtitle(paste("Change in the Woodland Area of",country,"from 1998 to 2023"))
  
  filename<-paste("scatterplot_of_",country,"_woodland_over_time.png",sep="")
  ggsave(file.path(fig_path,filename),plot)
}

rm(list=setdiff(ls(),c("woodland_area_by_country","year_ending_March_31st", "plot")))




















## NONSENSE NONSEN NONSNEON NONSENSEEEEEEEEEEEEEEEEEE NONSEN NONSESNE NONSENSE ########################################

EnglandData<-read_ods("raw_data/area-timeseries-15jun23.ods",sheet=4)
WalesData<-read_ods("raw_data/area-timeseries-15jun23.ods",sheet=5)
ScotlandData<-read_ods("raw_data/area-timeseries-15jun23.ods",sheet=6) 
NIData<-read_ods("raw_data/area-timeseries-15jun23.ods",sheet=7) 
UKData<-read_ods("raw_data/area-timeseries-15jun23.ods",sheet=8)
TotalExtractedData<-data.frame(EnglandData,WalesData,ScotlandData,NIData,UKData)
rm(list=setdiff(ls(),"TotalExtractedData"))

year<-as.numeric(raw_extracted_data$`Year ending 31 March`)
England<-as.numeric(woodland_area$`England total (thousand ha)`)
Wales<-as.numeric(woodland_area$`Wales total (thousand ha)`)
Scotland<-as.numeric(woodland_area$`Scotland total (thousand ha)`)
NI<-as.numeric(woodland_area$`Northern Ireland total (thousand ha)`)
UK_Total<-as.numeric(woodland_area$`UK total (thousand ha)`)

EnglandWoodlandArea<-read_csv(here('data/processed_data', "England.csv"))
ScotlandWoodlandArea<-read_csv(here('data/processed_data', "Scotland.csv"))
WalesWoodlandArea<-read_csv(here('data/processed_data', "Wales.csv"))
NIWoodlandArea<-read_csv(here('data/processed_data', "NI.csv"))
UKWoodlandArea<-read_csv(here('data/processed_data', "UK.csv"))

# extracting the relevant data on the total amount of trees and the year range into a new data.frame

Total_Woodland_Area<-matrix(c(EnglandWoodlandArea$`England total (thousand ha)`, NIWoodlandArea$`Northern Ireland total (thousand ha)`, 
                              ScotlandWoodlandArea$`Scotland total (thousand ha)`, WalesWoodlandArea$`Wales total (thousand ha)`, 
                              UKWoodlandArea$`UK total (thousand ha)`), ncol=5, nrow=26)
Year<-EnglandWoodlandArea$`Year ending 31 March`
Woodland_Growth_By_Year<-data.frame(Year, Total_Woodland_Area)

Micro2<-Micro%>% 
  separate(col=...1, into=c("..","Folder", "Subfolder", "Filename"), sep="/") %>% 
  subset(select=-c(..,Folder,Subfolder)) %>%
  mutate(participant_number=str_sub(Filename, end=3)) %>%
  mutate(condition=str_sub(Filename, start=4,end=7))

view(Micro2)

Micro2 %>% 
  group_by(condition) %>% 
  summarise(mean=mean(microsaccades_per_second))

Woodland_Growth_By_Year2<-Woodland_Growth_By_Year %>% 
  rename()

names(Woodland_Growth_By_Year)<-c("Year","England_Trees","NI_Trees","Scotland_Trees","Wales_Trees","UK_Trees")
