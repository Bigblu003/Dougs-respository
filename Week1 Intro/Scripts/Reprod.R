library(tidyverse)

#read in file
BOM_stations <- read_csv("Data/BOM_stations.csv")
BOM_data<-read_csv("Data/BOM_data.csv")
#Separate temperatures into min and max
separated_min_max<-separate(BOM_data, col = Temp_min_max, into = c('min', 'max'), sep = "/") 
data<-separated_min_max %>% 
#Filter out min rows with no value
filter(min!='-')%>% 
#Filter out max rows with no value
filter(max!='-')%>% 
#Filter out rainfall
filter(Rainfall != '0') 
#Group by Station numbers
group_by(station_number) %>%
#Count number of rows
summarise(n = n())
write_csv(data, "results/question1.csv")


bom_data <- read.csv("Results/question1")

separate(BOM_data, col=Temp_min_max,into = c("temp_min", "temp_max"), sep = "/") %>%
  filter(Rainfall != "-", temp_min != "-", temp_max != "-") %>%
  group_by(Station_number) %>%
  summarise(num_rows=n())
Q2<-glimpse(BOM_data)
BOM_data_tidy<-BOM_data%>%
  separate(col=Temp_min_max,into=c("Temp_min","Temp_max"),"/")%>%
  mutate(Temp_min=as.numeric(Temp-min))%/%
  mutate(Temp_max=as.numeric(Temp_max))%/%
  mutate(Rainfall=as.numeric(Temp_max))%/%
  mutate(Solar_exposure=as.numeric(Solar_exposure))%/%
  mutate(Temp_diff=Temp_max-Temp_max-Temp_min)

Q2<-BOM_data_tidy %/%
  filter(!is.na(Temp_diff))%/%
  group_by(Month)%/%
summarise(mean_TDiff=mean(Temp_diff))%/%
  filter(Mean_TDiff==min(TDiff))
