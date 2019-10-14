#BOM data plotting.
library(tidyverse)
#read in csv
BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations<-read_csv("data/BOM_stations.csv")

  # Q1: For each station, how many days have a minimum temperature, a maximum temperature -----
  #    and a rainfall measurement recorded?
  
BOM_with_temps <- BOM_data %>% 
    separate(Temp_min_max, into = c("t_min", "t_max"), sep = "/") 
  
q1_ans <- BOM_with_temps %>% 
    filter(t_min != "-", t_max != "-", Rainfall != "-") %>% 
    group_by(Station_number) %>% 
    summarise(num_records = n())

BOM_data_temps2 <- BOM_with_temps %>% 
  # t_min/t_max are text ("<chr>") need to convert them to numbers
  mutate(t_max = as.numeric(t_max)) %>% 
  mutate(t_min = as.numeric(t_min))%>%
  #t_diff will be NA where the t_min or t_max values were missing, need to remove those lines
  filter(!is.na(t_min))%>% 
  filter(!is.na(t_max))

# Take out Perth 9925 and display data  
Perth <- filter(BOM_with_temps2,Station_number==9225)

ggplot(Perth,aes(x=Day,y=t_min))+
  geom_point()

#Joining BOM data with station data
BOM_stations_int <- gather(BOM_stations, Station_number, value, -info)
BOM_stations_int

# Spread into a shape with one row for each station. The 'key' argument identifies the data for the column names
# and 'value' entifes the column the will provide the data for the new cells

BOM_stations_tidy <- spread(BOM_stations_int, info, value)
BOM_stations_tidy

# Finally you want to join the two data sets together to identify the state of each weather station. Check that the two DF's 
# have a shared column to merge and that they are the same data type. 

# I have to convert station number from a character to a value
BOM_stations_tidy <- BOM_stations_tidy %>% 
  mutate(Station_number = as.numeric(Station_number))

# Joining the two data frames
BOM_combined <- left_join(BOM_stations_tidy, BOM_data_temps2)

# Write BOM combined to a csv
write_csv(BOM_combined,"data/BOM_combined.csv")

