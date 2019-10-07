library(tidyverse)

BOM_data <- read_csv("data/BOM_data.csv")
BOM_data

BOM_stations <- read_csv("data/BOM_stations.csv")
BOM_stations

## Questions

## Question 1: 
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?

BOM_data %>% 
  separate(Temp_min_max, into = c("min", 'max')) %>% 
  filter(min != "", max != "", Rainfall != 0) %>%  
  group_by(Station_number) %>% 
  summarise(n_rows = n()) 


## Question 2:
# Which month saw the lowest average daily temperature difference?

BOM_data %>% 
  separate(Temp_min_max, into = c("min", 'max')) %>%
  filter(min != "", max != "") %>%  
  mutate(temp_difference = as.numeric(max) - as.numeric(min)) %>% 
  group_by(Month) %>% 
  summarise(mean_temp_dif = mean(temp_difference)) %>% 
  arrange(mean_temp_dif)  


## Question 3
# Which state saw the lowest average daily temperature difference?


# Gather into 3 columns - Station ID, Type of Data (info), actual recorded value itself

BOM_stations_int <- gather(BOM_stations, Station_number, value, -info)
BOM_stations_int

# Spread into a shape with one row for each station.
BOM_stations_tidy <- spread(BOM_stations_int, info, value)
BOM_stations_tidy

# Join the two sata sets together to identify data fo each weather station. Check that the two DF's 
# have a shared column to merge and that they are the same data type. 

# convert station number from a character to a value
BOM_stations_tidy <- BOM_stations_tidy %>% 
  mutate(Station_number = as.numeric(Station_number))

# Join the two data frames
BOM_combined <- left_join(BOM_stations_tidy, BOM_data)

# Use the solution from Question 2 to work out which state saw the lowest average daily temperature difference?
BOM_combined %>% 
  separate(Temp_min_max, into = c("min", 'max')) %>% 
  filter(min != "", max != "") %>% 
  mutate(temp_difference = as.numeric(max) - as.numeric(min)) %>% 
  group_by(state) %>% # Group by state
  summarise(mean_temp_dif = mean(temp_difference)) %>% 
  arrange(mean_temp_dif) 


## Question 4
# Does the westmost (lowest longitude) or eastmost (highest longitude) weather station in our dataset have a higher average solar exposure?

# For the lowest longitude 
BOM_combined %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>% 
  group_by(Station_number, lon) %>% 
  summarise(mean_solar = mean(Solar_exposure, na.rm = TRUE)) %>%   
  arrange(lon)

# For the highest longitude 
BOM_combined %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>%
  group_by(Station_number, lon) %>% # 
  summarise(mean_solar = mean(Solar_exposure, na.rm = TRUE)) %>%  #  
  arrange(desc(lon)) # 









