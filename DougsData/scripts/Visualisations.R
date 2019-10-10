# Visualisation for BOM data
library(tidyverse)
BOM_combined<-read_csv("data/BOM_combined.csv")

## Question 1: 
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?

BOM_combined %>% 
  filter(t_min !="", t_max != "", Rainfall != 0) %>%  
  group_by(Station_number) %>% 
  summarise(n_rows = n()) 


## Question 2:
# Which month saw the lowest average daily temperature difference?
BOM_combined %>%
  mutate(Temp_diff=t_max-t_min) %>%
  group_by(Month) %>%
  summarise(meantempdiff = mean(Temp_diff)) %>%
arrange(meantempdiff)

## Question 3
# Which state saw the lowest average daily temperature difference?
BOM_combined %>%
  mutate(Temp_diff=t_max-t_min) %>%
  group_by(state) %>%
  summarise(meantempdiff = mean(Temp_diff)) %>%
  arrange(meantempdiff)
