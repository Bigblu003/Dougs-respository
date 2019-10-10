#BOM data plotting.
library(tidyverse)
#read in csv
BOM_data <- read_csv("BOM_data.csv")
BOM_stations<-read_csv("BOM_stations.csv")

  # Q1: For each station, how many days have a minimum temperature, a maximum temperature -----
  #    and a rainfall measurement recorded?
  
BOM_with_temps <- BOM_data %>% 
    separate(Temp_min_max, into = c("t_min", "t_max"), sep = "/") 
  
q1_ans <- BOM_with_temps %>% 
    filter(t_min != "-", t_max != "-", Rainfall != "-") %>% 
    group_by(Station_number) %>% 
    summarise(num_records = n())

# We are changing the temp columns to numeric values
BOM_with_temps2<- BOM_with_temps%>% 
 mutate(t_max= as.numeric(t_max))%>%
mutate(as.numeric(t_min))

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


