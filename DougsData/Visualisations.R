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

#Visualising BOM data

ggplot(BOM_combined,aes(x = Year, y = t_min, colour = Month)) +
  geom_line() +
  facet_wrap(~ state)
a_countries
library(tidyverse)
ggplot(data=a_countries,aes(x=year,y=lifeExp,color=continent))+
  geom_line()+
  facet_wrap(~countries)

#Challenge 12
ggplot(data=gapminder_2012,mapping=aes(x=gdpPercap,y=lifeExp,colour=continent,size=pop))+
geom_point()+
scale_x_log10()+
facet_wrap(~year)

ggplot(
  data=gapminder,
  mapping=aes(x=gdpPercap,y=lifeExp,colour=continent,size=pop))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~year)  

#challenge 13
ggplot(data=gapminder,aes(x=pop,fill=continent))+
  geom_density(alpha=0.6)+
  facet_wrap(~year)+
  scale_x_log10()
#Introduction to visualation 
#challenge 1
summary(data)
data()

#creating graphics with ggplot (intro to data vis)
#challenge 3
ggplot(gapminder_1977,aes(x=gdpPercap,y=lifeExp))+
  geom_point(colour="blue",size=5)
gapminder<-read_csv("data/gapminder.csv")
gapminder_1977<-filter(gapminder,year==1977)
ggplot(
  data=gapminder_1977,
  mapping=aes(x=gdpPercap,y=lifeExp,colour=continent,size=pop))+
    geom_point()

#challenge 4
ggplot(data=gapminder_1977)+
  geom_point(mapping=aes(x=gdpPercap,y=lifeExp,colour=continent,size=pop))
 
ggplot(data=gapminder_1977)+
  geom_point(mapping=aes(x=gdpPercap,y=lifeExp,colour=continent,size=pop)
             )
ggplot(gapminder_1977,aes(x=gdpPercap,y=lifeExp))+
  geom_point(colour="blue",size=5)

 #Challenge 7

ggplot(data=gapminder,mapping=aes(x=year,y=lifeExp))+geom_point()
       
 ggplot(data=gapminder,aes(x=year,y=lifeExp, group=country,colour=continent))+
   geom_line()
 ggplot(data=gapminder,aes(x=year,y=lifeExp,group=country,colour=continent))+
   geom_line()+
   geom_point()
 
 ggplot(data=gapminder,aes(x=year,y=lifeExp,group=country))+
          geom_line(mapping=aes(colour=continent))+
          geom_point()  
 
ggplot(data=gapminder_1977,aes(x = year, y = lifeExp, group = country,colour = continent))+
  geom_point(colour="black")+
  geom_line()
ggplot(data=gapminder,mapping=aes(x=gdpPercap,y=lifeExp))+
  geom_point()

ggplot(data=gapminder,aes(x=gdpPercap,y=lifeExp))+
  geom_point(alpha=0.5)+
  scale_x_log10()

ggplot(data=gapminder,aes(x=gdpPercap,y=lifeExp))+
  geom_point()+
  scale_x_log10()+
  geom_smooth(method="lm")

ggplot(data=gapminder,aes(x=gdpPercap,y=lifeExp))+
  geom_point(size=3,color="orange")+
  scale_x_log10()+
  geom_smooth(method="lm",size=1.5)

#challenge10
ggplot(data=gapminder,aes(x=gdpPercap,y=lifeExp,color=continent))+
  geom_point(size=3,shape=17)+
  scale_x_log10
  geom_smooth(method="lm",size=1.5)
#challenge 10b
  ggplot(data=gapminder,aes(x=gdpPercap,y=lifeExp,color=continent))+
    geom_point()+
    scale_x_log10()+
    scale_colour_manual(values=c("red","green", "blue","purple","black"))
  
 #challenge 11
  ggplot(data=gapminder,aes(x=gdpPercap,y=lifeExp,color=continent))+
    geom_point()+
  scale_x_log10()+
    scale_color_brewer(palette="Set1")
  
  a_countries<-filter (gapminder,str_starts(country,"A"))

  ggplot(data=a_countries,aes(x=year,y=lifeExp,color=continent))+
    geom_line()+
    facet_wrap(~country)
  
 # Challenge 12
  ggplot(
    data=gapminder,
    mapping=aes(x= gdpPercap,y = lifeExp,colour = continent,size= pop))+
      geom_point()+
      scale_x_log10()+
      facet_wrap(~year)
  
 #challenge 13
  ggplot(data=gapminder,aes(x=pop, fill=continent))+
    geom_density(alpha=0.6)+
    facet_wrap(~year)+
    scale_x_log10()

    
  