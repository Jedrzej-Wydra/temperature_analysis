# preparing data from weather station

library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)

# august

weather_station_august <- read_excel('datasets/august.xlsx')

names(weather_station_august) <- c('Date', 'Time', 'Temperature')

weather_station_august %>%
  transmute(time = paste(Date, str_sub(Time, 1L, 2L)),
            temperature = Temperature) %>% 
  group_by(time) %>% 
  summarise(temperature = mean(temperature)) %>% 
  na.omit() %>% 
  mutate(time = ymd_h(time)) %>% 
  subset(time>=ymd_h('2021-08-02 01') & time<=ymd_h('2021-08-17 01')) ->
  weather_station_august

time_august <- ymd_h('2021-08-02 01') + hours(0:360)

weather_station_august <- left_join(data.frame(time = time_august), weather_station_august)

# december

weather_station_december_1 <- read_excel('datasets/december_1.xlsx')
weather_station_december_2 <- read_excel('datasets/december_2.xlsx')

weather_station_december <- rbind(weather_station_december_1,
                                  weather_station_december_2)

remove(weather_station_december_1, weather_station_december_2)

names(weather_station_december) <- c('Date', 'Time', 'Temperature')

weather_station_december %>%
  transmute(time = paste(Date, str_sub(Time, 1L, 2L)),
            temperature = Temperature) %>% 
  group_by(time) %>% 
  summarise(temperature = mean(temperature)) %>% 
  na.omit() %>% 
  mutate(time = ymd_h(time)) %>% 
  subset(time>=ymd_h('2021-12-05 01') & time<=ymd_h('2021-12-20 01')) ->
  weather_station_december

time_december <- ymd_h('2021-12-05 01') + hours(0:360)

weather_station_december <- left_join(data.frame(time = time_december), weather_station_december)

#save datasets

write.csv(weather_station_august, 'datasets/August/weather_station/weather_station.csv')
write.csv(weather_station_december, 'datasets/December/weather_station/weather_station.csv')
