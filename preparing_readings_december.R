# preparing data from recorders

library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)

# december

field_december_1 <- read_csv('datasets/December/field/field_1.csv',skip = 1)
field_december_2 <- read_csv('datasets/December/field/field_2.csv',skip = 1)

names(field_december_1) <- c('nr', 'Time', 'Temp1', 'Temp2', 'col1', 'col2')
names(field_december_2) <- c('nr1', 'Time1', 'Temp3', 'Temp4', 'col11', 'col21')

field_december <- cbind(field_december_1, field_december_2)

remove(field_december_1, field_december_2)

field_december %>%
  transmute(time = ymd_h('2021-12-05 01') + minutes(0:(length(field_december$Time)-1)),
            temperature = (Temp1 + Temp2 + Temp3 + Temp4) / 4) %>% 
  mutate(time = str_sub(as.character(time), 1L, 13L)) %>% 
  group_by(time) %>% 
  summarise(temperature = mean(temperature)) %>% 
  na.omit() %>% 
  mutate(time = ymd_h(time)) %>% 
  subset(time>=ymd_h('2021-12-05 00') & time<=ymd_h('2021-12-20 01')) ->
  field_december

ggplot(weather_station_december, aes(time, temperature)) +
  geom_line() +
  theme_bw() +
  geom_line(data = field_december, aes(time, temperature), color = 'red')


