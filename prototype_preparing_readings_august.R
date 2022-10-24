# preparing data from recorders

library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)

# august

field_august_1 <- read_csv('datasets/August/field/field_1.csv',skip = 1)
field_august_2 <- read_csv('datasets/August/field/field_2.csv',skip = 1)

names(field_august_1) <- c('nr', 'Time', 'Temp1', 'Temp2', 'col1', 'col2')
names(field_august_2) <- c('nr1', 'Time1', 'Temp3', 'Temp4', 'col11', 'col21')

field_august <- cbind(field_august_1, field_august_2)

remove(field_august_1, field_august_2)

field_august %>%
  transmute(time = ymd_h('2021-08-02 01') + minutes(0:(length(field_august$Time)-1)),
            temperature = (Temp1 + Temp2 + Temp3 + Temp4) / 4) %>% 
  mutate(time = str_sub(as.character(time), 1L, 13L)) %>% 
  group_by(time) %>% 
  summarise(temperature = mean(temperature)) %>% 
  na.omit() %>% 
  mutate(time = ymd_h(time)) %>% 
  subset(time>=ymd_h('2021-08-02 00') & time<=ymd_h('2021-08-17 01')) ->
  field_august

ggplot(weather_station_august, aes(time, temperature)) +
  geom_line() +
  theme_bw() +
  geom_line(data = field_august, aes(time, temperature), color = 'red')