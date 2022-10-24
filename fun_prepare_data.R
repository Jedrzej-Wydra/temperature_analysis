fun_prepare_data <- function(location, month) {
  library(readr)
  library(lubridate)
  library(stringr)
  library(dplyr)
  library(ggplot2)
  
  if (month == "August")
  {
    date1 <- '2021-08-02 01'
    date2 <- '2021-08-02 00'
    date3 <- '2021-08-17 01'
  }
  if (month == "December")
  {
    date1 <- '2021-12-05 01'
    date2 <- '2021-12-05 00'
    date3 <- '2021-12-20 01'
  } 
  
  if (location == "weather_station") {return()}
  if (!(month %in% c("August", "December"))) {return(print("Invalid value of argument >>month<<"))}
  if (!(location %in% c("attic",
                        "basement",
                        "field",
                        "forest",
                        "garage",
                        "roof",           
                        "shack",
                        "underground",
                        "unhabited_bulding"))) {return(print("Invalid value of argument >>location<<"))}
  
  path <- paste0('datasets/', month, '/', location, '/')
  
  files <- list.files(path)
  path_to_file <- paste0(path, files)
  
  file_1 <- read_csv(path_to_file[1],skip = 1)
  file_2 <- read_csv(path_to_file[2],skip = 1)
  
  names(file_1) <- c('nr', 'Time', 'Temp1', 'Temp2', 'col1', 'col2')
  names(file_2) <- c('nr1', 'Time1', 'Temp3', 'Temp4', 'col11', 'col21')
  
  table <- cbind(file_1, file_2)
  
  remove(file_1, file_2)
  
  table %>%
    transmute(time = ymd_h(date1) + minutes(0:(length(table$Time)-1)),
              temperature = (Temp1 + Temp2 + Temp3 + Temp4) / 4) %>% 
    mutate(time = str_sub(as.character(time), 1L, 13L)) %>% 
    group_by(time) %>% 
    summarise(temperature = mean(temperature)) %>% 
    na.omit() %>% 
    mutate(time = ymd_h(time)) %>% 
    subset(time>=ymd_h(date2) & time<=ymd_h(date3)) ->
    table
  
  return(table)
}