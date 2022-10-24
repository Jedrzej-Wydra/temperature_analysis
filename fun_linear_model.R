fun_linear_model <- function(location, month, summary = FALSE) {
  
  if (month == "August")
  {
    weather_station <- read_csv("datasets/August/weather_station/weather_station.csv")
  }
  if (month == "December")
  {
    weather_station <- read_csv("datasets/December/weather_station/weather_station.csv")
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
  
  dataset <- fun_prepare_data(location, month)
  
  model <- lm(dataset$temperature ~ weather_station$temperature)
  
  error_min <- sum((dataset$temperature -
                      model$coef[2]*weather_station$temperature -
                      model$coef[1])^2,
                   na.rm = T)
  
  error_max <- sum((dataset$temperature -
                      weather_station$temperature)^2,
                   na.rm = T)
  
  if (summary) {return(list(Summary = summary(model), Min_error = error_min, Max_error = error_max))}
  else {return(c(Min_error = error_min, Max_error = error_max))}
}