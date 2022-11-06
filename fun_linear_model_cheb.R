fun_linear_model_cheb <- function(df, location, month, n = 0, summary = FALSE) {
  
  #  if (month == "August")
  #  {
  #    weather_station <- read_csv("datasets/August/weather_station/weather_station.csv")
  #    weather_station <- weather_station[146:361,]
  #  }
  #  if (month == "December")
  #  {
  #    weather_station <- read_csv("datasets/December/weather_station/weather_station.csv")
  #    weather_station <- weather_station[1:241,]
  #  } 
  
  #  if (location == "weather_station") {return()}
  #  if (!(month %in% c("August", "December"))) {return(print("Invalid value of argument >>month<<"))}
  #  if (!(location %in% c("attic",
  #                        "basement",
  #                        "field",
  #                        "forest",
  #                        "garage",
  #                        "roof",           
  #                        "shack",
  #                        "underground",
  #                        "unhabited_bulding"))) {return(print("Invalid value of argument >>location<<"))}
  
  #  dataset <- fun_prepare_data(location, month)
  dataset <- df
  
  if (n == 0) {n <- dim(dataset[1])}
  
  dataset_boot <- bootstrap_prepare_cheb_nodes(dataset, n)
  
  model <- lm(dataset_boot$temperature ~ weather_station$temperature)
  
  model_best <- lm(dataset$temperature ~ weather_station$temperature)
  
  error <- sum((dataset$temperature -
                  model$coef[2]*weather_station$temperature -
                  model$coef[1])^2,
               na.rm = T)
  
  error_min <- sum((dataset$temperature -
                      model_best$coef[2]*weather_station$temperature -
                      model_best$coef[1])^2,
                   na.rm = T)
  
  error_max <- sum((dataset$temperature -
                      weather_station$temperature)^2,
                   na.rm = T)
  
  if (summary) {return(list(Summary = summary(model), error = error, Best_error = error_min, Worst_error = error_max))}
  else {return(c(error = error, Best_error = error_min, Worst_error = error_max))}
}