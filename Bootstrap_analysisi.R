START <- Sys.time()

library(readr)

month <- "August"

step = 1

bootstrap_dataset <- NULL

distances <- c(2, 6, 12, 24, 48, 96, 192)

number_of_observation <- 2:12

locations <- c("attic",
               "basement",
               "field",
               "forest",
               "garage",
               "roof",           
               "shack",
               "underground",
               "unhabited_bulding")

  if (month == "August")
  {
    weather_station <- read_csv("datasets/August/weather_station/weather_station.csv")
    weather_station <- weather_station[146:361,]
  }
  if (month == "December")
  {
    weather_station <- read_csv("datasets/December/weather_station/weather_station.csv")
    weather_station <- weather_station[1:241,]
  } 

for (i in locations)
{
  data_loc <- fun_prepare_data(i, month)
  for (j in distances)
  {
    for (k in number_of_observation)
    {
      if (j >= k)
        {
          for (l in 1:1000) # change to 1 000
          {
            print(paste(step,"of",549000))
        
            results <- fun_linear_model(data_loc, i, month, k, j)
            bootstrap_dataset <- rbind(bootstrap_dataset,
                                      data.frame(location = i,
                                                  error = results[1],
                                                  Best_error = results[2],
                                                  Worst_error  = results[3],
                                                  distance = j,
                                                  number = k))
        
            step <- step + 1
          }
        }
    }
  }
}

rownames(bootstrap_dataset) <- NULL

write.csv(bootstrap_dataset, 'bootstrap_august.csv')

Sys.time() - START
