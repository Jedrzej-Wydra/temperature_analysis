START <- Sys.time()

library(readr)

step = 1

for (month in c("August", "December"))
{
  
  bootstrap_dataset <- NULL
  
  number_of_observation <- 4:12
  
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
    
    for (k in number_of_observation)
    {
      for (l in 1:1000) # change to 1 000
      {
        message(paste(step,"of",162000))
        
        results <- fun_linear_model_cheb(data_loc, i, month, k)
        bootstrap_dataset <- rbind(bootstrap_dataset,
                                    data.frame(location = i,
                                              error = results[1],
                                              Best_error = results[2],
                                              Worst_error  = results[3],
                                              number = k))
          
        step <- step + 1
       }
      
   }
    
}
  
 rownames(bootstrap_dataset) <- NULL
 
write.csv(bootstrap_dataset, paste0('bootstrap_',tolower(month),'_cheb','.csv'))
  
}

(duration <- Sys.time() - START)

