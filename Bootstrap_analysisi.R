START <- Sys.time()

library(readr)

step = 1

bootstrap_dataset <- NULL

distances <- c(1, 2, 6, 12, 24, 48, 96, 144)

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

for (i in locations)
{
  for (j in distances)
  {
    for (k in number_of_observation)
    {
      for (l in 1:10000)
      {
        print(paste(step,"of",7920000))
        
        results <- fun_linear_model(i, "August", k, j)
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

rownames(bootstrap_dataset) <- NULL

Sys.time() - START