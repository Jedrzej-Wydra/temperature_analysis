#distance between at least two observation is greater than or equal to d

bootstrap_prepare <- function(month, dataset, n = dim(dataset[1]), d = 1) {
  
  if (n == dim(dataset)[1]) return(dataset)
  if (n < 2) return(print("Argument >>n<< is to small"))
  
  samp <- 1:(dim(dataset)[1] - (d - 1))
  
  if (month == "August")
  {
    samp <- setdiff(samp, which(is.na(weather_station_august$temperature)))
    samp <- setdiff(samp,(66-d+1):66)
  }
  
  if (month == "December")
  {
    samp <- setdiff(samp, which(is.na(weather_station_december$temperature)))
    samp <- setdiff(samp,c((242-d+1):242,336-d+1,346-d+1))
  }
  
  l1 <- sample(samp,1)
  l2 <- l1 + (d - 1)
  
  ln <- sample((l1+1):(l2-1), n-2)
  
  df <- dataset[c(l1,ln,l2),]
  
  bootstrap_sample <- dplyr:: left_join(data.frame(time = dataset$time), df)
  
  return(bootstrap_sample)
}