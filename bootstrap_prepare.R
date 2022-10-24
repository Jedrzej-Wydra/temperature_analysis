#distance between at least two observation is greater than or equal to d

bootstrap_prepare <- function(dataset, n = dim(dataset[1]), d = 0) {
  
  if (n == dim(dataset)[1]) return(dataset)
  if (n < 2) return(print("Argument >>n<< is to small"))
  
  df <- dataset[1:(dim(dataset)[1] - d),]
  
  random_df <- df[sample(1:(dim(dataset)[1]), n-1),]
  last_observation <- random_df$time[length(random_df$time)] + hours(d)
  index <- match(last_observation, dataset$time)
  
  data <- rbind(random_df, dataset[index,])
  
  bootstrap_sample <- dplyr:: left_join(data.frame(time = dataset$time), data)
  
  return(bootstrap_sample)
}