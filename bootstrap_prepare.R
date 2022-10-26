#distance between at least two observation is greater than or equal to d

bootstrap_prepare <- function(dataset, n = dim(dataset[1]), d = 2) {
  
#  if (n == dim(dataset)[1]) return(dataset)
#  if (n < 2) return(print("Argument >>n<< is to small"))
  
  samp <- 1:(dim(dataset)[1] - (d - 1))
  
  l1 <- sample(samp,1)
  l2 <- l1 + (d - 1)
  
  vect <- setdiff(l1:l2, c(l1,l2))
  
  if (length(vect) < n-2) {ln <- vect}
  else {ln <- sample(vect, n-2)}
  
  df <- dataset[c(l1,ln,l2),]
  
  bootstrap_sample <- dplyr:: left_join(data.frame(time = dataset$time), df)
  
  return(bootstrap_sample)
}