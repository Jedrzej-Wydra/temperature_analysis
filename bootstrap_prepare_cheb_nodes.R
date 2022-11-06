bootstrap_prepare_cheb_nodes <- function(dataset, n = dim(dataset[1])) {
  
  samp <- 1:(dim(dataset)[1] - 23)
  
  l1 <- sample(samp,1)
  
  vect <- round(0.5*(l1+l1+23)+0.5*23*cos(pi*(2*(1:n)-1)/(2*n)))
  
  df <- dataset[vect,]
  
  bootstrap_sample <- dplyr:: left_join(data.frame(time = dataset$time), df)
  
  return(bootstrap_sample)
}