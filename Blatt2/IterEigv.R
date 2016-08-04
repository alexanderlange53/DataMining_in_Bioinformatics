IterEigv <- function(CovM, StartV, m){
  v <- matrix(0, nrow = nrow(CovM), ncol = m)
  v[,1] <- StartV
  w <- 0
  
  for(i in 2:m){
  w <- CovM%*%v[,i-1]
  v[,i] <- w/(sqrt(sum(w^2)))
  }
  return(v)
}