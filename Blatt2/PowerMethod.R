PowerMethod <- function(XdataMat, nEvs){
  CovM <- cov(t(XdataMat))
  v <- 0
  eps <- 0.001
  itmax <- 100
  count <- 1
  ops <- 1
  
  EigVec <- matrix(0, nrow = nrow(CovM), ncol = nEvs)
  EigVal <- rep(0, times = nEvs)
  
  for(i in 1:nEvs){
    v <- c(runif(nrow(CovM)))
    count <- 1
    ops <- 1
  while((count < itmax) | (ops > eps)){
    w <- CovM%*%v
    w <- w/(sqrt(sum(w^2)))
    ops <- max(abs(v - w))
    v <- w
    count <- count +1
  }
  EigVec[,i] <- v
  EigVal[i] <- t(v)%*%CovM%*%v
  
  CovM <- CovM - EigVal[i]*EigVec[,i]%*%t(EigVec[,i])
  }
  
  output <- list(EigVec, EigVal)
  return(output)
}