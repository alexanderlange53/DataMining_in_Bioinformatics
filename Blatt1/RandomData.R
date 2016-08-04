RandomData <- function(nDims, nVecs){
  X <- matrix(runif(nDims*nVecs), ncol = nVecs)
  return(X)
}