GrammMat <- function(D){
  D <- D*D
  R <- rowMeans(D)
  C <- matrix(rep(colMeans(D), times = nrow(D)), nrow = nrow(D), byrow = T)
  M <- mean(D)*matrix(1, nrow = nrow(D), ncol = ncol(D))
  X <- -0.5*(D - R - C + M)
  return(X)
}