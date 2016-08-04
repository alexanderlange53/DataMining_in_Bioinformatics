Rotation <- function(alpha, Data){
  alpha <- ((2*pi)/360)*alpha
  RotMat <- matrix(c(cos(alpha), sin(alpha), -sin(alpha), cos(alpha)), ncol = 2, byrow = T)
  X <- RotMat%*%Data
  V <- var(X[1,])
  Out <- cbind(V, X)
  return(Out)
}