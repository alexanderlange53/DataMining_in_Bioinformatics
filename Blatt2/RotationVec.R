RotationVec <- function(alpha, X){
  alpha <- ((2*pi)/360)*alpha
  v <- c(cos(alpha), sin(alpha))
  out <- t(v)%*%X%*%v
  return(out)
}