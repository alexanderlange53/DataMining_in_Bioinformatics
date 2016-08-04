NormAll <- function(x){
  euk <- function(y){
   z <- y/sqrt(sum(y^2))
    if(any(is.nan(z))| any(z == Inf)){
      z <- rep(0, length(y))
    }
   return(z)
  }
  manhattan <- function(y){
   z <- y/sum(abs(y))
   if(any(is.nan(z))| any(z == Inf)){
     z <- rep(0, length(y))
   }
   return(z)
  }
  st.error <- function(y){
   z <- y/sd(y)
   if(any(is.nan(z)) | any(z == Inf)){
     z <- rep(0, length(y))
   }
   return(z)
  }
  RE <- apply(x, 1,euk)
  RM <- apply(x, 1, manhattan)
  RS <- apply(x, 1, st.error)
  CE <- apply(x, 2,euk)
  CM <- apply(x, 2, manhattan)
  CS <- apply(x, 2, st.error)
  output <- list(
    Row.Euk = t(RE),
    Row.Manhattan  = t(RM),
    Row.Er  =t(RS)
    ,Col.Euk = CE,
    Col.Manhattan  = CM,
    Col.Er  = CS
  )
  return(output)
}