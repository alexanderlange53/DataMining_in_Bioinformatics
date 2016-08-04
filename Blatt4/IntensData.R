IntensData <- function(x, norm){
  
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
  
  colnames(x) <- Labels
  cn <- colnames(x)
  MeanMat <- sapply(unique(cn), function(g) rowMeans(x[,cn==g,drop=FALSE]))
  x <- as.matrix(x)
  x <- x - mean(x)
  Eig <- svd(x)
  EG <- Eig[[2]]
  EGP <- as.data.frame(EG[,c(1,2)])
  Outlier <- rep(0, times = nrow(EGP))
  Outlier <- ifelse(sqrt(rowSums(EGP*EGP)) > 0.06, '1', '0')
  MeanMat <- cbind(MeanMat, Outlier)
  Candidates <- matrix(as.numeric(subset(MeanMat, Outlier == 1)), nrow = sum(as.numeric(Outlier)))
  Candidates <- Candidates[,-9]
  if(norm == 0){
  Intensity <- t(Candidates)
  }else{
  Intensity <- apply(Candidates, 1, norm)
  }
  return(t(Intensity))
}