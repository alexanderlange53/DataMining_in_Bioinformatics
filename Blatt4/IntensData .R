IntensData <- function(x, norm){
  
  euk <- function(y){
    y/sqrt(sum(y^2))
  }
  manhattan <- function(y){
    y/sum(abs(y))
  }
  st.error <- function(y){
    y/sqrt(var(y))
  }
  
  colnames(x) <- Labels
  cn <- colnames(x)
  MeanMat <- sapply(unique(cn), function(g) rowMeans(x[,cn==g,drop=FALSE]))
  CovM <- cov(t(x))
  EGP <- eigs(CovM, 2)$vectors
  EGP <- as.data.frame(EGP)
  Outlier <- rep(0, times = nrow(EGP))
  Outlier <- ifelse(sqrt(rowSums(EGP*EGP)) > 0.06, '1', '0')
  MeanMat <- cbind(MeanMat, Outlier)
  Candidates <- matrix(as.numeric(subset(MeanMat, Outlier == 1)), nrow = 10)
  Candidates <- Candidates[,-9]
  Intensity <- apply(Candidates, 1, norm)
  return(Intensity)
}