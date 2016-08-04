PlotCumSum <- function(x){
  covMat <- cov(x)
  Eig <- eigen(covMat)
  EigVal <- as.data.frame(Eig[[1]])
  EigCum <- as.data.frame(cumsum(EigVal)/sum(EigVal))
  names(EigCum) <- 'CumS'
  ggplot(EigCum, aes(x = 1:nrow(EigCum), y = CumS)) + geom_line(size = 1.5) + theme_bw(15) +
    geom_hline(yintercept = c(0.7, 0.9), color = 'darkblue') + labs(x = 'Eigenvalues', y = 'Variance share')
    
}