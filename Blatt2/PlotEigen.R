PlotEigen <- function(x){
  covMat <- cov(x)
  Eig <- eigen(covMat)
  EigVal <- as.data.frame(Eig[[1]])
  ggplot(EigVal, aes(x = 1:9, y = Eig[[1]])) + theme_bw(15) + labs(x = NULL, y = 'Eigenvaule') +
    geom_bar(stat="identity", position=position_dodge(), col = 'black', alpha = .5, fill = 'darkblue')
}