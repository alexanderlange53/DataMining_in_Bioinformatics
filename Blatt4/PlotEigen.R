PlotEigen <- function(x){
  x <- as.matrix(x)
  x <- x - mean(x)
  Eig <- svd(x)
  EigVal <- as.data.frame(c(Eig[[1]]))
  ggplot(EigVal, aes(x = 1:nrow(EigVal), y = Eig[[1]])) + theme_bw(12) + labs(x = NULL, y = 'Eigenvaule') +
    geom_bar(stat="identity", position=position_dodge(), col = 'black', alpha = .5, fill = 'darkblue')
}