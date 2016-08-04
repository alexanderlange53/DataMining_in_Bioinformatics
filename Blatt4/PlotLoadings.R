PlotLoadings <- function(Data){
  x <- as.matrix(Data)
  x <- x - mean(x)
  Eig <- svd(x)
  EG <- Eig[[2]]
EGP <- as.data.frame(EG[,c(1,2)])
EGP$Outlier <- ifelse(sqrt(rowSums(EGP*EGP)) > 0.06, '1', '0')
ggplot(EGP, aes(x = V1, y = V2, fill = Outlier, shape = Outlier)) + geom_point(size = 3) + theme_bw(12) +
  scale_fill_manual(values = c('black', '#00800080')) +
  scale_shape_manual(values =  c(21,24)) + theme(legend.position = 'none') +
  labs(x = 'Eigenvector 1', y = 'Eigenvector 2')
}