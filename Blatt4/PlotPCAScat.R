PlotPCAScat <- function(Data, Labels){
  x <- as.matrix(Data)
  x <- x - mean(x)
  Eig <- svd(x)
  EG <- Eig[[2]]
  PC <- t(EG[,c(1,2)])%*%as.matrix(Data)
  X <- as.data.frame(cbind(t(PC), t(Labels)))
  X$V3 <- as.factor(X$V3)
  ggplot(X, aes(x = V1, y = V2, fill = V3, shape = V3), alpha = 1) + geom_point(size = 3.5) + theme_bw(12) +
    scale_fill_manual(values = c("#00800080", "#0000A080", "#FF000080",'black', 
                                  "#00800080", "#0000A080", "#FF000080", 'black'), 
                       labels = c('1 Unwounded', '1 Harvested 0.5','1 Harvested 2', '1 Harvested 5',
                                 '2 Unwounded', '2 Harvested 0.5','2 Harvested 2', '2 Harvested 5')) +
    scale_shape_manual(values = c(21,21,21,21,24,24,24,24),
                       labels = c('1 Unwounded', '1 Harvested 0.5','1 Harvested 2', '1 Harvested 5',
                                  '2 Unwounded', '2 Harvested 0.5','2 Harvested 2', '2 Harvested 5')) +
    labs(x = 'z1', y = 'z2', fill = 'Genes', shape = 'Genes')
}