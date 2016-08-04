PlotPCAScat <- function(Data, Labels){
  X <- as.data.frame(cbind(t(Data), t(Labels)))
  X$V3 <- as.factor(X$V3)
  ggplot(X, aes(x = V1, y = V2, color = V3, alpha = V3)) + geom_point(size = 1.8) + theme_bw(15) +
    scale_color_manual(values = c("#00800080", "#0000A080", "#FF000080")) +
    scale_alpha_manual(values = c(0.2, 1, 1)) +
    labs(x = 'z1', y = 'z2', color = 'Genes', alpha = 'Genes')
}