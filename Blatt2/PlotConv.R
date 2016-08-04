PlotConv <- function(IEigV, XdataMat){

 It <- as.data.frame(cbind(rep(0, times = ncol(IEigV)), rep(0, times = ncol(IEigV)), t(IEigV)))
 Xdat <- as.data.frame(t(XdataMat - mean(XdataMat)))
 ggplot(Xdat, aes(x = V1, y = V2)) + 
  geom_point() + labs(x = 'x1', y = 'x2', title = 'Convergence to eigenvector') + 
  theme_bw(15) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_segment(data = It, aes(x = V1, y = V2, xend = V3, yend = V4), 
               arrow = arrow(length = unit(0.25,"cm")), color = 'darkblue')
}