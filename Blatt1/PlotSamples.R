PlotSamples <- function(Output){
  Output <- as.data.frame(t(Output))
  Output$seq <- seq(1, 360)
  names(Output) <- c('100', '1000', '10000', '100000', 'id')
  Output1 <- melt(Output, id = 'id')
  ggplot(Output1, aes(x = id, y = value, group = variable, color = variable)) + geom_line(size = 2) +
    labs(y = 'Variance', x = 'Rotation angle') + theme_bw(12) +  geom_hline(yintercept = 1/12, color = 'darkblue') +
    scale_color_manual(values = gray.colors(5), name = 'Sample size') +
    theme(legend.position = 'right')
}