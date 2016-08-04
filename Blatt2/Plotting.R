Plotting <- function(Data1, Data2, Data3, Data4){
  Data1 <- as.data.frame(t(Data1))
  Data2 <- as.data.frame(t(Data2))
  Data3 <- as.data.frame(t(Data3))
  Data4 <- as.data.frame(t(Data4))
  Data1$Stage <- 'Original Data'
  Data2$Stage <- 'Centered'
  Data3$Stage <- 'Rotated with maximum Var'
  Data4$Stage <- 'Rotated with minimum Var'
  Data <- rbind(Data1, Data2, Data3, Data4)
  Group <- rep(c(1,1,1,1, rep(0, times = (nrow(Data1)-4))), times = 4)
  Data <- cbind(Data, Group)
  Data$Group <- as.factor(Data$Group)
  Data$Stage <- factor(Data$Stage, levels = c('Original Data', 'Centered', 'Rotated with maximum Var',
                                              'Rotated with minimum Var'))
  ggplot(Data, aes(x = Data$V1, y = Data$V2, fill = Group, shape = Group, size = Group)) + geom_point() + labs(x = 'x1', y = 'x2') + coord_equal() +
    theme_bw(15) + theme(legend.position = 'none') + scale_fill_manual(values = c('black', 'blue')) +
    scale_shape_manual(values = c(21, 24)) + scale_size_manual(values = c(2, 4)) +
    facet_wrap(~ Stage)
}