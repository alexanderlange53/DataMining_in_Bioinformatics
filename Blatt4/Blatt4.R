############################################# Sheet 4 ###############################################
library(ggplot2)
library(rARPACK)
library(reshape2)
library(gridExtra)
library(grid)
library(dplyr)
library(lattice)

source('PlotEigen.R')
source('PlotPCAScat.R')
source('PlotLoadings.R')
source('NormAll.R')
source('IntensData.R')

# Ex. 2
Wpos <- read.csv('wound_pos.csv', header = F)
Wneg <- read.csv('wound_neg.csv', header = F)
TimeVec <- read.csv('TimeVec.csv', header = F)
CondVec <- read.csv('CondVec.csv', header = F)

# Creating labels
A <- rep(c(0.1,0.1,0.2, 0.2,0.3,0.3,0.4,0.4), 4)
Labels <- cbind(t(A), TimeVec[,33:72])

# (a)
# Creating Sample
WposS <- Wpos[,6:77]
WnegS <- Wneg[,6:77]

# Eigenvalues
PlotEigen(WposS)
PlotEigen(WnegS)

# (b)
# Scatterplot of first two PC's 
PlotPCAScat(WposS, Labels)
PlotPCAScat(WnegS, Labels)

# (c)
# Detecting outliers with loadings scatterplot
PlotLoadings(WposS)
PlotLoadings(WnegS)

# Ex. 3
# (a)

# Normalizing data by euklidian, manhattan and standard deviation 
WposNorm <- NormAll(WposS)
WnegNorm <- NormAll(WnegS)

# Eigenvalues
Eiglpos <- lapply(WposNorm, PlotEigen)
marrangeGrob(Eiglpos, nrow = 2, ncol = 3)
Eiglneg <- lapply(WnegNorm, PlotEigen)
marrangeGrob(Eiglneg, nrow = 2, ncol = 3)

# Scatterplot of first two PC's 
Scatplpos <- lapply(WposNorm, PlotPCAScat, Labels = Labels)
marrangeGrob(Scatplpos, nrow = 2, ncol = 3)
Scatplneg <- lapply(WnegNorm, PlotPCAScat, Labels = Labels)
marrangeGrob(Scatplneg, nrow = 2, ncol = 3)

# Loadings Plot
Loadlpos <- lapply(WposNorm, PlotLoadings)
marrangeGrob(Loadlpos, nrow = 2, ncol = 3)
Loadlneg <- lapply(WnegNorm, PlotLoadings)
marrangeGrob(Loadlneg, nrow = 2, ncol = 3)

# (b)
# Visalization of intensity 
IntensityPos <- IntensData(WposS, 'euk')
levelplot(IntensityPos)

IntensityNeg <- IntensData(WnegS, 'euk')
levelplot(IntensityNeg)
