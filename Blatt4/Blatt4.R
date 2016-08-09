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
# contains mass spectrometry data of metabolite profiling from a plant called arabidopsis thaliana 
# coloumns represent the observations:
# -2 different plant types:
#  + wild type
#  + JA-deficit mutant plant dde 2-2
# -4 different groups in each typ plant:
#  + unwounded
#  + harvested half hour after wounding
#  + harvested 2 hours after wounding
#  + harvested 5 hours after wounding
# -9 observations for each group --> 72 obs total
# rows represent the dimensions:
# -different marker (metabolie candidates) --> 837 different genes in total
# We try to find the genes with the highest metabolic intensity 

# Eigenvalues
# Calculated by SVD decomposition, because the data has more dimensions than observations
# --> only 72 eigenvalues claculated from 837 dimensions
PlotEigen(WposS)
PlotEigen(WnegS)

# (b)
# Scatterplot of first two PC's
# No groups visible in both samples
PlotPCAScat(WposS, Labels)
PlotPCAScat(WnegS, Labels)

# (c)
# Detecting outliers with loadings scatterplot
# Green triangles shows the metabolits which have the most influence of the eigenvectors
PlotLoadings(WposS)
PlotLoadings(WnegS)

# Ex. 3
# (a)

# Normalizing data by euklidian, manhattan and standard deviation 
# In geeneral: normalization is usefull to bring all obs to the same scale, because
# otherwise the PCA can be biased. (overestimation of variables on a large scale)
# Row wise: Normalizing over the different groups and observations
# Coloumn wise: Normalizing over the different metabolits
WposNorm <- NormAll(WposS)
WnegNorm <- NormAll(WnegS)

# Eigenvalues
# For all plots hold:
# - first row -> row wise normalization
# - second row -> colum wise normalization
# - first column -> euklidian
# - second column -> manhattan
# - third column -> standard deviation
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
# Normalizing over the metabolits is more efficient to detect outliers in loadingsplot
# --> most variance in between metabolits
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
