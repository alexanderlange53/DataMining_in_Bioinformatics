######################################### Sheet 3 ##################################################
library(ggplot2)
library(rARPACK)

source('PlotEigen.R')
source('PlotCumSum.R')
source('PlotPCAScat.R')
source('GrammMat.R')

# Ex 1
# (a)
Ecoli <- read.csv('EcoliVectors.csv', header = F)
EcoliLab <- read.csv('EcoliLabels.csv', header = F)

# (b)
# Eigenvalue illsutration
PlotEigen(t(Ecoli))
PlotCumSum(t(Ecoli))

# PCA illustartion
EcoliPCA <- eigen(cov(t(Ecoli)))$vectors[,c(1,2)]
EcoliProj <- t(EcoliPCA)%*%as.matrix(Ecoli)
# Data is grouped on intrinsic dimensions
PlotPCAScat(EcoliProj, EcoliLab)

# Same procedure with Bacillus subtilis data set
Bsub <- read.csv('BsubVectors.csv', header = F)
BsubLab <- read.csv('BsubLabels.csv', header = F)
# Eigenvalue illsutration
PlotEigen(t(Bsub))
PlotCumSum(t(Bsub))
# PCA illustartion
BsubPCA <- eigen(cov(t(Bsub)))$vectors[,c(1,2)]
BsubProj <- t(BsubPCA)%*%as.matrix(Bsub)
PlotPCAScat(BsubProj, BsubLab)

# Ex. 2
# Multidimensional scaling
# Distance matrix
D <- as.matrix(dist(t(as.matrix(Ecoli)), method = 'manhattan', diag = T, upper = T))
# Grammsche matrix
G <- GrammMat(D)
EG <- eigs(G, 2)$vectors
Z <- D%*%EG
# Illustration of m. scaling
# Eigenvectors of grammsche matrix gives scaled principal components
PlotPCAScat(t(Z), EcoliLab)

# Same with other data set
# Distance matrix
D <- as.matrix(dist(t(as.matrix(Bsub)), method = 'manhattan', diag = T, upper = T))
# Grammsche matrix
G <- GrammMat(D)
EG <- eigs(G, 2)$vectors
Z2 <- D%*%EG
# Illustration of m. scaling
PlotPCAScat(t(Z2), BsubLab)
