###################################### Sheet 1 ##################################################
library(ggplot2)
library(ggfortify)
library(reshape2)

# Ex. 1
# Calculation of Eigenvalues and Eigenvectors
A <- matrix(c(3, -2, -4, 1), ncol = 2, byrow = T)
E <- eigen(A)
EigenValues <- E$values
EigenVectors <- E$vectors

# Ex. 2
source('RandomData.R')
source('Rotation.R')
source('Plotting.R')
source('PlotSamples.R')
source('RotationVec.R')

# Generating 2D random data set from uniform distribution
XdataMat <- RandomData(2,100)
# Centering data
XdataMatCent <- XdataMat - mean(XdataMat)

# Rotation of data with orthogonal rotation matrix
alphaAngle <- c(seq(1,360))
XdataMatCentR <- sapply(alphaAngle, Rotation, XdataMatCent)

# Correlation of rotation angle and variance in x1 direction
# blue line shows the 'theoretical' Variance of a uniform distribution U[0,1]
autoplot(ts(XdataMatCentR[1,])) + theme_bw(12) + geom_hline(yintercept = (1/12), color = 'darkblue')

# Finding rotated set mith maximum variance
XdataMatCentRMax <- XdataMatCentR[, which.max(XdataMatCentR[1,])]
XdataMatCentRMax <- matrix(XdataMatCentRMax[3:length(XdataMatCentRMax)], nrow = 2, byrow = T)

# Finding rotated set mith minimum variance
XdataMatCentRMin <- XdataMatCentR[, which.min(XdataMatCentR[1,])]
XdataMatCentRMin <- matrix(XdataMatCentRMin[3:length(XdataMatCentRMin)], nrow = 2, byrow = T)

# Illustration of the different stages
# blue triangles illustrates 4 randomly selected points from the data set, to show rotation
Plotting(XdataMat, XdataMatCent, XdataMatCentRMax, XdataMatCentRMin)

# Comparing the impact of rotation to the variance for different sample sizes
Samples <- c(100, 1000, 10000, 100000)
Output <- matrix(0, ncol = 360, nrow = 4)
alphaAngle <- c(seq(1,360))
count <- 1
for(i in Samples){
  XdataMat <- RandomData(2,i)
  XdataMatCent <- XdataMat - mean(XdataMat)
  XdataMatCentR <- sapply(alphaAngle, Rotation, XdataMatCent)
  Output[count,] <- XdataMatCentR[1,]
  count <- count +1
}

# Illustration of rotation angle and sample sizes
# With increasing sample size, the effect of rotating the data set vanishes
# by means of central limit theorem and convergence in probability the distribution of the variance 
# collapse and convergence into a single value
PlotSamples(Output)

# Ex. 3

# Generatign random data
XdataMat <- RandomData(2,100)
# Centering Data
XdataMatCent <- XdataMat - mean(XdataMat)

# Calculation of covariance matrix
CovMat <- cov(t(XdataMat))
# own implementation of covariance matrix
(1/(ncol(XdataMat)-nrow(XdataMat)))*(XdataMatCent)%*%t(XdataMatCent)

# rotation of covariance matrix
alphaAngle <- c(seq(1,360))
CovMatR <- sapply(alphaAngle, RotationVec, CovMat)

# Correlation of rotation angle and variance
autoplot(ts(CovMatR)) + theme_bw(12)+ geom_hline(yintercept = (1/12), color = 'darkblue')

# Comparing different sample sizes
Samples <- c(100, 1000, 10000, 100000)
Output <- matrix(0, ncol = 360, nrow = 4)
alphaAngle <- c(seq(1,360))
count <- 1
for(i in Samples){
  XdataMat <- RandomData(2,i)
  CovMat <- cov(t(XdataMat))
  CovMatR <- sapply(alphaAngle, RotationVec, CovMat)
  Output[count,] <- CovMatR
  count <- count +1
}

# Illustration of rotation angle and sample sizes
# This is equivalent to exercise 2 because v'Cv gives the eigenvalues of the covariance matrix,
# which reflects the variance in the projected space
PlotSamples(Output)
