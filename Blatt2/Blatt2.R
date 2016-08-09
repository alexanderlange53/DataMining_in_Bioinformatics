###################################### Sheet 2 ##################################################
library(ggplot2)
library(ggfortify)
library(reshape2)
library(colorspace)
library(GGally)

# Aufgabe 1
# (a)
source('RandomData.R')
source('RotationVec.R')
source('PlotSamples.R')
source('IterEigv.R')
source('PlotConv.R')
source('PlotEigen.R')
source('PowerMethod.R')

# Generatign random data
TransMat <- matrix(c(0.25, 1.299, -0.433, 0.75), nrow = 2, byrow = T)
# multiplying random data with deterministic matrix
XdataMat <- TransMat%*%RandomData(2,100)

# Centering Data
XdataMatCent <- XdataMat - mean(XdataMat)

# Calculation of covariance matrix
CovMat <- cov(t(XdataMat))

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
  XdataMat <- TransMat%*%RandomData(2,i)
  CovMat <- cov(t(XdataMat))
  CovMatR <- sapply(alphaAngle, RotationVec, CovMat)
  Output[count,] <- CovMatR
  count <- count +1
}

# Illustration of rotation angle and sample sizes
# the variance does not converge, due to the deterministic term
PlotSamples(Output)

# (b)

# generating data and cov matrix
XdataMat <- RandomData(2,100)
CovMat <- cov(t(XdataMat))

# Iterative computation of eigenvectors
start <- c(1,0.7)
m <- 20
IEigV <- IterEigv(CovMat, start, m)

# Illustration of Convergence
# multiplying any arbitray vector to the covariance matrix turns the vector into the direction of the
# biggest variance
PlotConv(IEigV, XdataMat)

# (d)
# Comparison of R implemented eigenvactor calculation
# and own implementation of iterative power method
eigen(cov(t(XdataMat)))
PowerMethod(XdataMat, 2)

# Aufgabe 2

Hidden1 <- as.data.frame(t(read.csv('Hidden1.csv', header = F)))
Hidden2 <- as.data.frame(t(read.csv('Hidden2.csv', header = F)))

# (a)
# Density of variables in Hidden1
# every dimension is asymptoticaly normal
H1 <- melt(Hidden1)
ggplot(H1, aes(H1$value)) + 
  geom_histogram(breaks=seq(min(H1$value), max(H1$value), by =10), 
                 col="black", 
                 aes(fill=..count..)) + theme_bw(15) +
  facet_wrap(~ variable)

# Density of variables in Hidden2
# every dimension is asymptoticaly normal
H2 <- melt(Hidden2)
ggplot(H2, aes(H2$value)) + 
  geom_histogram(breaks=seq(min(H1$value), max(H1$value), by =10), 
                 col="black", 
                 aes(fill=..count..)) + theme_bw(15) +
  facet_wrap(~ variable)

# (b)
# Pairwise scatterplot
ggpairs(Hidden1)

# (c)
# Eigenvalues 
# intrinsic dimensionality is probably 2
PlotEigen(Hidden1)
PlotEigen(Hidden2)

# (d)

# Even if every dimension is normally distributed, there might be hidden structures, that are definitly not 
# normal
PCA1 <- c(eigen(cov(Hidden1))[[2]][,1])%*%as.matrix(t(Hidden1))
PCA2 <- c(eigen(cov(Hidden1))[[2]][,2])%*%as.matrix(t(Hidden1))
PCA <- as.data.frame(t(rbind(PCA1, PCA2)))
ggplot(PCA, aes(x = V1, y = V2)) + geom_point() + theme_bw(15) + labs(x = 'z1', y = 'z2')

PCA1 <- c(eigen(cov(Hidden2))[[2]][,1])%*%as.matrix(t(Hidden2))
PCA2 <- c(eigen(cov(Hidden2))[[2]][,2])%*%as.matrix(t(Hidden2))
PCA <- as.data.frame(t(rbind(PCA1, PCA2)))
ggplot(PCA, aes(x = V1, y = V2)) + geom_point() + theme_bw(15) + labs(x = 'z1', y = 'z2')
