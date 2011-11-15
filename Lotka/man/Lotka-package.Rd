\name{Lotka-package}
\alias{Lotka-package}
\alias{Lotka}
\docType{package}
\title{a set of functions for Lotka's r, R0, and T.}
\description{
Functions for net reproduction (and higher moments), estimating Lotka's r, mean generation time, as well as two functions to decompose differences in R0, with summary and plot methods.}
\details{
\tabular{ll}{
Package: \tab Lotka\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2011-03-07\cr
License: \tab General Public\cr
LazyLoad: \tab yes\cr
}
}
\author{
Tim Riffe\\

Maintainer: Tim Riffe <tim.riffe@gmail.com>

}
\references{
Coale, Ansley J. (1957) A New Method for Calculating Lotka's r- the Intrinsic Rate of Growth in a Stable Population. Population Studies, Vol. 11 no. 1, pp 92-94.

Dublin, Louis I. and Lotka, Alfred J. (1925) On the True Rate of Natural Increase. Population Index. VOl. 21, no. 151.

Kitagawa, E.M. (1955) Components of a difference between two rates. Journal of the American Statistical Association. Vol. 50. No. 272. pp. 1168-1194.

Kuczynski Robert R. (1932) Fertility and Reproduction. Falcon Press. New York.

Lotka, Alfred J (1922) The Stability of the Normal Age Distribution. PNAS. Vol. 8, No. 11, pp. 339-345.
}
\keyword{ package }
\examples{

library(Lotka)
data(LotkaData)
head(LotkaData)

# from R0Decomp:
Dx2 <- LotkaData[,"Dx2"]  
Dx1 <- LotkaData[,"Dx1"]   
# Exposures
Nx2 <- LotkaData[,"Nx2"] 
Nx1 <- LotkaData[,"Nx1"] 
# Births 
Bx2 <- LotkaData[,"Bx2"]
Bx1 <- LotkaData[,"Bx1"]
# Proportion births female
px2 <- LotkaData[,"px2"]
px1 <- LotkaData[,"px1"]

Decomp1 <- R0Decomp(Dx1,Nx1,Bx1,px1,Dx2,Nx2,Bx2,px2)
summary(Decomp1)
plot(Decomp1)

# from LotkaRCoale
fx <- (LotkaData[,"Bx1"]*LotkaData[,"px1"])/LotkaData[,"Nx1"]
Lx <- LotkaData[,"Lx1"]
x <- LotkaData[,"Age"]
# 
(r <- LotkaRCoale(fx,Lx,x))
# plug back into renewal equation to see how close r is:
1-sum(exp(-r*x)*Lx*fx)

# now with the 1975 data
fx <- (LotkaData[,"Bx2"]*LotkaData[,"px2"])/LotkaData[,"Nx2"]
Lx <- LotkaData[,"Lx2"]
x <- LotkaData[,"Age"]
# 
(r <- LotkaRCoale(fx,Lx,x))
# plug back into renewal equation to see how close r is:
1-sum(exp(-r*x)*Lx*fx)
}
