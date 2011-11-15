\name{Rmomentn}
\alias{Rmomentn}
\title{Function to calculate net reproduction and its higher moments.}
\description{A simple function, written mostly to save space, since it is alled multiple times from other functions in the \code{Lotka} package}
\usage{Rmomentn(fx, Lx, x, n = 0)}
\arguments{
  \item{fx}{vector of fertility rates. Usually daughters born to mothers.}
  \item{Lx}{vector of lifetable exposures, L(x), with a l0 radix of 1.}
  \item{x}{vector of ages. e.g. 0:110 or alpha:beta, where alpha is the youngest age at reproduction and beta is the oldest age at reproduction.}
  \item{n}{the moment. 0 (default) = net reproduction, 1= 1st moment, and so forth. Formulas rarely call for moments higher than 2.}
}
\details{Simply uses the formula:\code{sum((x^n)*fx*Lx)}. \code{x}, \code{fx} and \code{Lx} must be of equal length.}
\value{the reproductive value at moment, n. If n is 0 (default), then th value is to be interpreted as net reproduction. In this case, a value of 1 corresponds with an r of 0, <1 with r<0 and >1 with r>0.}
\references{iter alia: Lotka, Alfred J (1922) The Stability of the Normal Age Distribution. PNAS. Vol. 8, No. 11, pp. 339-345.}
\author{Tim Riffe}

\seealso{called by: \code{\link{LotkaRCoale}},  \code{\link{LotkaRAnalytic}}, \code{\link{MeanGeneration}}, \code{\link{R0Decomp}}, \code{\link{R0Kitagawa}}, \code{\link{rDecomp}}}

\examples{
library(Lotka)
data(LotkaData)
head(LotkaData)
# for 1998
fx <- (LotkaData[,"Bx1"]*LotkaData[,"px1"])/LotkaData[,"Nx1"]
Lx <- LotkaData[,"Lx1"]
x <- LotkaData[,"Age"]
Rmomentn(fx,Lx,x,0) # net reproduction, 0th moment
# first and second moments are used in Lotka's analytic solution for r
Rmomentn(fx,Lx,x,1) # first moment
Rmomentn(fx,Lx,x,2) # second moment
# for 1975
fx <- (LotkaData[,"Bx2"]*LotkaData[,"px2"])/LotkaData[,"Nx2"]
Lx <- LotkaData[,"Lx2"]
x <- LotkaData[,"Age"]
Rmomentn(fx,Lx,x,0) # net reproduction, 0th moment
Rmomentn(fx,Lx,x,1) # first moment
Rmomentn(fx,Lx,x,2) # second moment

## The function is currently defined as
function(fx,Lx,x,n=0){
	sum((x^n)*fx*Lx)
  }
}

