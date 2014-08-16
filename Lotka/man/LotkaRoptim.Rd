\name{LotkaRoptim}
\alias{LotkaRoptim}
\title{solving for r using the R base function \code{optimize}.}
\description{Find r by minimizing the sum of the squared residual (difference between value of sum(exp(r*x)*fx*Lx) and 1. Tolerance is set extremely low, such that if you plug the solved r back into the equation, it will equal 1.}
\usage{LotkaRoptim(fx, Lx, x)}
\arguments{
  \item{fx}{vector of fertility rates. Usually daughters born to mothers.}
  \item{Lx}{vector of lifetable exposures, L(x), with a l0 radix of 1.}
  \item{x}{vector of ages. e.g. 0:110 or alpha:beta, where alpha is the youngest age at reproduction and beta is the oldest age at reproduction.}
  }
\details{This function ought to always converge...}
\value{r, the intrinsic growth rate of the stable population corresponding with the input fertility and survival vectors.}
\references{Dublin, Louis I. and Lotka, Alfred J. (1925) On the True Rate of Natural Increase. Population Index. VOl. 21, no. 151.}
\author{Tim Riffe}
\note{This finds a very exact solution to r, in effect better than the Lotka analytic solution and equal to the Coale 1957 solution.}
\seealso{\code{\link{optimize}}, \code{\link{LotkaRCoale}},\code{\link{LotkaRAnalytic}}}
\examples{
library(Lotka)
data(LotkaData)
head(LotkaData)
# first with the 1998 data
fx <- (LotkaData[,"Bx1"]*LotkaData[,"px1"])/LotkaData[,"Nx1"]
Lx <- LotkaData[,"Lx1"]
x <- LotkaData[,"Age"]
# 
(r <- LotkaRoptim(fx,Lx,x))
# plug back into renewal equation to see how close r is:
1-sum(exp(-r*x)*Lx*fx) # (better than analytic, not as close as Coale)

# now with the 1975 data
fx <- (LotkaData[,"Bx2"]*LotkaData[,"px2"])/LotkaData[,"Nx2"]
Lx <- LotkaData[,"Lx2"]
x <- LotkaData[,"Age"]
# 
(r <- LotkaRoptim(fx,Lx,x))
# plug back into renewal equation to see how close r is:
1-sum(exp(-r*x)*Lx*fx) 

## The function is currently defined as
function(fx,Lx,x){
	findR <- function(r){
		# this is the Lotka equation, with the 1 moved over to the right side!
		(1-sum(exp(-r*x)*fx*Lx))^2
	}
	optimize(f=findR,seq(-1,1,.0001),tol=.00000000001)$minimum
  }
}

