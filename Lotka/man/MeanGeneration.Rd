\name{MeanGeneration}
\alias{MeanGeneration}
\title{Calculates the mean generation time, given fx and Lx.}
\description{This function uses the following equation to find T, the mean generation time: \code{(R1/R0)+r*.5*(((R1/R0)^2)-(R2/R0))}, where \code{r} is Lotka's r, solved using \code{LotkaRCoale()}, and \code{R0}, \code{R1} and \code{R2} and the 0th, 1st and 2nd moments, respectively of the renewal equation.}
\usage{MeanGeneration(fx, Lx, x)}
\arguments{
  \item{fx}{vector of fertility rates. Usually daughters born to mothers.}
  \item{Lx}{vector of lifetable exposures, L(x), with a l0 radix of 1.}
  \item{x}{vector of ages. e.g. 0:110 or alpha:beta, where alpha is the youngest age at reproduction and beta is the oldest age at reproduction.}
  }
\details{The same value can be extracted from the LotkaRCoale iteration, but this is yet another formula.}
\value{T, the mean generation time}
\references{Kuczynski Robert R. (1932) Fertility and Reproduction. Falcon Press. New York.}
\author{Tim Riffe}
\note{
There are several ways of finding this value....
}

\seealso{\code{\link{Rmomentn}}, \code{\link{LotkaRCoale}}}
\examples{
library(Lotka)
data(LotkaData)
head(LotkaData)
# first with the 1998 data
fx <- (LotkaData[,"Bx1"]*LotkaData[,"px1"])/LotkaData[,"Nx1"]
Lx <- LotkaData[,"Lx1"]
x <- LotkaData[,"Age"]
# 
(MG1 <- MeanGeneration(fx,Lx,x))
(R01 <- sum(fx*Lx)) # the corresponding R0 estimate
(r1 <- log(R01)/MG1) # and another way to get at r
1-sum(exp(-r1*x)*fx*Lx) # and it's about the same residual as for Lotka's analytic solution

### another way to get at mean generation, perhaps more precise:
r1 <- LotkaRCoale(fx,Lx,x)
log(R01)/r1 # this is also a good estimate
## The function is currently defined as
function(fx,Lx,x){
	r <- LotkaRCoale(fx,Lx,x)
	R0 <- Rmomentn(fx,Lx,x,0)
	R1 <- Rmomentn(fx,Lx,x,1)
	R2 <- Rmomentn(fx,Lx,x,2)
	(R1/R0)+r*.5*(((R1/R0)^2)-(R2/R0))
  }
}


