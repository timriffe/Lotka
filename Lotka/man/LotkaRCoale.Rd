\name{LotkaRCoale}
\alias{LotkaRCoale}
\title{Coale's iterative solution to finding the intrinsic growth rate, r}
\description{The Lotka equation needs to be solved for r. This can be done either analytically or iteratively. This function solves it iteratively using Coale's 1957 proposed procedure.}
\usage{LotkaRCoale(fx, Lx, x)}

\arguments{
  \item{fx}{vector of fertility rates. Usually daughters born to mothers.}
  \item{Lx}{vector of lifetable exposures, L(x), with a l0 radix of 1.}
  \item{x}{vector of ages. e.g. 0:110 or alpha:beta, where alpha is the youngest age at reproduction and beta is the oldest age at reproduction.}
  }
\details{You can check the r calculated by plugging it into the discrete renewal equation (1 = sum(exp(r*t)*fx*Lx). The analytic solution might be off the lark in about the 5th decimal place.}
\value{r, the intrinsic growth rate of the stable population corresponding with the input fertility and survival vectors.}

\references{Coale, Ansley J. (1957) A New Method for Calculating Lotka's r- the Intrinsic Rate of Growth in a Stable Population. Population Studies, Vol. 11 no. 1, pp 92-94.

Dublin, Louis I. and Lotka, Alfred J. (1925) On the True Rate of Natural Increase. Population Index. VOl. 21, no. 151.}
\author{Tim Riffe}
\note{Possibly the most efficient of the 3 r-solving functions included in this package.}

\seealso{\code{\link{LotkaRAnalytic}},\code{\link{LotkaRoptim}}}
\examples{
library(Lotka)
data(LotkaData)
head(LotkaData)
# first with the 1998 data
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


## The function is currently defined as
function(fx,Lx,x){
	# from Coale, Ansley J. (1957) A New Method for Calculating Lotka's r- the Intrinsic Rate of Growth in a Stable Population.
	# Population Studies, Vol. 11 no. 1, pp 92-94
	R0 <- Rmomentn(fx,Lx,x,0)
	# first assuming a mean generation time of 29
	ri <- log(R0)/29
	for (i in 1:15){ # 10 is more than enough!
		deltai <- sum(exp(-ri*x)*fx*Lx)-1
		# the mean generation time self-corrects 
		# according to the error produced by the Lotka equation
		ri <- ri+(deltai/(29-(deltai/ri)))
	}
	return(ri)	
  }
}

