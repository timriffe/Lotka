\name{LotkaRanalytic}
\alias{LotkaRanalytic}
\title{Lotka's analytic solution to finding the intrinsic growth rate, r}
\description{The Lotka equation needs to be solved for r. This can be done either analytically or iteratively. This function solves it analytically using Lotka's own formula.}
\usage{LotkaRanalytic(fx, Lx, x)}
\arguments{
  \item{fx}{vector of fertility rates. Usually daughters born to mothers.}
  \item{Lx}{vector of lifetable exposures, L(x), with a l0 radix of 1.}
  \item{x}{vector of ages. e.g. 0:110 or alpha:beta, where alpha is the youngest age at reproduction and beta is the oldest age at reproduction.}
}
\details{You can check the r calculated by plugging it into the discrete renewal equation (1 = sum(exp(r*t)*fx*Lx). The analytic solution might be off the lark in about the 5th decimal place.}
\value{r, the intrinsic growth rate of the stable population corresponding with the input fertility and survival vectors.}
\references{Dublin, Louis I. and Lotka, Alfred J. (1925) On the True Rate of Natural Increase. Population Index. VOl. 21, no. 151}
\author{Tim Riffe}
\note{not quite as precise as the Coale approximation (when iterated enough) or the general optimized r.}
\seealso{\code{\link{LotkaRCoale}},\code{\link{LotkaRoptim}}}
\examples{
library(Lotka)
data(LotkaData)
head(LotkaData)
# first with the 1998 data
fx <- (LotkaData[,"Bx1"]*LotkaData[,"px1"])/LotkaData[,"Nx1"]
Lx <- LotkaData[,"Lx1"]
x <- LotkaData[,"Age"]
# 
(r <- LotkaRanalytic(fx,Lx,x))
# plug back into renewal equation to see how close r is:
1-sum(exp(-r*x)*Lx*fx) # rather close

# now with the 1975 data
fx <- (LotkaData[,"Bx2"]*LotkaData[,"px2"])/LotkaData[,"Nx2"]
Lx <- LotkaData[,"Lx2"]
x <- LotkaData[,"Age"]
# 
(r <- LotkaRanalytic(fx,Lx,x))
# plug back into renewal equation to see how close r is:
1-sum(exp(-r*x)*Lx*fx) # also rather close



## The function is currently defined as
function(fx,Lx,x){
	R0 <- Rmomentn(fx,Lx,x,0)
	R1 <- Rmomentn(fx,Lx,x,1)
	R2 <- Rmomentn(fx,Lx,x,2)
	# alpha would have been:	 R1/R0	
	# Beta would have been:		 alpha^2-(R2/R0)	
	# to save myself confusion, they are altered and 
	# assigned directly to a and b for the cuadratic formula
	b <- R1/R0
	a <- .5*((b^2)-R2/R0)
	c <- -log(R0)
	r <- rep(0,2) # in case R0 = 1...
	# cuadratic formula, 2 solutions:
	r[1] <- (-b+sqrt((b^2)-4*a*c))/(2*a)
	r[2] <- (-b-sqrt((b^2)-4*a*c))/(2*a)
	# check using Lotka equation:
	nr1 <- sum(exp(-r[1]*x)*fx*Lx)
	nr2 <- sum(exp(-r[2]*x)*fx*Lx)
	# it should equal 1 exactly, but isn't quite so precise
	resid1 <- (1-nr1)^2
	resid2 <- (1-nr2)^2
	r <- ifelse(resid1<resid2,r[1],r[2])
	return(r)
}
}