\name{R0DecompKitagawa}
\alias{R0DecompKitagawa}
\title{
Decompose differences in R0 into fertility, sex ratio and survival components.
}
\description{
The function uses a 2-step Kitagawa decomposition technique to decompose the difference between two R0 estimates into 3 age-specific contribution vectors: fertility, sex ratio and survival. This is a non-symmetrical decomposition technique specific for the case of R0.
}
\usage{
R0DecompKitagawa(fx1, px1, Lx1, fx2, px2, Lx2)
}

\arguments{
6 vectors of age-specific values, all of equal length:
  \item{fx1, fx2}{
vector of age-specific all-birth fertility by age of mother.
}
  \item{px1, px2}{
vector of proportion female of births by age of mother.
}
  \item{Lx1, Lx2}{
vector of lifetable person-years lived between the interval x, x+1 (livings) calculated using a radix of 1. Any \code{Lx} vector can be converted to radix 1 by dividing the entire vector by the value used for l0 in the survival function column of the lifetable.
}
}
\details{
All vectors must refer to the same ages. These may include pre- and post-reproductive ages. In the case of \code{px} and \code{fx}, where non-reproductive ages are used, the vectors must contain no \code{NA}s- instead impute zeros in these ages.
}
\value{
A list with 8 components. The list is given the class \code{"R0DecompKitagawa"} in order to define \code{plot} and \code{summary} methods.
\item{Epsilon }{A vector of the age-specific net contributions to differences in R0.}
\item{Fert }{A vector of the age-specific gross fertility contributions to changes in R0.}
\item{SRB }{A vector of the age-specific contributions to changes in R0 due to differences in the sex ratio at birth (given as the proportion female in the arguments.}
\item{Surv }{A vector of the age-specific survival contributions to changes in R0.}
\item{R01 }{The R0 estimate corresponding with the first set of observations, (population or observation 1).}
\item{R02 }{The R0 estimate corresponding with the second set of observations, (population or observation 2).}
\item{R0diff }{The difference in the two R0 estimates, \code{R01-R02}.}}
\references{
Kitagawa, E.M. (1955) Components of a difference between two rates. Journal of the American Statistical Association. Vol. 50. No. 272. pp. 1168-1194.

Riffe, Timothy (2011) unpublished paper. Decomposing Net Reproduction (R0). Available at \url{https://sites.google.com/site/timriffepersonal/documents/R0DecompMath.pdf}
}
\author{
Timothy Riffe
}
\note{
This function has not undergone substantial testing, and the formulas used in it have not yet been peer reviewed.
}

\seealso{
See Also \code{\link{plot.R0DecompKitagawa}}, \code{\link{summary.R0DecompKitagawa}}, \code{\link{plot.R0Decomp}}
}
\examples{
library(Lotka)
data(LotkaData)
head(LotkaData)
# Fertility 
fx2 <- LotkaData[,"Bx2"]/LotkaData[,"Nx2"]
fx1 <- LotkaData[,"Bx1"]/LotkaData[,"Nx1"]
# Proportion births female
px2 <- LotkaData[,"px2"]
px1 <- LotkaData[,"px1"]
# Proportion births female
Lx2 <- LotkaData[,"Lx2"]
Lx1 <- LotkaData[,"Lx1"]

(DecompK <- R0DecompKitagawa(fx1,px1,Lx1,fx2,px2,Lx2))
summary(DecompK)
plot(DecompK)

## The function is currently defined as
function(fx1,px1,Lx1,fx2,px2,Lx2){
	SurvComponent <- FertComponent <- AgeComponent <- rep(0,length(fx1))
	N <- length(fx1)
	R01 <- sum(fx1*px1*Lx1)
	R02 <- sum(fx2*px2*Lx2)
	R0diff <- R01-R02
	AgeComponent 		<- (fx1*Lx1)-(fx2*Lx2)
	FertComponent 		<- (fx1-fx2)*(Lx1+Lx2)*(px1+px2)*.25
	SexRatComponent 	<- (fx1+fx2)*(Lx1+Lx2)*(px1-px2)*.25
	SurvComponent 		<- (Lx1-Lx2)*(fx1*px1+fx2*px2)*.5
	output <- list(Epsilon=AgeComponent,Fert=FertComponent,SRB=SexRatComponent,Surv=SurvComponent,R01=R01,R02=R02,R0diff)
	class(output) <- "R0DecompKitagawa"
	return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
