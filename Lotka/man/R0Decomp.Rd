\name{R0Decomp}
\alias{R0Decomp}
\title{
Decompose differences in R0, separating the effects of fertility, the sex ratio at birth and survival/mortality.
}
\description{
Function takes birtha and death counts and exposures as arguments, as well as a vector of the age specific proportion of births that are female (or male if male R0 is being compared). The \code{LT()} function from the \code{LifeTable} package is used to calculate \code{lx}, \code{Lx} and \code{dx} for the two sets of observations. The difference between the net reproductive values corresponding with the two rate sets is decomposed using an extension of Kitagawa-style decomposition, that nests the sex ratio effects and the pure fertility effect inside the net fertility effect, thereby leaving the survival effect unchanged. The survival effect is calculated in two ways, which have different interpretations. The effects of differences in the survival function (\code($Surv), in the output) indicates the ages where the net survival changes from all prior ages have their impact. The effects of differences in the death density function (\code{dx}, \code{$Mort} in the output) indicate the ages where morality changes produce impacts that effect reproduction at all later ages. There are also \code{plot()} and \code{summary()} methods for the output from this fnction.  
}
\usage{
R0Decomp(Dx1, Nx1, Bx1, px1, Dx2, Nx2, Bx2, px2)
}

\arguments{
All 8 input vectors must be of equal length and correspond to the same ages.
  \item{Dx1, Dx2}{
Vector of age-specific death counts for population 1,2, ages 0 to omega (or the last reproductive age)
}
  \item{Nx1, Nx2}{
Vector of age-specific person-years of exposure for population 1,2, ages 0 to omega (or the last reproductive age)
}
  \item{Bx1, Bx2}{
Vector of age-specific Birth counts for population 1,2, ages 0 to omega (or the last reproductive age). These counts refer to all births, irrespective of sex of birth.
}
  \item{px1, px2}{
vector of age-specific proportions of births that are female (by age of mother) for population 1,2. If age specific data are not available, then a vector of the aggregate proportion of births that are female, (1/(1+SRB)), the same length as all other arguments. If the age range of data used includes ages prior to or after fertile ages, be sure that the vector contains no \code{NAs}- impute 0s if necessary.
}
}

\details{
Be sure all 8 argument vectors are of the same length. Dx and Bx are counts. Nx are exposures. px must be calculated or assumed ahead of time. If no data is available for \code{px}, and it is assumed to be equal for both sets of observations, says (1/(1+1.05)), it will account for none of the difference in R0, and results can be interpreted equally well.
}
\value{
A list with 8 components. The list is given the class \code{"R0Decomp"} in order to define \code{plot} and \code{summary} methods.
\item{Epsilon }{A vector of the age-specific net contributions to differences in R0.}
\item{Fert }{A vector of the age-specific gross fertility contributions to changes in R0.}
\item{SRB }{A vector of the age-specific contributions to changes in R0 due to differences in the sex ratio at birth (given as the proportion female in the arguments.}
\item{Surv }{A vector of the age-specific survival contributions to changes in R0. Contributions are attributed according to differences in the survival function. \code{Surv} and \code{Mort} are alternative interpretations of the same thing, and they sum to the same amount. When accounting for the total difference in R0, either \code{Surv} or \code{Mort} should be used, but not both.}
\item{Mort }{A vector of the age-specific mortality contributions to changes in R0. Contributions are attributed according to differences in the death density function. \code{Surv} and \code{Mort} are alternative interpretations of the same thing, and they sum to the same amount. When accounting for the total difference in R0, either \code{Surv} or \code{Mort} should be used, but not both.}
\item{R01 }{The R0 estimate corresponding with the first set of observations, (population or observation 1).}
\item{R02 }{The R0 estimate corresponding with the second set of observations, (population or observation 2).}
\item{R0diff }{The difference in the two R0 estimates, \code{R01-R02}.}
}
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
See Also \code{\link{plot.R0Decomp}},  \code{\link{summary.R0Decomp}},  \code{\link{R0DecompKitagawa}}
}
\examples{
library(Lotka)
data(LotkaData)
head(LotkaData)
# Deaths
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
plot(Decomp1,dx=T)
# and equivalently, but with different interpretation:
plot(Decomp1,dx=F)
## The function is currently defined as

function(Dx1,Nx1,Bx1,px1,Dx2,Nx2,Bx2,px2){
	require(LifeTable)
	N <- length(Nx1)
	LT1 <- LT(Nx1,Dx1,axmethod="schoen",axsmooth=T,mxsmooth=F,radix=1,type="single-age")
	LT2 <- LT(Nx2,Dx2,axmethod="schoen",axsmooth=T,mxsmooth=F,radix=1,type="single-age")
	dx1 <- LT1$dx ; dx2 <- LT2$dx ;lx1 <- LT1$lx ; lx2 <- LT2$lx
	Lx1 <- (lx1[1:(N-1)]+lx1[2:N])/2 ; Lx2 <- (lx2[1:(N-1)]+lx2[2:N])/2
	fx1 <- Bx1/Nx1	;	fx2 <- Bx2/Nx2
# # # # # # # # # # # # # # # # # # # # 	
	MortComponent3 <- MortComponent2 <- MortComponent <- SurvComponent <- FertComponent <- AgeComponent <- rep(0,(N-1))
# # # # # # # # # # # # # # # # # # # # 
	R01 <- sum(px1[-N]*fx1[-N]*Lx1)
	R02 <- sum(px2[-N]*fx2[-N]*Lx2)
	R0diff <- R01-R02
# # # # # # # # # # # # # # # # # # # # 
	AgeComponent 	<- (px1[-N]*fx1[-N]*Lx1)-(px2[-N]*fx2[-N]*Lx2)
	FertComponent <- (px1[-N]+px2[-N])*(fx1[-N]-fx2[-N])*(Lx1+Lx2)*.25
	SexRatComponent <- (px1[-N]-px2[-N])*(fx1[-N]+fx2[-N])*(Lx1+Lx2)*.25
	SurvComponent 	<- (Lx1-Lx2)*(px1[-N]*fx1[-N]+px2[-N]*fx2[-N])*.5
# # # # # # # # # # # # # # # # # # # # 
	for (i in 1:(N-1)){
		MortComponent[i]	<- (dx2[i]-dx1[i])*(sum(((px1*fx1)[(i+1):N]+(px2*fx2)[(i+1):N]))+.5*((px1*fx1)[i]+(px2*fx2)[i]))*.5					
	}
	output <- list(Epsilon=AgeComponent,Fert=FertComponent,SRB=SexRatComponent,Surv=SurvComponent,Mort=MortComponent,R01=R01,R02=R02,R0diff=R0diff)
	class(output) <- "R0Decomp"
	return(output)
  }
}
