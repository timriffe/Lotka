\name{LotkaData}
\alias{LotkaData}
\docType{data}
\title{
Example data for Lotka package
}
\description{
Vital data for Spanish females, 1975 and 1998. 
}
\usage{data(LotkaData)}
\format{
  The format is:
 num [1:105, 1:11] 0 1 2 3 4 5 6 7 8 9 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:11] "Age" "Nx1" "Dx1" "px1" ...
}
\details{
Age 104 is an open age groupe, i.e. "104+".
1 indicates 1998 and 2 indicates 1975, sorry if that's counterintuitive...
\code{Nx} are exposures. \code{Dx} are deaths. \code{Bx} are total births by age of mother. \code{px} are proportions of births that are female by age of mother. \code{Lx} is the lifetable L(x) with radix 1, as calculated by the \code{LT()} from the \code{LifeTable} package.
}
\source{
Deaths and Exposures: Human Mortality Database. University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at \link{www.mortality.org}. Data download in February 2011.

Fertility data from: España Instituto Nacional de Estadistica. Birth Vital Stats Microdata, 1975 and 1998. \link{http://www.ine.es/en/prodyser/micro_mnp_nacim_en.htm}
}
\examples{
library(Lotka)
data(LotkaData)
head(LotkaData)
# Exposures, from HMD
plot(LotkaData[,1],LotkaData[,"Nx1"],type='l',col="blue",main="female exposures, Spain",xlab="Age",ylab="Person Years Exposure")
lines(LotkaData[,1],LotkaData[,"Nx2"],col="red")
legend("topright",col=c("blue","red"),lty=1,legend=c(1998,1975))
# Deaths, count from HMD
plot(LotkaData[,1],LotkaData[,"Dx1"],type='l',col="blue",main="Female deaths, Spain",xlab="Age",ylab="Death count",ylim=c(0,10000))
lines(LotkaData[,1],LotkaData[,"Dx2"],col="red")
legend("topright",col=c("blue","red"),lty=1,legend=c(1998,1975))
# Births, count aggregated by author 
plot(LotkaData[15:55,1],LotkaData[15:55,"Bx1"],type='l',col="blue",main="total births by age of mother, Spain",xlab="Age",ylab="Births",ylim=c(0,50000))
lines(LotkaData[15:55,1],LotkaData[15:55,"Bx2"],col="red")
legend("topright",col=c("blue","red"),lty=1,legend=c(1998,1975))
# Proportion female of births by age of mother, aggregated by author
plot(LotkaData[15:55,1],LotkaData[15:55,"px1"],type='l',col="blue",main="Proportion female of births by age of mother, Spain",xlab="Age",ylab="proportion female of births",ylim=c(.4,.6))
lines(LotkaData[15:55,1],LotkaData[15:55,"px2"],col="red")
legend("topright",col=c("blue","red"),lty=1,legend=c(1998,1975),bg="white")
# Female Lx (using LT() in LifeTable package (axmethod="schoen")
plot(LotkaData[,1],LotkaData[,"Lx1"],type='l',col="blue",main="Female Lx (radix = 1), Spain",xlab="Age",ylab="Lx",ylim=c(0,1))
lines(LotkaData[,1],LotkaData[,"Lx2"],col="red")
legend("topright",col=c("blue","red"),lty=1,legend=c(1998,1975),bg="white")}
\keyword{datasets}
