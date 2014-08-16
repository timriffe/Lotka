\name{plot.R0Decomp}
\alias{plot.R0Decomp}
\title{
a plot method for the output from an \code{R0Decomp()} decomposition.
}
\description{
This function recognizes the output from an \code{R0Decomp()} decomposition and plots it as stacked \code{barplot()}s. Three areas are depicted over each age, fertility, sex ratio, and mortality (or survival). Includes legend and age labels with guidelines, as well as several default settings.
}
\usage{
plot.R0Decomp(x, xlim = c(0, 60), col = c("#19B355", "#D8FD0C", "#DA1F04"), main, ylim, dx = T, leg.pos = "topright")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
output from \code{R0Decomp()}.
}
  \item{xlim}{
the limits for the plot area, defined as \code{c(xmin,xmax)}. \code{R0Decomp} output does not include a vector of ages; all vectors are assumed to start at zero and to be in single ages. The default value is \code{c(0,60)}, which includes both pre and post reproductive ages for women. If \code{dx==TRUE}, then it is always good to have 0 as the left bound of the plot area, since infant mortality often accounts for large differences in R0.
}
  \item{col}{
a vector specifying three colors, in order: Fertility, SRB, survival/mortality. Defaults to \code{c("#19B355", "#D8FD0C", "#DA1F04")}, green, yellow, red.
}
  \item{main}{
title for the plot. Defaults to "R0 difference decomposition; R01 = x, R02 = x"
}
  \item{ylim}{
A vector specifying the y axis limits for the plot area. Default values make use of \code{pretty()}.
}
  \item{dx}{
logical. Should the survival/mortality component be displayed as due to \code{lx} or \code{dx}? Default = TRUE, meaning that the dx interpretation is used.
}
  \item{leg.pos}{
text string indicating the legend position. Default = "topright". See type \code{?legend} in the console for more info on other position options.
}
}
\details{
This plot method is not 100\% flexible. To reproduce the basic scheme but with full control of graphical settings, see the code below for plotting ideas. The basic strategy is to \code{rbind} the three output vectors together that you wish to plot (Fert,SRB,Mort), and make two copies of this. In one copy, place only the positive values, with zeroes in all other elements. In the other copy place only the engative values with zeroes in all other elements. Then use \code{barplot()} twice for these two matrces, with \code{add=TRUE} the scond time. Plotting positives and negatives separately removes overlapping. 
}
\author{
Timothy Riffe
}

\seealso{
See Also \code{\link{R0Decomp}}, \code{\link{summary.R0Decomp}}
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
function(x,xlim=c(0,60),col=c("#19B355","#D8FD0C","#DA1F04"),main,ylim,dx=T,leg.pos="topright"){
	if (missing(main)){
		main <- paste("R0 difference decomposition; R01 = ",round(x$R01,digits=4),", R02 = ",round(x$R02,digits=4))
	}
	# age range to be plotted
	age <- xlim[1]:xlim[2]
	
	if (dx==T){
		Epsilon <- rbind(x$Fert[(age+1)],x$SRB[(age+1)],x$Mort[(age+1)])
		leg <- c(paste("Fertility ",round(sum(x$Fert),4)),paste("SRB     ",round(sum(x$SRB),4)),paste("dx          ",round(sum(x$Mort),4)))
	}
	else {
		Epsilon <- rbind(x$Fert[(age+1)],x$SRB[(age+1)],x$Surv[(age+1)])
		leg <- c(paste("Fertility ",round(sum(x$Fert),4)),paste("SRB     ",round(sum(x$SRB),4)),paste("Survival ",round(sum(x$Surv),4)))
	}
	
	EPSpos <- Epsilon * .5*(sign(Epsilon)+1) 	
	EPSneg <- Epsilon * .5*abs(sign(Epsilon)-1) 
	
	# ylim (for looping)
	if (missing(ylim)){
		ylim <- range(pretty(c(EPSpos,EPSneg)))
	}
	ymax <- ylim[2]
	ymin <- ylim[1]
	barplot(EPSpos,space=0,col=col,ylab="contrib to diff in R0",xlab="age",main=main,ylim=ylim)
	barplot(EPSneg,add=T,space=0,col=col)
	segments(age[which((age)\%\%5==0)]-xlim[1],ymin,age[which((age)\%\%5==0)]-xlim[1],0,lty=2,col="#00000030")
	text(age[which(age\%\%5==0)]-xlim[1],ymin,labels=age[which(age\%\%5==0)],pos=1,xpd=T)
	legend(leg.pos,fill=col,legend=leg)
  }
}

