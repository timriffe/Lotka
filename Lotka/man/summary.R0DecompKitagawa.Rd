\name{summary.R0DecompKitagawa}
\alias{summary.R0DecompKitagawa}
\title{
summary method for the output from \code{R0DecompKitagawa}
}
\description{
This function recognizes the output from an \code{R0DecompKitagawa()} decomposition and produces a summary table with a row each for fertility, sex ratio and survival components (summed over age) and a column each for the absolute and percent contributions to the difference in R0 values. Some percents may appear as negative or greater than 100, which is impossible, but the interpretation is straightforward.
}
\usage{
summary.R0DecompKitagawa(x)
}

\arguments{
  \item{x}{
output from \code{R0DecompKitagawa()}.
}
}
\value{
summary table printed to the console. There is no other output.
}
\author{
Timothy Riffe
}
\note{
A future modification might make optional LaTeX output for the table using \code{xtable}. This is not yet implemented here.
}


\seealso{
See Also \code{\link{plot.R0DecompKitagawa}}, \code{\link{R0DecompKitagawa}}
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

DecompK <- R0DecompKitagawa(fx1,px1,Lx1,fx2,px2,Lx2)
summary(DecompK)

## The function is currently defined as
function(x){
	R01 <- round(x$R01,4)
	R02 <- round(x$R02,4)
	R0diff <- round(x$R0diff,4)
	Epsilon <- round(R01-R02,4)
	Survival <- round(sum(x$Surv),4)
	Fertility <- round(sum(x$Fert),4)
	SexRatio <- round(sum(x$SRB),4)
	
	results <- matrix(nrow=3,ncol=2)
	results[1,] <- c(Fertility,round((Fertility/R0diff)*100,2))
	results[2,] <- c(SexRatio,round((SexRatio/R0diff)*100,2))
	results[3,] <- c(Survival,round((Survival/R0diff)*100,2))
	colnames(results) <- c("absolute","percent")
	rownames(results) <- c("Fertility","SexRatio","Survival")
	line1 <- "\n3 Factor nested Kitagawa Decomposition of R0 Difference"
	line2 <- paste("\nR01 =",R01," ; R02 =",R02,"")
	line3 <- paste("\nThe difference, Epsilon, =",Epsilon,"\n\n")
	cat("\n##############################################################")
	cat(line1,line2,line3)
	print(results)
	cat("\n##############################################################")
  }
}

