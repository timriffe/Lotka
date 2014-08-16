summary.R0DecompKitagawa <-
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

