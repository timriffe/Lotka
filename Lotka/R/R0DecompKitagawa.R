R0DecompKitagawa <-
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
	output <- list(Epsilon=AgeComponent,Fert=FertComponent,SRB=SexRatComponent,Surv=SurvComponent,R01=R01,R02=R02,R0diff=R0diff)
	class(output) <- "R0DecompKitagawa"
	return(output)
}

