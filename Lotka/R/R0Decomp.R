R0Decomp <-
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

