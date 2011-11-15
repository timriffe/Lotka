LotkaRanalytic <-
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

