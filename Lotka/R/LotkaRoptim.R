LotkaRoptim <-
function(fx,Lx,x=1:length(fx)-.5,TOL=.Machine$double.eps){
	findR <- function(r){
		# this is the Lotka equation, with the 1 moved over to the right side!
		(1-sum(exp(-r*x)*fx*Lx))^2
	}
	optimize(f=findR,seq(-1,1,.0001),tol=TOL)$minimum
}

