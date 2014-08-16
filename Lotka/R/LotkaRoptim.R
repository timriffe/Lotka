LotkaRoptim <-
function(fx,Lx,x){
	findR <- function(r){
		# this is the Lotka equation, with the 1 moved over to the right side!
		(1-sum(exp(-r*x)*fx*Lx))^2
	}
	optimize(f=findR,seq(-1,1,.0001),tol=.00000000001)$minimum
}

