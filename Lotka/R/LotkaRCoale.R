LotkaRCoale <-
function(fx,Lx,x){
	# from Coale, Ansley J. (1957) A New Method for Calculating Lotka's r- the Intrinsic Rate of Growth in a Stable Population.
	# Population Studies, Vol. 11 no. 1, pp 92-94
	R0 <- Rmomentn(fx,Lx,x,0)
	# first assuming a mean generation time of 29
	ri <- log(R0)/29
	for (i in 1:15){ # 10 is more than enough!
		deltai <- sum(exp(-ri*x)*fx*Lx)-1
		# the mean generation time self-corrects 
		# according to the error produced by the Lotka equation
		ri <- ri+(deltai/(29-(deltai/ri)))
	}
	return(ri)	
}

