MeanGeneration <-
function(fx,Lx,x){
	r <- LotkaRCoale(fx,Lx,x)
	R0 <- Rmomentn(fx,Lx,x,0)
	R1 <- Rmomentn(fx,Lx,x,1)
	R2 <- Rmomentn(fx,Lx,x,2)
	(R1/R0)+r*.5*(((R1/R0)^2)-(R2/R0))
}

