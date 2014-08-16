plot.R0Decomp <-
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
	segments(age[which((age)%%5==0)]-xlim[1],ymin,age[which((age)%%5==0)]-xlim[1],0,lty=2,col="#00000030")
	text(age[which(age%%5==0)]-xlim[1],ymin,labels=age[which(age%%5==0)],pos=1,xpd=T)
	legend(leg.pos,fill=col,legend=leg)
}

