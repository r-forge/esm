hist.disc <- function(vecteur,as.prop=FALSE,do.plot=FALSE,ylab='',xlab='')
{
	niveaux <- as.integer(levels(as.factor(vecteur)))
	quantite <- NULL
	for(i in 1:length(niveaux))
	{
		quantite[i] <- sum(match(vecteur,niveaux[i],nomatch=0))
	}
	proportion <- quantite/length(vecteur)
	
	if(as.prop){hauteur <- proportion} else {hauteur <- quantite}
	
	if(do.plot)
	{
		par(lend=1,yaxs='i')
		plot(niveaux,hauteur,type='h',ylim=c(0,max(hauteur)),lwd=12,ylab=ylab,xlab=xlab)
		par(lend=0,yaxs='r',xaxs='r')
	}
	
	return(as.data.frame(cbind(niveaux,quantite,proportion)))
}