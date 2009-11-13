mat2d <- function(x,y,...)
{
	yule <- function(n)
	{
		return(2*floor(2.5*sqrt(sqrt(length(n)))))
	}
	matrice <- matrix(0,ncol=yule(y),nrow=yule(x))
	effX <- cut(x,yule(x),labels=FALSE)
	effY <- cut(y,yule(y),labels=FALSE)
	rownames(matrice) <- c(1:yule(x))
	colnames(matrice) <- c(1:yule(y))
	X <- range(pretty(range(x)))
	X <- seq(min(X),max(X),length.out=nrow(matrice))
	Y <- range(pretty(range(y)))
	Y <- seq(min(Y),max(Y),length.out=ncol(matrice))
	for(i in 1:nrow(matrice))
	{
		for(j in 1:ncol(matrice))
		{
			matrice[i,j] <- sum(match(effX,as.numeric(rownames(matrice))[i],nomatch=0)*match(effY,as.numeric(colnames(matrice))[j],nomatch=0)) 
		}
	}
	pal <- colorRampPalette(c('white','black'))
	image(X,Y,matrice,col=pal(100),...)
	rug(x,ticksize = 0.01)
	rug(y,side=2,ticksize = 0.01)
	colnames(matrice) <- Y
	rownames(matrice) <- X
	return(matrice)
}