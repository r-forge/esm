empty = function(mat) mat[rowSums(mat)>0,colSums(mat)>0]

pdi <- function(fit)
{
	if(length(fit)==1){return(1)}
	fit <- sort(as.vector(fit),decreasing=TRUE)
	test <- fit[2:length(fit)]
	out <- sum(fit[1]-test)/length(test)
	return(out)
}

rr <- function(fit)
{
	N <- length(fit)
	if(N==1){return(1)}
	Ni <- sum(fit>0)
	return(1-(Ni-1)/(N-1))
}

hs <- function(fit,ifzero=1e-12)
{
	if(length(fit)==1){return(1)}
	fit <- as.vector(fit)
	fit[fit==0] <- ifzero
	pp <- fit/sum(fit)
	partiel <- pp * log(pp)
	shannon <- ifelse(length(fit)<=1,0,-sum(partiel) / log(length(fit)))
	return(1-shannon)
}

ssi <- function(fit)
{
	if(length(fit)==1){return(1)}
	score <- NULL
	fit <- as.vector(fit)
	h <- sum(fit>0)
	H <- length(fit)
	SSI <- sqrt((H/h-1)/(H-1))
	return(SSI)
}

getspe <- function(mat,measure=pdi,normal='whole',...)
{
	mat <- empty(mat)
	if(normal=='species'){mat <- t(apply(mat,1,function(x)x/max(x)))}
	if(normal=='whole'){mat <- mat/max(mat)}
	out <- unlist(apply(mat,1,measure,...))
	names(out) <- rownames(mat)
	return(out)
}

cv		= function(d) sd(d)/mean(d)
last	= function(d) d[length(d)]

scale = function(v,m=0,M=1)
{
	v <- v-min(v)
	v <- v/max(v)
	v <- v*(M-m)
	v <- v+m
	return(v)
}

dmat = function(m,n=2)
{
	e <- (max(m)-min(m))/n
	cl <- seq(from=0,to=1,by=e)
	nm <- matrix(0,ncol=ncol(m),nrow=nrow(m))
	for(i in 1:(length(cl)-1))
	{
		nm[(cl[i]<m)&(m<=cl[(i+1)])] <- cl[i]
	}
	return(scale(nm,0,1))
}

Overlap = function(mat)
{
	for(i in 1:nrow(mat))
	{
		mat[i,] <- mat[i,]/sum(mat[i,])
	}
	out <- matrix(0,nrow=nrow(mat),ncol=nrow(mat))
	colnames(out) <- rownames(mat)
	rownames(out) <- rownames(mat)
	for(i in 1:(nrow(mat)-1))
	{
		for(j in (i+1):nrow(mat))
		{
			out[i,j] <- 1-1/2*sum(abs(mat[i,]-mat[j,]))
		}
	}
	diag(out) <- rep(1,length(diag(out)))
	return(out)
}

getOLsp = function(OLmat,id)
{
	byR <- OLmat[id,]
	byC <- OLmat[,id]
	OlSP <- c(byR[c((id+1):length(byR))],byC[c(1:(id-1))])
	return(OlSP)
}

getOLvec = function(net)
{
	net <- empty(net)
	net <- net/max(net)
	OLmat <- Overlap(net)
	l <- c(1:nrow(OLmat))
	OL <- lapply(l,function(x)getOLsp(OLmat,id=x))
	if(!is.null(rownames(OLmat))){names(OL)<-rownames(OLmat)}
	return(OL)
}