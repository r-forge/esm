get.PDI <- function(fit)
{
	if(length(fit)==1){return(1)}
	fit <- sort(as.vector(fit),decreasing=TRUE)
	test <- fit[2:length(fit)]
	out <- sum(fit[1]-test)/length(test)
	return(out)
}

get.RR <- function(pop)
{
	if(length(pop)==1){return(1)}
	Ni <- sum(pop>0)
	N <- length(pop)
	return(1-(Ni-1)/(N-1))
}

get.HS <- function(pop)
{
	if(length(pop)==1){return(1)}
	partiel <- NULL
	pop <- as.vector(pop)+0.000000000001
	for(i in 1:length(pop))
	{
		p <- pop[i]/sum(pop)
		partiel[i] <- p * log(p)
	}
	shannon <- ifelse(length(pop)<=1,0,-sum(partiel) / log(length(pop)))
	return(1-shannon)
}

get.SSI <- function(occup)
{
	if(length(occup)==1){return(1)}
	score <- NULL
	fit <- as.vector(occup)
	h <- sum(fit>0)
	H <- length(occup)
	SSI <- sqrt((H/h-1)/(H-1))
	return(SSI)
}

PDI <- function(m)
{
	spe <- vector('numeric',length=nrow(m))
	for(i in 1:nrow(m))
	{
		spe[i] <- get.PDI(m[i,])
	}
	return(spe)
}

hs <- function(m)
{
	spe <- vector('numeric',length=nrow(m))
	for(i in 1:nrow(m))
	{
		spe[i] <- get.HS(as.vector(as.numeric(m[i,])))
	}
	return(spe)
}

ssi <- function(m)
{
	spe <- vector('numeric',length=nrow(m))
	for(i in 1:nrow(m))
	{
		spe[i] <- get.SSI(as.vector(as.numeric(m[i,])))
	}
	return(spe)
}

rr <- function(m)
{
	spe <- vector('numeric',length=nrow(m))
	for(i in 1:nrow(m))
	{
		spe[i] <- get.RR(as.vector(as.numeric(m[i,])))
	}
	return(spe)
}

getspe <- function(mat,measure=PDI)
{
	if(max(mat)!=1){mat<-mat/max(mat)}
	out <- measure(as.matrix(mat))
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