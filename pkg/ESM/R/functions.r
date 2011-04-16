fixmat = function(mat) mat[rowSums(mat)>0,colSums(mat)>0]
subsamp = function(mat,nc=5,nr=5) fixmat(mat[sample(c(1:nrow(mat)),nr,replace=FALSE),sample(c(1:ncol(mat)),nc,replace=FALSE)])

entropy = function(vec)
{
	uns <- unique(vec)
	ps <- NULL
	for(i in 1:length(uns))
	{
		ps <- c(ps,sum(vec==uns[i])/length(vec))
	}
	ENT = -sum(ps*log(ps))
	INF = ENT/log(length(uns))
	return(as.data.frame(list(E=ENT,I=INF)))
}

simpson = function(fit)
{
	fit <- round(fit*1000,0)
	Simp <- sum(fit*(fit-1))/(sum(fit)*(sum(fit)-1))
	return(Simp)
}

pdi <- function(fit)
{
	if(length(fit)==1){return(1)}
	fit <- sort(as.vector(fit),decreasing=TRUE)
	test <- fit[2:length(fit)]
	out <- sum(fit[1]-test)/length(test)
	return(out)
}

Rpdi <- function(fit)
{
	fit <- fit[fit>0]
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
{ # Normalized and using quantitative data
	if (length(fit) == 1) {
        return(1)
    }
	n <- length(fit)
	fit <- as.vector(fit)
	S <- sqrt(sum((fit-mean(fit))^2))
	return((S/mean(fit))/(n*sqrt((n-1)/n)))
}


getspe <- function(mat,measure=pdi,normal='species',...)
{
	mat <- fixmat(mat)
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

dmat = function(mat,n=2)
{
	new <- mat
	Levs <- seq(min(mat),max(mat),length=n)
	for(i in 1:length(Levs))
	{
		AreOk <- (new>Levs[i])&(new<=Levs[(i+1)])
		new[AreOk] <- Levs[(i+1)]
	}
	return(new)
}