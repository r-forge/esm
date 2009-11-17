get.HR <- function(pop)
{
	Ni <- sum(pop>0)
	N <- length(pop)
	return(1-Ni/N)
}

get.SHANNON <- function(pop)
{
	partiel <- NULL
	pop <- as.vector(pop)+0.0000000001
	for(i in 1:length(pop))
	{
		p <- pop[i]/sum(pop)
		partiel[i] <- p * log(p)
	}
	shannon <- ifelse(length(pop)<=1,0,-sum(partiel) / log(length(pop)))
	return(1-shannon)
}

get.DFP <- function(fit)
{
	score <- NULL
	fit <- as.vector(fit)
	fit <- na.exclude(sort(fit,decreasing=TRUE))
	vecdiff <- NULL	
	for(i in 1:(length(fit)-1))
	{
		vecdiff[i] <- (fit[i+1]-fit[i])+1
	}
	score <- prod(abs(vecdiff))
	return(1-score)
}

get.WADFP <- function(fit)
{
	ngen <- length(fit)
	sorted <- sort(fit,decreasing=TRUE)
	tempS <- NULL
	correc <- (ngen-1)/ngen
	for(g in 1:(ngen-1))
	{
		tempS[g] <- (1+sorted[g]-sorted[g+1])*sorted[g]
	}
	S <- sum(tempS)/sum(fit)
	S <- (S-correc)/(2-correc)
	return(S)
}

get.SSI <- function(occup)
{
	score <- NULL
	fit <- as.vector(occup)
	h <- sum(fit>0)
	H <- length(occup)
	SSI <- sqrt((H/h-1)/(H-1))
	return(SSI)
}

dfp <- function(m)
{
	spe <- vector('numeric',length=nrow(m))
	for(i in 1:nrow(m))
	{
		spe[i] <- get.DFP(m[i,])
	}
	return(spe)
}

wadfp <- function(m)
{
	spe <- vector('numeric',length=nrow(m))
	for(i in 1:nrow(m))
	{
		spe[i] <- get.WADFP(m[i,])
	}
	return(spe)
}


shannon <- function(m)
{
	spe <- vector('numeric',length=nrow(m))
	for(i in 1:nrow(m))
	{
		spe[i] <- get.SHANNON(as.vector(as.numeric(m[i,])))
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

hr <- function(m)
{
	spe <- vector('numeric',length=nrow(m))
	for(i in 1:nrow(m))
	{
		spe[i] <- get.HR(as.vector(as.numeric(m[i,])))
	}
	return(spe)
}

getspe <- function(mat,measure=hr)
{
	if(max(mat)!=1){mat<-mat/max(mat)}
	out <- measure(as.matrix(mat))
	names(out) <- rownames(mat)
	return(out)
}