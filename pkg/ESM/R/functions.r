get.DBF <- function(fit)
{
	fit <- sort(as.vector(fit),decreasing=TRUE)
	test <- fit[2:length(fit)]
	out <- sum(fit[1]-test)/length(test)
	return(out)
}

get.HR <- function(pop)
{
	Ni <- sum(pop>0)
	N <- length(pop)
	return(1-Ni/N)
}

get.HS <- function(pop)
{
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
	score <- NULL
	fit <- as.vector(occup)
	h <- sum(fit>0)
	H <- length(occup)
	SSI <- sqrt((H/h-1)/(H-1))
	return(SSI)
}

dbf <- function(m)
{
	spe <- vector('numeric',length=nrow(m))
	for(i in 1:nrow(m))
	{
		spe[i] <- get.DBF(m[i,])
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

cv		= function(d) sd(d)/mean(d)
last	= function(d) d[length(d)]