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
	return(shannon)
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
	out <- measure(mat)
	names(out) <- rownames(mat)
	return(out)
}

std.matrix <- function(filename,t=TRUE)
{
	dat <- read.table(filename)
	if(t){dat<-t(dat)}
	dat <- dat/max(dat)
	return(dat)
}

GDS <- function(mat)
{
	D <- dim(mat)
	N <- prod(D)
	Ni <- sum(mat>0)/N
	out <- NULL
	out$RES <- D[1]
	out$ORGA <- D[2]
	out$SIZE <- N
	out$COMP <- Ni
	return(out)
}

sortmat.byspe <- function(mat,spe)
{
	mat[rank(spe),] <- mat[c(1:nrow(mat)),]
	return(mat)
}

draw.network <- function(data,org.col='grey',res.col='grey',border=NA,sortorg=c(1:nrow(data)),sortres=c(1:ncol(data)),...)
{
	
	data <- sortmat.byspe(data,sortorg)
	data <- t(sortmat.byspe(t(data),sortres))
	
	n.elements <- max(dim(data))
	
	pos.h <- seq(from=1,to=n.elements,length.out=ncol(data))
	pos.p <- seq(from=1,to=n.elements,length.out=nrow(data))
	
	par(bty='n',xaxt='n',yaxt='n')
	symbols(
		pos.p,
		y = rep(2,length(pos.p)),
		circles=rep(0.3,length(pos.p)),
		inches=FALSE,
		add=FALSE,
		ylim=c(0.7,2.3),
		bg=org.col,
		fg=border,
		...
		)

	for(p in 1:nrow(data))
	{
		for(h in 1:ncol(data))
		{
			if(data[p,h]>0)
			{
				arrows(pos.p[p],2,pos.h[h],1,lwd=(data[p,h]/1)*3+0.1,code=0)
			}
		}
	}

	symbols(
		pos.p,
		y = rep(2,length(pos.p)),
		circles=rep(0.4,length(pos.p)),
		inches=FALSE,
		add=TRUE,
		bg=org.col,
		fg=border,
		main='Réseau',
		xlab='',
		ylab=''
		)
	symbols(
		pos.h,
		y = rep(1,length(pos.h)),
		circles=rep(0.4,length(pos.h)),
		inches=FALSE,
		add=TRUE,
		bg=res.col,
		fg=border
		)
}

matrix.hr <- function(matrix)
{
	for(c in 1:ncol(matrix))
	{
		for(r in 1:nrow(matrix))
		{
			if(matrix[r,c]>0){matrix[r,c]<-1}
		}
	}
	return(matrix)
}