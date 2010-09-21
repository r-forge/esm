nullweb = function(ref)
{
	ref <- empty(ref)
	ref[ref>0] <- 1
	margin <- ifelse(ncol(ref)<nrow(ref),2,1)
	currentweb <- matrix(0,ncol=ncol(ref),nrow=nrow(ref))
	pc <- colMeans(ref)
	pr <- rowMeans(ref)
	if(margin==2)
	{
		for(i in 1:ncol(ref))
		{
			currentweb[,i] <- (pc[i]+pr)/2
		}
	} else {
		for(i in 1:nrow(ref))
		{
			currentweb[i,] <- (pr[i]+pc)/2
		}
	}
	return(apply(currentweb,margin,function(x)rbinom(length(x),1,x)))
}

nullreps = function(ref,rep) replicate(rep,nullweb(ref),FALSE)