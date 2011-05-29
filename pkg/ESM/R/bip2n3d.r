bip2n3d = function(mat,filename='bip2n3d')
{
	n3d <- NULL
	spInf <- NULL
	for(pred in 1:nrow(mat)) for(prey in 1:ncol(mat))
	{
		if(mat[pred,prey]>0)
		{
			n3d <- rbind(n3d,c(pred,(nrow(mat)+prey),mat[pred,prey]))
		}
	}
	write.table(n3d,file=paste(filename,'.web',sep=''),row.names=FALSE,col.names=FALSE)
}