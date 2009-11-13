nest = function(matrice,do.plot)
{
	perm <- 1
	while(perm != 0)
	{
		perm <- 0
		for(i in 1:(nrow(matrice)-1))
		{
			temp.i <- matrice[i,]
			temp.s <- matrice[(i+1),]
			if(sum(temp.i)<sum(temp.s))
			{
			matrice[i,] <- temp.s
			matrice[(i+1),] <- temp.i
			perm <- perm + 1
			}
		}
	}
	perm <- 1
	while(perm != 0)
	{
		perm <- 0
		for(i in 1:(ncol(matrice)-1))
		{
			temp.i <- matrice[,i]
			temp.s <- matrice[,(i+1)]
			if(sum(temp.i)<sum(temp.s))
			{
			matrice[,i] <- temp.s
			matrice[,(i+1)] <- temp.i
			perm <- perm + 1
			}
		}
	}
	if(do.plot)
	{
		cols <- colorRampPalette(c("#FFFFFF", "#000000"))
		image(matrice,col= cols(5))
	}
	return(matrice)
}