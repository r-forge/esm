sortmat.byspe <- function(mat,spe)
{
	mat[rank(spe),] <- mat[c(1:nrow(mat)),]
	return(mat)
}