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