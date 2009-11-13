persp.withcol <- function(x,y,z,pal=heat.colors,nb.col=10,xlg=TRUE,ylg=TRUE,...)
{
	colnames(z) <- y
	rownames(z) <- x
	
	nrz <- nrow(z) 
	ncz <- ncol(z) 
	
	color <- pal(nb.col)
	zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz] 
	facetcol <- cut(zfacet, nb.col) 
	par(xlog=xlg,ylog=ylg)
	persp(
		as.numeric(rownames(z)),
		as.numeric(colnames(z)),
		as.matrix(z),
		col=color[facetcol],
		...
		) 
}