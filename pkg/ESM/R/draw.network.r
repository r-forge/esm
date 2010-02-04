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
		main='Web',
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