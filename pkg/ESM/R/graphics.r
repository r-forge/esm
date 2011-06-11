ViewNetwork <- function(data,org.col='grey',res.col='grey',border=NA,sortorg=c(1:nrow(data)),sortres=c(1:ncol(data)),...)
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

ViewMatrix = function(web,type='diagonal',gr.lty=1,gr.col='lightgrey',no.fg='black',no.bg='black',...)
{
	web <- empty(web)
	if(max(web)>1){web <- web/max(web)}
	## This is from bipartite
	if (type == "diagonal") {
		require(vegan)
		ca <- cca(web)
		web <- web[order(summary(ca)$sites[, 1], decreasing = TRUE), order(summary(ca)$species[, 1], decreasing = TRUE)]
	}
	if (type == "nested") {
		web <- web[order(rowSums(web), decreasing = TRUE), order(colSums(web), decreasing = TRUE)]
	}
	## This is not from bipartite anymore
	nc <- ncol(web)
	nr <- nrow(web)
	op <- par(no.readonly = TRUE)
	# This is from bipartite
	m.predsize = max(strwidth(colnames(web), units = "inches"))
	m.preysize = max(strwidth(rownames(web), units = "inches"))
	mm <- 2*max(m.predsize, m.preysize)
	# This is not from bipartite anymore
	par(omi = c(0, 0, 0, 0), mai = c(mm, mm, 0, 0),las=2)
	plot(0,0,type='n',xlim=c(0,nc),ylim=c(0,nr),asp=1,bty='n',xlab='',ylab='',yaxt='n',xaxt='n',...)
	segments(par()$usr[1], c(1:(nr))-0.5, x1=nc-0.5,col=gr.col,lty=gr.lty)
	segments(c(1:(nc))-0.5, par()$usr[3], y1=nr-0.5,col=gr.col,lty=gr.lty)
	for(col in 1:nc)
	{
		for(row in 1:nr)
		{
			if(web[row,col]>0) symbols(col-0.5,row-0.5,circles=web[row,col]/2,inches=FALSE,add=TRUE,bg=no.bg,fg=no.fg)
		}
	}
	axis(1,at=c(1:nc)-0.5,labels=colnames(web),tick=FALSE,crt=45,cex=1)
	axis(2,at=c(1:nr)-0.5,labels=rownames(web),tick=FALSE,crt=45,cex=0.2)
	par(op)
}

plotspe = function(x,...,palette=rainbow)
{
	spe <- getspe(x,...)
	xs <- scale(c(1:ncol(x))/ncol(x),0,1)
	
	Colors <- palette(21)
	spe <- round(spe*20,0)+1
	
	par(xaxs='i',yaxs='i')
	plot(0,pch=NA,ylim=c(0,1),xlim=range(xs))
	polygon(x=c(0,1,0),y=c(0,0,1),border=NA,col='lightgrey')
	for(i in 1:nrow(x))
	{
		lines(xs,sort(x[i,]/max(x[i,]),decreasing=TRUE),lwd=2,col=Colors[spe[i]])
	}
	par(xaxs='r',yaxs='r')
}