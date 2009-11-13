partition.var <- function(comp)
{
	pdf(file='partition-variance.pdf',width=6,height=3)
	par(mai=c(0,0,0,0))
	plot(0,0,pch=NA,axes=F,xlab='',ylab='',ylim=c(-1,1),xlim=c(0,100))
	rect(0,-0.1,comp[1],0.1,col='darkgrey')
	rect(comp[1],-0.1,sum(comp[1:2]),0.1,col='black')
	rect(sum(comp[1:2]),-0.1,sum(comp[1:3]),0.1,col='lightgrey')
	rect(sum(comp[1:3]),-0.1,sum(comp),0.1,col='white')
	
	text(0,0.2,'Env.',adj=0)
	text(comp[1],-0.2,'Shared',adj=0)
	text(sum(comp[1:2]),0.2,'Phyl.',adj=0)
	dev.off()
}