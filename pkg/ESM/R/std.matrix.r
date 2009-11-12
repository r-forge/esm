std.matrix <- function(filename,t=TRUE)
{
	dat <- read.table(filename)
	if(t){dat<-t(dat)}
	dat <- dat/max(dat)
	return(dat)
}