matrix.hr <- function(matrix)
{
	for(c in 1:ncol(matrix))
	{
		for(r in 1:nrow(matrix))
		{
			if(matrix[r,c]>0){matrix[r,c]<-1}
		}
	}
	return(matrix)
}