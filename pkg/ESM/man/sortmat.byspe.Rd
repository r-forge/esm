\name{sortmat.byspe} 
\Rdversion{1.1} 
\alias{sortmat.byspe} 
\title{Draw the interaction network}
\description{Draw the interaction network between organisms and resources, with bolder lines for stronger interactions}
\usage{ 
sortmat.byspe(mat,spe)
}

\arguments{ 
\item{mat}{A matrix giving the performance of each organism on each resource, with organisms as rows}
\item{spe}{A function to estimate specificity, see \kbd{getspe}}
}

\value{
	\item{}{\kbd{mat} (sorted)}
}
