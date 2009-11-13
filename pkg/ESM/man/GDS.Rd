\name{GDS} 
\Rdversion{1.1} 
\alias{GDS} 
\title{Descriptive statistics}
\description{Return a list with basic description of a dataset}
\usage{ 
GDS(mat)
}

\arguments{ 
\item{mat}{A matrix giving the performance of each organism on each resource, with organisms as rows} 
}

\value{
	\item{RES}{number of resources}
	\item{ORGA}{number of organisms}
	\item{SIZE}{number of interactions}
	\item{COMPL}{proportion of interactions > 0}
}
