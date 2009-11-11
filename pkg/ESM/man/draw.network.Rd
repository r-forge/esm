\name{draw.network} 
\Rdversion{1.1} 
\alias{draw.network} 
\title{Draw the interaction network}
\description{Draw the interaction network between organisms and resources, with bolder lines for stronger interactions}
\usage{ 
draw.network(data,org.col='grey',res.col='grey',border=NA,sortorg=c(1:nrow(data)),sortres=c(1:ncol(data)),...)
}

\arguments{ 
\item{data}{A matrix giving the performance of each organism on each resource, with organisms as rows}
\item{org.col}{The color of organisms (recycled)}
\item{res.col}{The color of resources (recycled)}
\item{border}{The color of the ball's border} 
\item{sortorg}{A vector giving the order in which to sort the organisms (for example using \kbd{rank} and \kbd{getspe})} 
\item{sortres}{Similar to sortorg for resources} 
\item{...}{Arguments to be passed to plot} 
}

\examples{ 
# Read an interaction matrix
library(vegan)
dune <- data(dune)
dune <- as.matrix(dune/max(dune))
# Plot
draw.network(dune,org.col='palegreen',sortorg=rank(getspe(dune,hr)))
}