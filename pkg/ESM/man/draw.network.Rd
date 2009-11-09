\name{draw.network} 
\Rdversion{1.1} 
\alias{draw.network} 
\title{Draw the interaction network}
\description{Draw the interaction network between organisms and resources, with bolder lines for stronger interactions}
\usage{ 
draw.network(data,color='grey',border=NA,...)
}

\arguments{ 
\item{data}{A matrix giving the performance of each organism on each resource, with organisms as rows}
\item{color}{The color of the balls (representing organisms or resources)}
\item{border}{The color of the ball's border} 
\item{sorted}{A function of specificity (see \kbd{getspe}) used to sort the data} 
\item{...}{Arguments to be passed to plot} 
}