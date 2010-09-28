\name{getspe} 
\Rdversion{1.1} 
\alias{getspe} 
\title{Specificity}
\description{Returns a vector containing the specificity of each organism}
\usage{ 
getspe(mat,measure=pdi,normal='whole',...)
}

\arguments{ 
\item{mat}{A matrix giving the performance of each organism on each resource, with organisms as rows} 
\item{measure}{The index of specificity to use. See details, defaults to \kbd{pdi}.}
\item{normal}{The method to normalize the matrix. Defaults to \kbd{whole}, can be set to \kbd{species}.}
\item{...}{Additional arguments to be passed to \kbd{measure}} 
}

\value{
	\item{}{A vector with the specificity of each organism. Uses names if the matrix has such property.}
}
 
\details{Values of \kbd{measure} can be : \kbd{rr}, the classical resource range; \kbd{ssi}, the species specialization index; \kbd{pdi} (default), the Paired Differences Index; \kbd{hs}, Shannon's entropy.

\kbd{normal} is the way to normalize the performance value within the matrix. Defaults to \kbd{whole}, which means that the matrix is divided by its maximal value. Can be set to \kbd{species}, so that the maximal value of each species is set to 1. The performances of the measures were not evaluated with this method of normalization, and it will result in a overestimation of the specificity.
}

\references{For PDI, and this package, cite : Poisot, T, Canard E, Mouquet N & Hochberg, M E "..." XXX.

For HS, cite : Schug J, et al. (2005) Promoter features related to tissue specificity as measured by Shannon entropy. Genome biology 6(4):R33

For SSI, cite : Julliard R, Clavel J, Devictor V, Jiguet F, & Couvet D (2006) Spatial segregation of specialists and generalists in bird communities. Ecology Letters 9(11):1237-1244.}

\examples{ 
# Read an interaction matrix
MAT <- 'http://www.nceas.ucsb.edu/interactionweb/data/host_parasite/text_matrices/mcgregor_p.txt'
mat <- t(read.table(MAT))
# Returns host range
getspe(mat)
} 