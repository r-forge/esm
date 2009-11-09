\name{getspe} 
\Rdversion{1.1} 
\alias{getspe} 
\title{Specificity}
\description{Returns a vector containing the specificity of each organism}
\usage{ 
getspe(mat,measure=hr)
}

\arguments{ 
\item{mat}{A matrix giving the performance of each organism on each resource, with organisms as rows} 
\item{measure}{The index of specificity to use. Values can be} 
}

\value{
	\item{}{A vector with the specificity of each organism. Uses names if the matrix as such property.}
}
 
\details{Values of \kbd{measure} can be : \kbd{hr}, the classical host range; \kbd{ssi}, the species specialization index; \kbd{dfp}, the DFP index; \kbd{shannon}, Shannon's entropy; \kbd{wadfp}, the weighted additive DFP.}

\references{For DFP and WADFP, and this package, cite : Poisot, T & Hochberg, M E "..." XXX.

For HS, cite : Schug J, et al. (2005) Promoter features related to tissue specificity as measured by Shannon entropy. Genome biology 6(4):R33

For SSI, cite : Julliard R, Clavel J, Devictor V, Jiguet F, & Couvet D (2006) Spatial segregation of specialists and generalists in bird communities. Ecology Letters 9(11):1237-1244.}

\examples{ 
# Read an interaction matrix
MAT <- 'http://www.nceas.ucsb.edu/interactionweb/data/host_parasite/text_matrices/mcgregor_p.txt'
mat <- t(read.table(MAT))
# Returns host range
getspe(mat,hr)
} 