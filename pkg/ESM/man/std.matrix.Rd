\name{std.matrix} 
\Rdversion{1.1} 
\alias{std.matrix} 
\title{Read an interaction matrix}
\description{Returns an interaction matrix normalized to 1}
\usage{ 
std.matrix(filename,t=FALSE)
}

\arguments{ 
\item{filename}{The file containing the matrix (interaction)} 
\item{t}{Logical to transpose the matrix, FALSE by default} 
}

\value{
	\item{}{An interaction matrix}
}
 
\details{Values of \kbd{measure} can be : \kbd{hr}, the classical host range; \kbd{ssi}, the species specialization index; \kbd{dfp}, the DFP index; \kbd{shannon}, Shannon's entropy; \kbd{wadfp}, the weighted additive DFP.}

\references{For DFP and WADFP, and this package, cite : Poisot, T & Hochberg, M E "..." XXX.

For HS, cite : Schug J, et al. (2005) Promoter features related to tissue specificity as measured by Shannon entropy. Genome biology 6(4):R33

For SSI, cite : Julliard R, Clavel J, Devictor V, Jiguet F, & Couvet D (2006) Spatial segregation of specialists and generalists in bird communities. Ecology Letters 9(11):1237-1244.}

\examples{ 
# Read an interaction matrix
MAT <- std.matrix('http://www.nceas.ucsb.edu/interactionweb/data/host_parasite/text_matrices/mcgregor_p.txt',t=TRUE)
} 