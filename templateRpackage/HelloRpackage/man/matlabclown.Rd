\name{matlabclown}
\alias{matlabclown}
\docType{data}
\title{
A dataset containing a matrix version of the "Matlab Clown"
}
\description{
A dataset containing a matrix version of the "Matlab Clown"
}
\usage{data(matlabclown)}
\format{
    A matrix of 320 rows and 200 columns
}
\examples{
data(matlabclown)

image(matlabclown)
levelplot(matlabclown)

X = matlabclown
svdX = svd(X)
U = svdX$u
S = diag(svdX$d)
V = svdX$v
k = 50
M = U[,1:k,drop=FALSE] \%*\% S[1:k,1:k,drop=FALSE] \%*\% t(V[,1:k,drop=FALSE])
image(M) 
image(M,col=gray.colors(k))
}
\keyword{datasets}
