\name{compressimage}
\alias{compressimage}
\title{Compute an approximate image of original image by way of a singular value decomposition}
\description{
Recall that any matrix \code{X} can be decomposed as the matrix product of \code{U}, \code{D}, \code{V}:
\deqn{X = U D V'}
This function returns the first \code{k} ``components'' of \code{X}.  See the code for details. 
}
\usage{compressimage(X,k)}
\arguments{
    \item{X}{An image in a matrix form}
    \item{k}{The number of components to use}
}

\value{
    \item{Xhat}{An approximation of original image using singluar valude decomposion}
}

\author{Nam H. Lee <nhlee@jhu.edu>}

\examples{
data(matlabclown)
Xhat = compressimage(X=matlabclown,k=50)
levelplot(Xhat)
}
