compressimage <-function(X=NULL,k=50){
    svdX = svd(X)
    U = svdX$u
    S = diag(svdX$d)
    V = svdX$v
    Xhat = U[,1:k,drop=FALSE] %*% S[1:k,1:k,drop=FALSE] %*% t(V[,1:k,drop=FALSE])
    return(Xhat)
}