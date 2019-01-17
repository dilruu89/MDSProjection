library(gdata)

MDS <-function(Dissimilarity_matrix, dim){
  
  #D<-Dissimilarity_matrix
  #D2 <- D^2
  D2<-Dissimilarity_matrix

  k=dim
  
  n <- nrow(D2)
  ones <- rep(1, n)
  C <- diag(1, n) - (1 / n) * ones %*% t(ones)
  C
  
  B <- - (1 / 2) *C %*% D2 %*% C
  B
  
  vec2 <- c(upperTriangle(B, diag=FALSE, byrow=TRUE))
  
  e <- eigen(B, symmetric = TRUE)
  ev <- e$values[seq_len(k)]
  evec <- e$vectors[, seq_len(k), drop = FALSE]
  
  k1 <- sum(ev > 0)
  
  if (k1 < k) {
    warning(gettextf("only %d of the first %d eigenvalues are > 0", 
                     k1, k), domain = NA)
    evec <- evec[, ev > 0, drop = FALSE]
    ev <- ev[ev > 0]
  }
  points <- evec * rep(sqrt(ev), each = n)
  
  results <- list("InnerProduct" = B, "Points" = points, "EigenValues" = e)
  
  return(results)
  
}
