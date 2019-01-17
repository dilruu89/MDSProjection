

Outofsample_method<-function(squared_matrix, points){
  
  D2<-squared_matrix
  n <- nrow(D2)
  ones <- rep(1, n-1)
  w <-c(ones,0)
  print(w)
  B <- mds.w.centered(D2,w)
  X<-points
  
  #Euc_distances<-c(dist(X, method = "euclidean"))
  #stdv<-sd(Euc_distances)
  #y <- nlm(minimizing_func,p=rnorm(2,sd=stdv),B,X)
  
  y <- nlm(minimizing_func,p=c(1,1),B,X)
  
  New_points <- rbind(X,y$estimate)
  #New_Euc_distances<-c(dist(New_points, method = "euclidean"))
  
  return(New_points)
  
}

mds.w.centered <- function(D,w) {
  
  n <- length(w)
  w <- matrix(w,ncol=1)
  e <- matrix(1,nrow=n,ncol=1)
  s <- sum(w)
  P <- diag(n) - e %*% t(w)/s
  Q <- diag(n) - w %*% t(e)/s
  X <- -0.5 * P %*% D %*% Q
  return (X) 
  
}


minimizing_func <- function(y,B,X) {
  
  n <- nrow(X)+1
  b <- matrix(B[-n,n],ncol=1)
  beta <- B[n,n]
  y <- matrix(y,ncol=1)
  
  res <- 2*sum((b - X %*% y)^2) + (beta-sum(y^2))^2
  
  attr(res,"gradient") <- -4 * t(X) %*% (b - X %*% y)  -
    4 * (beta - sum(y^2)) * y
  return(res)
} 






