mds.tau <- function(H)
{
  #
  #  This function returns the double centering of the inputted matrix.
  #  See Critchley for details.
  #
  n <- nrow(H)
  P <- diag(n) - 1/n
  return(-0.5 * P %*% H %*% P)
}


mds.kappa <- function(C) {
  #
  #  Critchley's kappa operator on centered nxn matrices.
  #  This is the inverse of tau.
  #
  n <- nrow(C)
  H <- matrix(1,nrow=n,ncol=n)
  d <- diag(C)
  H <- diag(d) %*% H
  H <- H + t(H) - 2 * C
  d <- seq(1, n^2, n + 1)
  H[d] <- 0
  return(H)
}

mds.edm2 <- function(X) {
  #
  #  Computes Euclidean squared distance matrix
  #    from configuration matrix.
  #
  n <- nrow(X)
  D <- diag(n) - matrix(1, nrow = n, ncol = n)/n
  D <- D %*% X
  D <- D %*% t(D)
  D <- mds.kappa(D)
  D[D < 0] <- 0
  return(D)
}


Delta2 <- matrix(c(0,1,2,1,1,0,1,2,2,1,0,1,1,2,1,0),byrow=T,ncol=4)
B.4 <- mds.tau(Delta2)  #B
eig <- eigen(B.4, symmetric = TRUE)
v <- eig$values[1:2]
v[v < 0] <- 0
X.4 <- eig$vectors[, 1:2] %*% diag(sqrt(v)) #points

a2 <- c(6.2,4.5,6.3,4.5)
A2 <- cbind(Delta2,a2)
A2 <- rbind(A2,c(a2,0))

#B.5 <- mds.tau(A2)
#eig <- eigen(B.5, symmetric = TRUE)
#v <- eig$values[1:2]
#v[v < 0] <- 0
#X.5 <- eig$vectors[, 1:2] %*% diag(sqrt(v))
#D2.5 <- mds.edm2(X.5)


w <- c(1,1,1,1,0)
B.x <- mds.tau.w(A2,w)

f <- function(y,B,X) {
  n <- nrow(X)+1
  b <- matrix(B[-n,n],ncol=1)
  beta <- B[n,n]
  y <- matrix(y,ncol=1)
  res <- 2*sum((b - X %*% y)^2) + (beta-sum(y^2))^2
  attr(res,"gradient") <- -4 * t(X) %*% (b - X %*% y) -
    4 * (beta - sum(y^2)) * y
  return(res)
} 

y <- nlm(f,p=rnorm(2,sd=5),B=B.x,X=X.4)
Y <- rbind(X.4,y$estimate)
D2.y <- mds.edm2(Y)

