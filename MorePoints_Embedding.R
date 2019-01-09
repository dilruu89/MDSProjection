
D<-dist.name.enh

newnames <- data.frame(randomnames=c("james","jane"))
randomnames <- rbind(randomnames, newnames)

dist.name.enh <- stringdistmatrix(tolower(randomnames$randomnames),
                                  tolower(randomnames$randomnames),
                                  method = distance.method,
                                  nthread = getOption("sd_num_thread"),q=2)


D2<-dist.name.enh
n1<-nrow(D)
n2<- nrow(D2)
ones <- rep(1, n1)
zeros <-rep(0,n2- n1)
w <-c(ones,zeros)
lower<-n1+1

mds.w.centered2<- function(D,w) {
  #
  #  This function computes tau_w for specified w.
  #
  n <- length(w)
  w <- matrix(w,ncol=1)
  e <- matrix(1,nrow=n,ncol=1)
  s <- sum(w)
  P <- diag(n) - e %*% t(w)/s
  Q <- diag(n) - w %*% t(e)/s
  return(-0.5 * P %*% D %*% Q)
}

minimizing_func <- function(y,B,X) {
  n <- nrow(X)+(n2-n1)
  B_xy <- t(B[lower:n,1:n1])
  B_yy <- B[lower:n,lower:n]
  
  y <- matrix(y,ncol=2)
  
  res <- 2*sum((B_xy - X %*% y)^2) + (B_yy-sum(y^2))^2
  
  attr(res,"gradient") <- -4 * t(X) %*% (B_xy - X %*% y) - 4 * (B_yy - sum(y^2)) * y
  
  return(res)
} 

B <- mds.w.centered2(D2^2,w)

X<-results$Points
Euc_distances<-c(dist(X, method = "euclidean"))

stdv<-sd(Euc_distances)


y <- nlm(minimizing_func,p=rnorm(4,sd=stdv),B,X)

#New_points <- rbind(X,y$estimate)




