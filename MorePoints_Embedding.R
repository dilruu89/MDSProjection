
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

mds.w.centered2(D2^2,w)


X<-results$Points
Euc_distances<-c(dist(X, method = "euclidean"))




