library(stringdist)
library(gdata)

Surnamefile1 <- read.csv("~/Documents/RScripts/surname.csv",header = TRUE)

distance.method <- "qgram"

randomnames <- Surnamefile1[sample(nrow(Surnamefile1), 10),]
randomnames <- as.data.frame(randomnames)
randomnames


dist.name.enh <- stringdistmatrix(tolower(randomnames$randomnames),
                                  tolower(randomnames$randomnames),
                                  method = distance.method,
                                  nthread = getOption("sd_num_thread"),q=2)

D<-dist.name.enh

results<-MDS(D,2)

new_point <-"rise"

new_distances <-stringdist(tolower(randomnames$randomnames),new_point, method = distance.method,
                           nthread = getOption("sd_num_thread"),q=2)

D2 <- cbind(D,c(new_distances))
D2 <- rbind(D2,c(new_distances,0))


n <- nrow(D2)
ones <- rep(1, n-1)
w <-c(ones,0)

B <- mds.w.centered(D2^2,w)

mds.w.centered <- function(D,w) {
  
  n <- length(w)
  w <- matrix(w,ncol=1)
  e <- matrix(1,nrow=n,ncol=1)
  s <- sum(w)
  P <- diag(n) - e %*% t(w)/s
  Q <- diag(n) - w %*% t(e)/s
  X <- 0.5 * P %*% D2^2 %*% Q
  return (X) 
  
}

X<-results$Points
Euc_distances<-c(dist(X, method = "euclidean"))

minimizing_func <- function(y,B,X) {
  
  n <- nrow(X)+1
  b <- matrix(B[-n,n],ncol=1)
  beta <- B[n,n]
  y <- matrix(y,ncol=1)
  
  res <- 2*sum((b - X %*% y)^2) + (beta-sum(y^2))^2
  
  attr(res,"gradient") <- -4 * t(X) %*% (b - X %*% y) -
    4 * (beta - sum(y^2)) * y
  return(res)
} 

stdv<-sd(Euc_distances)

y <- nlm(minimizing_func,p=rnorm(2,sd=stdv),B,X)

#y <- nlm(minimizing_func,p=c(0.76030400,1.20691993),B,X)

New_points <- rbind(X,y$estimate)

New_Euc_distances<-c(dist(New_points, method = "euclidean"))

hist(New_Euc_distances,col = "grey", xlim = c(0,25), ylim = c(0,15), main = 'Euclidean distances between predicted names')


