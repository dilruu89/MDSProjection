mmds <- res

if (!inherits(mmds,"mmds"))
  stop("mmds is not a 'mmds' object")
active <- mmds$source$D

#results will be stored in a list
res <- list ()

#MDS of active data
D<-active
#identity matrix
I <- diag(1, nrow(active))

#active matrix of ones
ONES <- matrix(1, nrow = nrow(active), ncol = 1)
#W vector of mass
m<-mmds$source$m
BigI<- I - (ONES%*%t(m))
#compute active cross-product matrix
S <-  -0.5 * BigI %*% D %*% t(BigI)
eigen <- eigen(S)
#only positive eigenvalues are kept
eigen$vectors <- eigen$vectors[, eigen$values > 0]
eigen$values <- eigen$values[eigen$values > 0]

#check principal components
if (pc < 2)
  pc <- 3
if (pc > length(eigen$values))
  pc <- length(eigen$values)

#eigenvalues are transformed into percentage


#compute active matrix of factor scores
F <- diag(as.vector(m)^(-0.5)) %*% eigen$vectors %*% diag(eigen$values^0.5)

sup <-dist.name.enh

Dsup <- sup^2

#supplementary matrix of ones
ONESsup <- matrix(1, nrow = nrow(sup), ncol = 1)

#compute supplementary cross-product matrix
Ssup <-  -0.5 * (BigI %*% (t(Dsup) -( D %*% m  %*% t(ONESsup))))

#compute supplementary matrix of factor scores
Fsup <- t(Ssup) %*% F %*% diag(eigen$values^-1)
coord <- data.frame(Fsup[, 1:pc])
rownames(coord) <- rownames(sup)
colnames(coord) <- paste ("PC", (1:pc), sep = "")
res$coord <- round(coord, 3)
res$group<-matrix(c("NoGroup","magenta"),1)
colnames(res$group)<-c("group","color")
#res$group<-as.data.frame.matrix(res$group)
res$col<-matrix(c("","NoGroup","magenta"),1)
colnames(res$col)<-c("element","group","color")
#res$col<-as.data.frame.matrix(res$col)






