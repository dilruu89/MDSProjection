
pc<-3
group.file<-NULL
active <-dist.name.enh

  res <- list ()
  D <- active^2
  I <- diag(1, nrow(active))

  ONES <- matrix(1, nrow = nrow(active), ncol = 1)
  m<-matrix(1/nrow(active),nrow = nrow(active), ncol = 1)
  BigI<-I-(ONES%*%t(m))
  
  #compute active cross-product matrix
  S <-  -0.5 * (BigI %*% D %*% t(BigI))
  eigen <- eigen(S)
  res$eigen <- round(eigen$values[1:pc], 3)
  eigen.perc <- (abs(eigen$values) * 100) / sum(eigen$values[eigen$values>0])
  res$eigen.perc <- round(eigen.perc[1:pc], 3)
  
  #only positive eigenvalues are kept
  eigen$vectors <- eigen$vectors[, eigen$values > 0]
  eigen$values <- eigen$values[eigen$values > 0]
  
  #res$eigen <- round(eigen$values[1:pc], 3)
  res$source<-list()
  res$source$D <- D
  res$source$m<-m
  
  #check principal components
  if (pc < 2)
    pc <- 3
  if (pc > length(eigen$values))
    pc <- length(eigen$values)

  #compute active matrix of factor scores
  F <- diag(as.vector(m)^(-0.5)) %*% eigen$vectors %*% diag(eigen$values^0.5)
  
  coord <- data.frame(F[, 1:pc]) #points
  rownames(coord) <- rownames(active)
  colnames(coord) <- paste ("PC", (1:pc), sep = "")
  res$coord = round(coord, 3)
  res$group<-matrix(c("NoGroup","black"),1)
  colnames(res$group)<-c("group","color")
  
  #res$active.group<-as.data.frame.matrix(res$group)
  res$col<-matrix(c("","NoGroup","black"),1)
  colnames(res$col)<-c("element","group","color")
  #res$col<-as.data.frame.matrix(res$col)
  
  class(res) <- c("mmds")
  if(!is.null(group.file)){
    res<-col.group(res,group.file)
  }
  
  
  
  
  
