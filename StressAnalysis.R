library(gdata)

calculateStress<-function(lowerbound, upperbound, dissimilaritymatrix ){

  vec2 <- c(upperTriangle(dissimilaritymatrix, diag=FALSE, byrow=TRUE))
  
  stressvec<-c()
  
  for (dim in lowerbound:upperbound){
    
    dist.surname.enh.MDS<-MDS(dissimilaritymatrix,dim)
    dist.surname.euc <- dist(dist.surname.enh.MDS$Points, method = "euclidean")
    vec1 <- c(dist.surname.euc)
    residualfac = sum((vec1-vec2)^2)
    normalizefac = sum(vec1^2)
    
    stressCMD=sqrt(residualfac/normalizefac)
    stressvec[dim]<-stressCMD
  }
  
  return(stressvec)
}

SummaryStress<-function(stressvec){

  print(stressvec)
  sort(stressvec)
  min(stressvec, na.rm=TRUE)
  dim<-which.min(stressvec)
  
  plot(stressvec,type = 'o', col='red', xlab = 'Dimensions', ylab = 'Stress', main = 'Stress changes with dimension in MDS',cex=.4)
  #lines(stressvec2,type = 'o',col='blue', cex=.4)

}

write.csv(stressvec,"~/Documents/MDS/NewAnalysis/Distributions/Stress_VS_Dim_AU_lv.csv")