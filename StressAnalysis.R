library(gdata)

calculatestress_point<-function(dissimilaritymatrix,points){
  
  vec2 <- c(upperTriangle(dissimilaritymatrix, diag=FALSE, byrow=TRUE))
  vec1 <- c(dist(points, method = "euclidean"))
  residualfac = sum((vec1-vec2)^2)
  normalizefac = sum(vec1^2)
  stressCMD=sqrt(residualfac/normalizefac)

  return(list("stress"=stressCMD,"residualfac"=residualfac,"normalizefac"=normalizefac, "Eucdist"=vec1))

}


calculateStress<-function(lowerbound, upperbound, dissimilaritymatrix ){

  vec2 <- c(upperTriangle(dissimilaritymatrix, diag=FALSE, byrow=TRUE))
  
  stressvec<-c()
  #results<-c()
  
  for (dim in lowerbound:upperbound){
    
    dist.surname.enh.MDS<-MDS(dissimilaritymatrix,dim)
    dist.surname.euc <- dist(dist.surname.enh.MDS$Points, method = "euclidean")
    vec1 <- c(dist.surname.euc)
    residualfac = sum((vec1-vec2)^2)
    normalizefac = sum(vec1^2)
    
    print("residualfac..")
    print(residualfac)
    print("normalizefac..")
    print(normalizefac)
    
    stressCMD=sqrt(residualfac/normalizefac)
    print("Stress..")
    print(stressCMD)
    stressvec[dim]<-stressCMD
    #results<-rbind(results,dist.surname.enh.MDS)
  }
  
  #res_list <- list("stressvec" = stressvec, "res" = results)
  
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

#write.csv(stressvec,"~/Documents/MDS/NewAnalysis/Distributions/Stress_VS_Dim_AU_lv.csv")
