library(stringdist)
library(gdata)

Surnamefile1 <- read.csv("~/Documents/RScripts/surname.csv",header = TRUE)

distance.method <- "jw"

randomnames <- Surnamefile1[sample(nrow(Surnamefile1), 10),]
randomnames <- as.data.frame(randomnames)
randomnames


dist.name.enh <- stringdistmatrix(tolower(randomnames$randomnames),
                                  tolower(randomnames$randomnames),
                                  method = distance.method,
                                  nthread = getOption("sd_num_thread"),q=2)

stress_analysis(dist.name.enh)

#D<-dist.name.enh

dissimilaritymatrix<-dist.name.enh
results_<-MDS(dissimilaritymatrix^2,2)

points<-results_$Points


#s<-calculateStress(0,5,D^2)
#SummaryStress(s)


#results_<-MDS(D^2,2)
#points<-results_$Points
#calculatestress_point(D,points)

#iterative calculate stress
stress_analysis<-function(D){
stressv<-c()
for (dim in 2:20){
  results_<-MDS(D^2,dim)
  t<-calculatestress_point(D,results_$Points)
  print(t$stress)
  stressv[dim]<-t$stress
}

#print(results_$EigenValues$values)
plot(stressv)
}

#write.csv(stressv,"~/Documents/MDS/NewAnalysis/Stress/stress_jw.csv")

