library(stringdist)
library(gdata)

Surnamefile1 <- read.csv("~/Documents/RScripts/surname.csv",header = TRUE)
#urnamefile1 <- read.csv("~/Documents/MDS/NewAnalysis/data/US_Surname.csv",sep = ";",stringsAsFactors = FALSE)

distance.method <- "qgram"

randomnames <- Surnamefile1[sample(nrow(Surnamefile1), 5000),]
randomnames <- as.data.frame(randomnames)
randomnames


dist.name.enh <- stringdistmatrix(tolower(randomnames$randomnames),
                                  tolower(randomnames$randomnames),
                                  method = distance.method,
                                  nthread = getOption("sd_num_thread"),q=2)

stress_vec<-stress_analysis(dist.name.enh)

results_<-MDS(dist.name.enh^2,12)
predictedDist <- c(dist(results_$Points, method = "euclidean"))

results_$EigenValues$values

-diff(results_$EigenValues$values)

points<-results_$Points


#iterative calculate stress
stress_analysis<-function(D){
stressv<-c()
for (dim in 2:25){
  results_<-MDS(D^2,dim)
  t<-calculatestress_point(D,results_$Points)
  print(t$stress)
  stressv[dim]<-t$stress
}
plot(stressv)
return(stressv)
}

write.csv(stress_vec,"~/Documents/MDS/NewAnalysis/Stress/Stress_vs_Dim_jaccard_2.csv")

