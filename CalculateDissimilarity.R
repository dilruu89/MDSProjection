library(stringdist)
library(gdata)

Surnamefile1 <- read.csv("~/Documents/RScripts/surname.csv",header = TRUE)
#Surnamefile1 <- read.csv("~/Documents/MDS/NewAnalysis/Data/US_Surname.csv",header = TRUE)

distance.method <- "qgram"

randomnames <- Surnamefile1[sample(nrow(Surnamefile1), 10),]
randomnames <- as.data.frame(randomnames)
randomnames

#newnames <- data.frame(randomnames=c("james","jane"))
#randomnames <- rbind(randomnames, newnames)

dist.name.enh <- stringdistmatrix(tolower(randomnames$randomnames),
                                  tolower(randomnames$randomnames),
                                  method = distance.method,
                                  nthread = getOption("sd_num_thread"),q=2)


#rest<-cmdscale(dist.name.enh,6)

results <- MDS(D,2)

ev<-results$EigenValues$values

(Plusev <- ev[ev> 0])
(Negev <- ev[ev< 0])

sum(Plusev)
sum(Negev)

#stressV <- calculateStress(2,20,dist.name.enh)
#SummaryStress(stressV)

#hist(ev,col = "grey", xlab = "Eigenvalues-jaccard", main = "Histogram of eigenvalues")

bigram_dist <- c(upperTriangle(dist.name.enh, diag=FALSE, byrow=TRUE))
write.csv(c(bigram_dist),"~/Documents/MDS/NewAnalysis/Distance/bigram_dist_distances.csv")

write.csv(rest,"~/Documents/MDS/NewAnalysis/Distances/bigram_dist_points.csv")

predictedDist <- c(dist(rest, method = "euclidean"))
write.csv(predictedDist,"~/Documents/MDS/NewAnalysis/Distance/bigram_dist_euclidean.csv")
