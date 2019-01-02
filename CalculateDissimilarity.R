library(stringdist)
library(gdata)

Surnamefile1 <- read.csv("~/Documents/RScripts/surname.csv",header = TRUE)
#Surnamefile1 <- read.csv("~/Documents/MDS/NewAnalysis/Data/US_Surname.csv",header = TRUE)

distance.method <- "jaccard"

randomnames <- Surnamefile1[sample(nrow(Surnamefile1), 5000),]
randomnames <- as.data.frame(randomnames)
randomnames

#newRow <- data.frame(randomnames='james')
#randomnames <- rbind(randomnames, newRow)

dist.name.enh <- stringdistmatrix(tolower(randomnames$randomnames),
                                  tolower(randomnames$randomnames),
                                  method = distance.method,
                                  nthread = getOption("sd_num_thread"))


rest<-cmdscale(dist.name.enh,6)

results <- MDS(dist.name.enh,6)

ev<-results$EigenValues$values

(Plusev <- ev[ev> 0])
(Negev <- ev[ev< 0])

sum(Plusev)
sum(Negev)

#stressV <- calculateStress(2,20,dist.name.enh)
#SummaryStress(stressV)

#hist(ev,col = "grey", xlab = "Eigenvalues-jaccard", main = "Histogram of eigenvalues")

jaccard_dist <- c(upperTriangle(dist.name.enh, diag=FALSE, byrow=TRUE))
write.csv(c(jaccard_dist),"~/Documents/MDS/NewAnalysis/Distance/jaccrd_distances.csv")

write.csv(rest,"~/Documents/MDS/NewAnalysis/Distance/jaccrd_points.csv")

predictedDist <- c(dist(rest, method = "euclidean"))
write.csv(predictedDist,"~/Documents/MDS/NewAnalysis/Distance/jaccard_euclidean.csv")
