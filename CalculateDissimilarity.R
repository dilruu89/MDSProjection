library(stringdist)

#Surnamefile1 <- read.csv("~/Documents/RScripts/surname.csv",header = TRUE)
Surnamefile1 <- read.csv("~/Documents/MDS/NewAnalysis/Data/US_Surname.csv",header = TRUE)

distance.method <- "qgram"

randomnames <- Surnamefile1[sample(nrow(Surnamefile1), 5000),]
randomnames <- as.data.frame(randomnames)
randomnames

#newRow <- data.frame(randomnames='james')
#randomnames <- rbind(randomnames, newRow)

dist.name.enh <- stringdistmatrix(tolower(randomnames$randomnames),
                                  tolower(randomnames$randomnames),
                                  method = distance.method,
                                  nthread = getOption("sd_num_thread"))

results <- MDS(dist.name.enh,6)

ev<-results$EigenValues$values

(Plusev <- ev[ev> 0])
(Negev <- ev[ev< 0])

sum(Plusev)
sum(Negev)

stressV <- calculateStress(2,20,dist.name.enh)
SummaryStress(stressV)

#hist(ev,col = "grey", xlab = "Eigenvalues-jaccard", main = "Histogram of eigenvalues")

write.csv(c(dist.name.enh),"~/Documents/MDS/NewAnalysis/Distances/unigram_distances.csv")
write.csv(results$Points,"~/Documents/MDS/NewAnalysis/Distances/unigram_points.csv")

predictedDist <- c(dist(results$Points, method = "euclidean"))

write.csv(predictedDist,"~/Documents/MDS/NewAnalysis/Distances/unigram_euclidean.csv")

