library(stringdist)

Surnamefile1 <- read.csv("~/Documents/RScripts/surname.csv",header = TRUE)

distance.method <- "qgram"

randomnames <- Surnamefile1[sample(nrow(Surnamefile1), 10),]
randomnames <- as.data.frame(randomnames)
randomnames

newRow <- data.frame(randomnames='james')
randomnames <- rbind(randomnames, newRow)

dist.name.enh <- stringdistmatrix(tolower(randomnames$randomnames),
                                  tolower(randomnames$randomnames),
                                  method = distance.method,
                                  nthread = getOption("sd_num_thread"),q=3)

results <- MDS(dist.name.enh,6)

ev<-results$EigenValues$values

(Plusev <- ev[ev> 0])
(Negev <- ev[ev< 0])

sum(Plusev)
sum(Negev)

stressV <- calculateStress(2,20,dist.name.enh)
SummaryStress(stressV)

hist(eigen(results$InnerProduct)$values)
sort(eigen(results$InnerProduct)$values)
