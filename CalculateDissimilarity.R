library(stringdist)

Surnamefile1 <- read.csv("~/Documents/RScripts/surname.csv",header = TRUE)

distance.method <- "lv"

randomnames <- Surnamefile1[sample(nrow(Surnamefile1), 5000),]
randomnames <- as.data.frame(randomnames)
randomnames

#newRow <- data.frame(randomnames='james')
#randomnames <- rbind(randomnames, newRow)

dist.name.enh <- stringdistmatrix(tolower(randomnames$randomnames),
                                  tolower(randomnames$randomnames),
                                  method = distance.method,
                                  nthread = getOption("sd_num_thread"),q=2)

results <- MDS(dist.name.enh,6)

results <- cmdscale(dist.name.enh,eig=TRUE)

ev<-results$EigenValues$values

(Plusev <- ev[ev> 0])
(Negev <- ev[ev< 0])

sum(Plusev)
sum(Negev)

stressV <- calculateStress(2,20,dist.name.enh)
SummaryStress(stressV)

hist(ev,col = "grey", xlab = "Eigenvalues-lv", main = "Histogram of eigenvalues")

