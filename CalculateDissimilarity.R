library(stringdist)

Surnamefile1 <- read.csv("~/Documents/RScripts/surname.csv",header = TRUE)

distance.method <- "jw"

randomnames <- Surnamefile1[sample(nrow(Surnamefile1), 1000),]
randomnames <- as.data.frame(randomnames)
randomnames

dist.name.enh <- stringdistmatrix(tolower(randomnames$randomnames),
                                  tolower(randomnames$randomnames),
                                  method = distance.method,
                                  nthread = getOption("sd_num_thread"),q=2)

results <- MDS(dist.name.enh,6)

ev<-results$EigenValues$values

(Plusev <- ev[ev> 0])
(Negev <- ev[ev< 0])

sum(Plusev)
sum(Negev)

stressV <- calculateStress(2,20,dist.name.enh)
SummaryStress(stressV)

