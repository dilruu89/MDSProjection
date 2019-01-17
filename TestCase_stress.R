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

D<-dist.name.enh


s<-calculateStress(0,5,D^2)
SummaryStress(s)

results_<-MDS(D^2,5)

points<-results_$Points

calculatestress_point(D,points)

stress<-c(0.7967197,0.5675386,0.489663,0.4414494)
plot(stress)
