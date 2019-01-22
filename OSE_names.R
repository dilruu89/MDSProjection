library(stringdist)
library(gdata)

Surnamefile1 <- read.csv("~/Documents/RScripts/surname.csv",header = TRUE)

distance.method <- "qgram"

randomnames <- Surnamefile1[sample(nrow(Surnamefile1), 5000),]
randomnames <- as.data.frame(randomnames)
randomnames


dist.name.enh <- stringdistmatrix(tolower(randomnames$randomnames),
                                  tolower(randomnames$randomnames),
                                  method = distance.method,
                                  nthread = getOption("sd_num_thread"),q=2)

D<-dist.name.enh

results<-MDS(D^2,2)

new_point <-"rom"

new_distances <-stringdist(tolower(randomnames$randomnames),new_point, method = distance.method,
                           nthread = getOption("sd_num_thread"),q=2)

D2 <- cbind(D,c(new_distances))
D2 <- rbind(D2,c(new_distances,0))

embed<-Outofsample_method(D2,results$Points)
plot(results$Points, col='red')
plot(embed, col='blue')

Prev_Euc_distances<-c(dist(results$Points, method = "euclidean"))
New_Euc_distances<-c(dist(embed, method = "euclidean"))


hist(Prev_Euc_distances,col = "grey", xlim = c(0,30), ylim = c(0,3000000), main = 'Euclidean distances between predicted names')
hist(New_Euc_distances,col = "grey", xlim = c(0,30), ylim = c(0,3000000), main = 'Euclidean distances between predicted names')



