library(stringdist)
library(gdata)

Surnamefile1 <- read.csv("~/Documents/RScripts/surname.csv",header = TRUE)

distance.method <- "qgram"

randomnames_it <- Surnamefile1[sample(nrow(Surnamefile1), 10),]
randomnames_it <- as.data.frame(randomnames_it)
randomnames_it


dist.name.enh <- stringdistmatrix(tolower(randomnames_it$randomnames_it),
                                  tolower(randomnames_it$randomnames_it),
                                  method = distance.method,
                                  nthread = getOption("sd_num_thread"),q=2)


D<-dist.name.enh

results<-MDS(D^2,2)

new_point <-"kontopoulos"

newname <- data.frame(randomnames_it=c(new_point))
randomnames_it <- rbind(randomnames_it, newname)

new_distances <-stringdist(tolower(randomnames_it$randomnames_it),new_point, method = distance.method,
                           nthread = getOption("sd_num_thread"),q=2)

D2 <- cbind(D,c(new_distances))
D2 <- rbind(D2,c(new_distances,0))

embed<-Outofsample_method(D2,results$Points)

embed<-Outofsample_method(D2,embed)


#----2-D plots-----------------------------

plot(results$Points, col='red')
plot(embed, col='blue')

#-----Predicted distances------------------

Prev_Euc_distances<-c(dist(results$Points, method = "euclidean"))
New_Euc_distances<-c(dist(embed, method = "euclidean"))


hist(Prev_Euc_distances,col = "grey", xlim = c(0,30), ylim = c(0,3000000), main = 'Euclidean distances between predicted names')
hist(New_Euc_distances,col = "grey", xlim = c(0,30), ylim = c(0,3000000), main = 'Euclidean distances between predicted names')

#-----obseved distances--------------------

ObservedDist <- c(upperTriangle(D, diag=FALSE, byrow=TRUE))
New_ObservedDist <- c(upperTriangle(D2, diag=FALSE, byrow=TRUE))

hist(ObservedDist,col = "grey", xlim = c(0,30), ylim = c(0,4000000), main = 'Bigram distances between predicted names')
hist(New_ObservedDist,col = "grey", xlim = c(0,30), ylim = c(0,4000000), main = 'Bigram distances between predicted names')


