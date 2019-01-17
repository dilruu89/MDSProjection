library(stringdist)

mat <- matrix(c(0,1,2,1,1,0,1,2,2,1,0,1,1,2,1,0),
              nrow = 4)

a2<-c(6.2,4.5,6.3,4.5)
#a2<-c(0,1,2,1)

A2<- cbind(mat,a2)
A2 <- rbind(A2,c(a2,0))

mat2<-MDS(mat,2)
plot(mat2$Points)

embed_point<-Outofsample_method(A2,mat2$Points)
plot(embed_point)


