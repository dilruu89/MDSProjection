library(stringdist)

mat <- matrix(c(0,1,2,1,1,0,1,2,2,1,0,1,1,2,1,0),
              nrow = 4)

a2<-c(6.2,4.5,6.3,4.5)

#newmat <- matrix(c(mat,t(t(a2)),t(a2),0), nrow =5)


matd <- matrix(c(0,1,2,1,6.2,
                
                1,0,1,2,4.5,
                
                2,1,0,1,6.3,
                
                1,2,1,0,4.5,
                
                6.2,4.5,6.3,4.5,0
                
                ),
              nrow = 5)

mat2<-MDS(mat,2)

dist<-dist(mat2$Points, method = "euclidean")

cmdscale(mat)


