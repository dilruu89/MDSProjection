Delta2 <- matrix(c(0,100,45,45,100,0,45,45,45,45,0,64,45,45,64,0),byrow=T,ncol=4)

res<-MDS(Delta2,2)

a2 <- c(386,386,457,457)
A2 <- cbind(Delta2,a2)
A2 <- rbind(A2,c(a2,0))

#example 22222

Delta2 <- matrix(c(0,0.6019542,1.3586267,0.601952,0,0.7940753,1.3586267,0.7940753,0),byrow =T,ncol=3)

res<-MDS(Delta2,2)

a2 <- c(25.15667, 25.39333, 25.55864)
A2 <- cbind(Delta2,a2)
A2 <- rbind(A2,c(a2,0))
