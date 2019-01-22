file1 <- read.csv("~/Downloads/air_expe.csv",header = TRUE)

sim <-c(file1$x2)

sim_m <- c(dist(sim, method = "euclidean"))

n <- 13
mat_1 <- matrix(0, ncol = n, nrow = n)
mat_1[lower.tri(mat_1)] <- sim_m
mat_1[upper.tri(mat_1)] <- sim_m
mat_1

res_ <-MDS(mat_1,2)

stress_analysis(mat_1)
