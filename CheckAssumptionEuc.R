library(ggplot2)

ObservedDist <- read.csv("~/Documents/MDS/NewAnalysis/Distributions/bigramdistances.csv",header = FALSE)

predictedDist <- read.csv("~/Documents/MDS/NewAnalysis/Distributions/Predict_bigramdist.csv",header = FALSE)

#simulatedDist <- read.csv("~/Documents/MDS/NewAnalysis/Distributions/simulatednamedist_trigram.csv",header = FALSE)



#boxplot
dat<-data.frame(ObservedDist,predictedDist)
colnames(dat) <- c("ObservedDist", "PredictedDist")
#ggplot(dat, aes(x=ObservedDist, y=PredictedDist)) + geom_boxplot(cex=0.2) +coord_flip()

ggplot(dat, aes(x=ObservedDist, y=PredictedDist)) +
  geom_boxplot(aes(group = cut_width(ObservedDist, 0.05)))

ggplot(dat, aes(x=ObservedDist, y=PredictedDist)) +
  geom_boxplot(aes(group = cut_width(ObservedDist, 0.02)), outlier.alpha = 0.1)







