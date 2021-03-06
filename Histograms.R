library(ggplot2)

ObservedDist <- read.csv("~/Documents/MDS/NewAnalysis/Distributions/Ob_trigrams.csv",header = FALSE)

predictedDist <- read.csv("~/Documents/MDS/NewAnalysis/Distributions/trigrams.csv",header = FALSE)

simulatedDist <- read.csv("~/Documents/MDS/NewAnalysis/Distributions/simulatednamedist_trigram.csv",header = FALSE)

simulatedDist<-veceu

#plot for the observed distances
qplot(bigram_dist,
      geom="histogram", 
      main = "Histogram for bigram distances between real names",
      binwidth = 0.5, xlim=c(0,2))

qplot(simulatedDist,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Euclidean distances in simulated names", 
      xlab = "Euclidean distances between simulated data points",
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(-5,30))


library("ggpubr")


sampledist <- sample(predictedDist,5000,replace = FALSE)
ggdensity(sampledist)
ggqqplot(sampledist)
shapiro.test(sampledist)
qqPlot(sampledist)
