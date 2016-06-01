# 16.04.2016

require(abind)
setwd("/home/somros/Documents/itn100results/sizeSpectrum2000/spectrumMetricsTables/log10")
listOfFiles<-list.files("/home/somros/Documents/itn100results/sizeSpectrum2000/spectrumMetricsTables/log10", 
                 recursive=TRUE, pattern="*.csv")
listOfFiles <- as.character(levels(factor(listOfFiles, levels = c("base.csv", "U_I2.csv", "U_I3.csv", "S500_I2.csv",
                                                           "S500_I3.csv", "S250_I2.csv", "S250_I3.csv"))))

length.list<-length(listOfFiles)
read.special<-function(x) {
  read.table(x, header=TRUE, sep=',', dec='.') # custom function to read the batches of .csv keeping the header
}
dataList <- lapply(listOfFiles, read.special)

baseFrame <- dataList[[1]]

# routine for the Mann-Whitney test of each metric against the baseline metrics

k <- length(dataList)
l <- length(dataList[[1]])
mannWhitney <- vector(mode="list", k)


for(i in 1:k) {
  mannWhitney[[i]] <- NaN*seq(l)
  for (j in 1:l) {
  mannWhitney[[i]][j] <- wilcox.test(dataList[[i]][,j], dataList[[1]][,j])$p.value
  }
}

pValues <- as.data.frame(abind(mannWhitney, along=0))[,-1] # p-val of MW test against base
colnames(pValues) <- c("slope", "intercept", "slpErr", "intErr", "rsquared", "dof")
write.csv(pValues, 
          "/home/somros/Documents/itn100results/sizeSpectrum2000/spectrum_pValues/pValuesLog10.csv")







# calculate mean metrics for each regime

colMeanAndSd <- function(x) {
  redx <- data.frame(x$slope, x$intercept)
  sdColumns <- list()
  for (i in 1:length(redx)) {
    sdColumns[[i]] <- sd(redx[,i])
  }
  meanColumns <- colMeans(redx)
  framedMetrics <- data.frame(meanColumns, as.matrix(unlist(sdColumns)))
  colnames(framedMetrics) <- c("mean", "sd")
  framedMetricsFinal <- framedMetrics#[-1,] 
  return(framedMetricsFinal)
}

medie <- lapply(dataList, colMeanAndSd)
medieFrame <- abind(medie, along=2)
write.csv(medieFrame, 
          "/home/somros/Documents/itn100results/sizeSpectrum2000/spectrum_pValues/means100.csv")



