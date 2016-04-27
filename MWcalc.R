# 16.04.2016

require(abind)
setwd("/home/somros/Documents/itn100results/sizeSpectrum2000/spectrumMetricsTables/100")
list<-list.files("/home/somros/Documents/itn100results/sizeSpectrum2000/spectrumMetricsTables/100", 
                 recursive=TRUE, pattern="*.csv")

length.list<-length(list)
read.special<-function(x) {
  read.table(x, header=TRUE, sep=',', dec='.') # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special)

baseFrame <- data_list[[1]]

# routine for the Mann-Whitney test of each metric against the baseline metrics

k <- length(data_list)
l <- length(data_list[[1]])
mannWhitney <- vector(mode="list", k)


for(i in 1:k) {
  mannWhitney[[i]] <- NaN*seq(l)
  for (j in 1:l) {
  mannWhitney[[i]][j] <- wilcox.test(data_list[[i]][,j], data_list[[1]][,j])$p.value
  }
}

pValues <- as.data.frame(abind(mannWhitney, along=0))[,-1] # p-val of MW test against base
colnames(pValues) <- c("slope", "intercept", "slpErr", "intErr", "rsquared", "dof")
write.csv(pValues, 
          "/home/somros/Documents/itn100results/sizeSpectrum2000/spectrum_pValues/pValues100.csv")

# calculate mean metrics for each regime

colMeanAndSd <- function(x) {
  sdColumns <- list()
  for (i in 1:length(x)) {
    sdColumns[[i]] <- sd(x[,i])
  }
  meanColumns <- colMeans(x)
  framedMetrics <- data.frame(meanColumns, as.matrix(unlist(sdColumns)))
  colnames(framedMetrics) <- c("mean", "sd")
  framedMetricsFinal <- framedMetrics[-1,] 
  return(framedMetricsFinal)
}

medie <- lapply(data_list, colMeanAndSd)
medieFrame <- abind(medie, along=2)
write.csv(medieFrame, 
          "/home/somros/Documents/itn100results/sizeSpectrum2000/spectrum_pValues/means100.csv")



