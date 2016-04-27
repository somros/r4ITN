# 15.04.2016 script to calculate regression parameters for each file, average them, plot the
# resulting regressions, MW them against baseline.

require(abind)
setwd("/home/somros/Documents/itn100results/sizeSpectrum2000/S250/i3")
list<-list.files("/home/somros/Documents/itn100results/sizeSpectrum2000/S250/i3", 
                 recursive=TRUE, pattern="*.csv") 
length.list<-length(list)
read.special<-function(x) {
  read.table(x, header=TRUE, sep='\t', dec='.') # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special) # stores all the files into a huge list

spectrumMetrics <-function(data) { # function to extract the frequencies for each replicate
  mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
  # community, which still has to be defined anyway
  breaks <- seq(0, ceiling(max(mass))+10, 100) # sets the breaks ranging over the biomass of the individuals
  length_classes <- c(1, breaks[2:(length(breaks)-1)]) # gets rid of the 0 and of the last value
  cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) # assigns the biomasses to their bin
  freq <- table(cat)
  freq <- cbind(freq)
  #freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
  freq[freq<2] <- NA # lower limit of resoulution, gets rid of the outliers
  ln_freq <- log(freq) # lognorm transformation of the frequency data
  ln_length <- log(length_classes) # lognormal transformation of the length classes
  freq_breaks <- data.frame(ln_length, ln_freq)
  fitExperiment <- lm(freq_breaks$freq ~ freq_breaks$ln_length)#
  intercept <- coef(summary(fitExperiment))[1,1]
  slope <- coef(summary(fitExperiment))[2,1]
  intErr <- coef(summary(fitExperiment))[1,2]
  slpErr <- coef(summary(fitExperiment))[2,2]
  rsquared <- summary(fitExperiment)$r.squared
  dof <- summary(fitExperiment)$df
  return(c(slope, intercept, slpErr, intErr, rsquared, dof))
  #return(freq_breaks)
  
}

metricsList <- lapply(data_list, spectrumMetrics)
metricsFrame <- as.data.frame(abind(metricsList, along=0))[,-c(6,8)]
colnames(metricsFrame) <- c("slope", "intercept", "slpErr", "intErr", "rsquared", "dof")
write.csv(metricsFrame, 
          "/home/somros/Documents/itn100results/sizeSpectrum2000/spectrumMetricsTables/100/S250_I3.csv")



