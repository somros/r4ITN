# 18.03.2016

# script to subset the individuals files to 2000 time step. not automated

setwd("/home/somros/Documents/itn100results/size250_i3/ind")
listOfFiles<-list.files("/home/somros/Documents/itn100results/size250_i3/ind", 
                 recursive=TRUE, pattern="*.csv")
lastTimeStep <- function(data) subset(data, data$time=="2000.0") # isolates the last time step, comment out for complete analysis

for (i in 1:length(listOfFiles)) {
  
  whole <- read.csv(listOfFiles[i], header=TRUE, sep='\t', dec=',')
  finalStep <- lastTimeStep(whole)
  fileName = paste('/home/somros/Documents/itn100results/sizeSpectrum2000/S250/i3/individuals@2000_', i, '.csv',sep='')
  write.table(finalStep, file = fileName, sep='\t', row.names = FALSE)

}
