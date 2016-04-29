# script for size spectra disaggregated on the functional groups
# 29.04.2016. Alberto Rovellini, Victoria University of Wellington


 
require(abind)
require(plyr)
setwd("/home/somros/Documents/itn100results/sizeSpectrum2000/demo")
list<-list.files("/home/somros/Documents/itn100results/sizeSpectrum2000/demo", 
                recursive=TRUE, pattern="*.csv") 
length.list<-length(list)
read.special<-function(x) {
 read.table(x, header=TRUE, sep='\t', dec='.') # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special) # stores all the files into a huge list

functionalGroupSeparator <- function(dataFrame) {
  dataFrame$class. <- as.character(dataFrame$class.)
  dataFrame$class. <- factor(dataFrame$class., levels = unique(dataFrame$class.))
  splitFrames <- split(dataFrame, dataFrame$class.)
  return(splitFrames)
}
 
bunchOfData <- lapply(data_list, functionalGroupSeparator)

test <- functionalGroupSeparator(data_list[[1]])
head(test[[2]])

classes <- length(levels(data_list[[1]]$class.))
replicates <- length(bunchOfData)
nestList <- vector("list", classes)
largeList <- vector("list", replicates)

##### to be continued. loop is probably not the best idea ever

for (i in 1:replicates) {
  
}


 for (i in 1:length(bunchOfData)) {
   for (j in 1:classes) {
     newList[[j]] <- bunchOfData[[i]][[j]] 
   }
 }



