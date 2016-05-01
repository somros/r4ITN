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
 
listOfDisaggregatedLists <- lapply(data_list, functionalGroupSeparator)

test <- functionalGroupSeparator(data_list[[1]])
head(test[[2]])

classes <- length(levels(data_list[[1]]$class.))
replicates <- length(listOfDisaggregatedLists)
nestList <- vector("list", replicates)
largeList <- list() # must have length classes

for (i in 1:classes) { # loop to initiate list of lists. one instance of nestList is attributed to each element of largelist
  largeList[[i]] <- nestList
}

# now actual loop

for (j in 1:classes) {
  for (k in 1:replicates) {
    for (l in 1:classes) {
    largeList[[l]][[k]] <- listOfDisaggregatedLists[[k]][[l]]
    }
  }
}

# now spectra routines must be applied to averages


