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
# same function applied to tot ind must be applied to each frame recursively. double lapply, likely.
# need to find solution for size bins, because they must change for different classes. could use
# max size/100 and see what happens.

# need to run a function over the replicates to identify the highest biomass and use that as safety bin
# for each functional group

frequencies <-function(data) { # function to extract the frequencies for each replicate
  mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
  # community, which still has to be defined anyway
  largestBin <- ceiling(max(mass))#+1000 # trick to have all the bins of the same length
  breaks <- seq(0, largestBin, largestBin/100) # sets the breaks ranging over the biomass of the individuals
  length_classes <- c(1, breaks[2:(length(breaks)-1)]) # gets rid of the 0 and of the last value
  cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) # assigns the biomasses to their bin
  freq <- table(cat)
  freq <- cbind(freq)
  #freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
  freq[freq<2] <- NA # lower limit of resoulution, gets rid of the outliers
  ln_freq <- log(freq) # lognorm transformation of the frequency data
  ln_length <- log(length_classes) # lognormal transformation of the length classes
  freq_breaks <- data.frame(ln_length, ln_freq)
  #freq_breaks <- freq_breaks[c(2:nrow(freq_breaks)),] # cuts out the first bin class (1g)
  
}

disaggregatedFreqList <- lapply(largeList, function(subList) lapply(subList, frequencies))

# frameLengths <- lapply(disaggregatedFreqList, function(subList) lapply(subList, nrow))

# function to take average of the frequency per bin across the replicates for each functional group
# end up with a list of 7 data frames, each for one group.

listOfMatrices <- lapply(disaggregatedFreqList, function(subList) {
  rawMatrix <- abind(subList, along=2)
  cleanMatrix <- data.frame(rawMatrix[,1], rawMatrix[,seq(1, ncol(rawMatrix), by = 2)])
  return(cleanMatrix)
  })


all.matrix <- abind(refined, along=3)
all.matrix[is.na(all.matrix)] <- 0 # gets rid of NAs, check if legit lol
mean_runs <- apply(all.matrix, c(1,2), mean) # gotcha
sd_runs <- apply(all.matrix, c(1,2), sd) # gotcha







