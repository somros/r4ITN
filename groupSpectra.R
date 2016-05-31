# script for size spectra disaggregated on the functional groups
# 29.04.2016. Alberto Rovellini, Victoria University of Wellington


require(abind)
require(plyr)
require(ggplot2)
setwd("/home/somros/Documents/itn100results/sizeSpectrum2000/S250/i3")
list<-list.files("/home/somros/Documents/itn100results/sizeSpectrum2000/S250/i3",
                recursive=TRUE, pattern="*.csv")
length.list<-length(list)
read.special<-function(x) {
 read.table(x, header=TRUE, sep='\t', dec='.') # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special) # stores all the files into a huge list
funGroupsNames <- c("smallpelagic", "mediumpelagic", "largepelagic", "smalldemersal",
                    "mediumdemersal", "largedemersal", "topcarnivores")



functionalGroupSeparator <- function(dataFrame) { 
  dataFrame$class. <- as.character(dataFrame$class.)
  dataFrame$class. <- factor(dataFrame$class., levels = funGroupsNames)
  splitFrames <- split(dataFrame, dataFrame$class.)
  return(splitFrames)
}


listOfDisaggregatedLists <- lapply(data_list, functionalGroupSeparator)

# test <- functionalGroupSeparator(data_list[[1]])
# head(test[[2]])
# emptyFrame <- head(largeList[[2]][[7]])


classes <- length(funGroupsNames) # 
replicates <- length(listOfDisaggregatedLists)
nestList <- vector("list", replicates)
largeListtmp <- list() # must have length classes

for (i in 1:classes) { # loop to initiate list of lists. one instance of nestList is attributed to each element of largelist
  largeListtmp[[i]] <- nestList
}

# now actual loop

for (j in 1:classes) {
  for (k in 1:replicates) {
    for (l in 1:classes) {
    largeListtmp[[l]][[k]] <- listOfDisaggregatedLists[[k]][[l]]
    }
  }
}

# need to assign NAs to empty frames!

largeList <- list()
for (i in 1:classes) { # loop to initiate list of lists. one instance of nestList is attributed to each element of largelist
  largeList[[i]] <- nestList
}


for (j in 1:classes) {
  for (k in 1:replicates) {
    for (l in 1:classes) {
      if (nrow(largeListtmp[[l]][[k]])==0) {
        largeList[[l]][[k]] <- NA
      } else {
        largeList[[l]][[k]] <- largeListtmp[[l]][[k]]  
      }
    }
  }
}

# as.data.frame(matrix(rep(0, length(largeListtmp[[l]][[k]])),
#                      nrow = 1, ncol = length(largeListtmp[[l]][[k]]),
#                      dimnames = list("row1", names(largeListtmp[[l]][[k]]))))

largeListFinal <- lapply(largeList, function(subList) {
  subList[is.na(subList)] <- NULL
  return(subList)
})


# function to detect the highest biomass for each functional group across all the replicates

highestBiomass <- function(listOfReplicates) {
  listMaxB <- vector(mode="list", length=length(listOfReplicates))
  for (i in 1:length(listOfReplicates)) {
    listMaxB[[i]] <- max(listOfReplicates[[i]]$biomass) 
  }
  vectorMaxB <- unlist(listMaxB)
  maxBFunGroup <- max(vectorMaxB)
  return(maxBFunGroup)
}

maxBins <- data.frame(funGroupsNames, unlist(lapply(largeListFinal, highestBiomass))) 
colnames(maxBins) <- c("funGroup", "maxBiomass")

# modify spectra calculator to dynamically apply the correct bin to the correct class


frequencies <-function(data) { # function to extract the frequencies for each replicate
  mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
  functionalGroup <- as.character(levels(factor(data$class))) # need to drop unused levels
  largestBin <- ceiling(maxBins[maxBins$funGroup==functionalGroup,2])#+1000 # trick to have all the bins of the same length
  breaks <- seq(0, largestBin, largestBin/50) # sets the breaks ranging over the biomass of the individuals
  length_classes <- c(1, breaks[2:(length(breaks)-1)]) # gets rid of the 0 and of the last value
  cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) # assigns the biomasses to their bin
  freq <- table(cat)
  freq <- cbind(freq)
  #freq[freq==0] <- NA # turn zeroes to NAs for the sake of the plot
  freq[freq<5] <- NA # lower limit of resoulution, gets rid of the outliers
  ln_freq <- log(freq) # lognorm transformation of the frequency data
  ln_length <- log(length_classes) # lognormal transformation of the length classes
  freq_breaks <- data.frame(ln_length, ln_freq)
  #freq_breaks <- freq_breaks[c(2:nrow(freq_breaks)),] # cuts out the first bin class (1g)
  
}

disaggregatedFreqList <- lapply(largeListFinal, function(subList) lapply(subList, frequencies))

# frameLengths <- lapply(disaggregatedFreqList, function(subList) lapply(subList, nrow))

# function to take average of the frequency per bin across the replicates for each functional group
# end up with a list of 7 data frames, each for one group.

listOfMatrices <- lapply(disaggregatedFreqList, function(subList) {
  rawMatrix <- abind(subList, along=2)
  repMatrix <- data.frame(rawMatrix[,seq(2, ncol(rawMatrix), by = 2)])
  repMatrix[is.na(repMatrix)] <- 0
  meanFrequency <- apply(repMatrix, 1, mean)
  sdFrequency <- apply(repMatrix, 1, sd)
  cleanMatrix <- data.frame(rawMatrix[,1], meanFrequency, sdFrequency)
  cleanMatrix[cleanMatrix==0] <- NA
  colnames(cleanMatrix) <- c("logWeight", "logAbundance", "sdAbundance")
  return(cleanMatrix)
})

# at this point: a list of 7 dataframes, each about the abundance per weight bin of a given functional group
# write a ggplot function and apply to it (output is going to suck). facet if possible.

plotFNSpectrum <- ggplot(data=listOfMatrices[[2]], aes(x=logWeight, y=logAbundance))+
  geom_point()
plotFNSpectrum

# as alternative, put them all together in one frame and melt them and use one graph to capture them all

for (i in 1:length(listOfMatrices)) {
  listOfMatrices[[i]]$funGroup <- rep(funGroupsNames[i], nrow(listOfMatrices[[i]]))
}

continuousSpectrum <- as.data.frame(abind(listOfMatrices, along=1))

# fix classes of columns

continuousSpectrum$logWeight <- as.numeric(levels(continuousSpectrum$logWeight))[continuousSpectrum$logWeight]
continuousSpectrum$logAbundance <-as.numeric(levels(continuousSpectrum$logAbundance))[continuousSpectrum$logAbundance]
continuousSpectrum$sdAbundance <- as.numeric(levels(continuousSpectrum$sdAbundance))[continuousSpectrum$sdAbundance]
continuousSpectrum$funGroup <- factor(continuousSpectrum$funGroup, levels = unique(continuousSpectrum$funGroup))

library(RColorBrewer)
par(mar = c(0, 4, 0, 0))
display.brewer.all()
doublePalette <- brewer.pal(9, "YlOrRd")
myPalette <- doublePalette[seq(3,length(doublePalette),1)]

# for colour key, might want to order the groups according to their dimensions.

orderedGroups <- maxBins[order(maxBins$maxBiomass),][,1]

continuousSpectrum$funGroup <- factor(continuousSpectrum$funGroup, levels = orderedGroups)

plotFNSpectrum <- ggplot(data=continuousSpectrum[!is.na(continuousSpectrum$logAbundance),], 
                         aes(x=logWeight, y=logAbundance, group=funGroup))+
  #geom_point(aes(shape=funGroup))+
  geom_line(aes(colour=funGroup), size=1)+
  scale_x_continuous(name="ln(weight)", 
                     limits=c(3,10),
                     breaks=seq(3,10,1))+
  scale_y_continuous(name="ln(number of individuals)", 
                     limits=c(0,10),
                     breaks=seq(0,10,1))+
  scale_colour_manual(values = myPalette)+
  theme(panel.background = element_rect(fill = 'white'))+
  #theme
  theme_bw()+
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())+
  theme(plot.title = element_text(size=14, vjust=2))+
  theme(axis.title.x = element_text(size=10,vjust=-0.5),
        axis.title.y = element_text(size=10,vjust=0.5))+
  theme(axis.text.x=element_text(size=10))+
  theme(axis.text.y=element_text(size=10))
plotFNSpectrum

ggsave("/home/somros/Documents/paperFishAndFisheries/pics/disaggregatedSpectra/s250i3.pdf", 
       plotFNSpectrum, useDingbats=FALSE)

