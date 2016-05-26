# Alberto Rovellini, ZMT Bremen. 24.11.2015
# alberto.rovellini@gmail.com

# calculates the average biomass of the pupolations in time over different runs of the same batch of 
# simulations, and the standard deviation. ribbon plotter (mind zero values for the ribbon).
# needs as input the "total.n" files.

setwd("/home/somros/Documents/itn100results/resultsBase/tot/half")
library(abind)
library(reshape)
library(ggplot2)
listOfFiles<-list.files("/home/somros/Documents/itn100results/resultsBase/tot/half", 
                 recursive=TRUE, pattern="*.csv") 
lengthList<-length(listOfFiles)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', nrow=2000) # custom function to read the batches of .csv keeping the header
}
dataList<-lapply(listOfFiles, read.special) # all the data in a huge list of data
matcol<-list() # empty list for the loop
for (i in c(1:lengthList)) { # should become a plyr function
        matcol[[i]]<-dataList[[i]][,c(1,5,7,9,11,13,15,21)] # list of matrix containing data of interest: time and classes for each .csv
}
allMatrix <- abind(matcol, along=3) # change the structure of the matrix matcol in order to use the function apply on it
meanAll <- apply(allMatrix, c(1,2), mean) # calculates the mean number of individuals in every position
sdAll <- apply(allMatrix, c(1,2), sd) # and its sd
sdAll[,1] <- meanAll[,1]

colnames(meanAll)<-c("Time","smallpelagic","mediumpelagic","largepelagic","smalldemersal",
                      "mediumdemersal","largedemersal","toppiscivores") # changes the names of the columns
colnames(sdAll)<-c("Time","smallpelagicSD","mediumpelagicSD","largepelagicSD","smalldemersalSD",
                    "mediumdemersalSD","largedemersalSD", "toppiscivoresSD") # for sd as well
meanAll<-as.data.frame(meanAll) # turns the matrix into a data frame
sdAll<-as.data.frame(sdAll) # same
meanAll[,c(2:length(meanAll))] <- meanAll[,c(2:length(meanAll))] + 1 # workaround for the log scale (either this or NAs, no big difference)

normal_scientific<-expression(0,10,10^2,10^3,10^4,10^5) # notation to be used in the plot
years <- seq(0,40,5)


meltMean <- melt(meanAll, id.vars = "Time", variable.name="variable", value.name="value")
meltSd <- melt(sdAll, id.vars = "Time", variable.name="variable", value.name="value")
complete <- data.frame(meltMean, meltSd[,3])
colnames(complete) <- c("time", "class", "mean", "sd")

library(RColorBrewer)
par(mar = c(0, 4, 0, 0))
display.brewer.all()
doublePalette <- brewer.pal(3, "YlOrRd")

# 2-tailed ribbon causes troubles for zero values due to log scale. use with caution

ribbonBiomass <- ggplot(complete, aes(x=time, y=mean, group=class))+
  geom_line(aes(linetype=class, colour=class), size=.7)+
  labs(#title = "Population dynamics of the community", 
    x="Time steps", 
    y="Abundance [individuals]")+
  scale_x_continuous("t [years]", limits = c(0,2000),
                   breaks=seq(0,2000,by=250), labels=years, expand=c(0,0)) +
  scale_y_continuous(name="Biomass [kg]", 
                     limits=c(1000,100000000),
                     breaks=c(1000,10000,100000,1000000,10000000,100000000), 
                     expand=c(0,0), labels=normal_scientific)+
  scale_colour_manual(name="Functional group",
                      values=c(c(c(rep(doublePalette[3], 3), 
                                   rep(doublePalette[2],3)), 
                                 doublePalette[1])),
                      labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                               "Medium demersal", "Large demersals", "Top piscivores"))+
  scale_linetype_manual(name="Functional group",
                      values=rep(c("solid","dotted","longdash"),3),
                      labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                               "Medium demersal", "Large demersals", "Top piscivores"))+
  coord_trans(y="log10")+
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

ribbonBiomass


ggsave("/home/somros/Documents/ITNFollowUp/picsWP/communityBiomass.pdf", ribbonBiomass, useDingbats=FALSE ) # set better res pls




