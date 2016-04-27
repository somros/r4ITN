library(ggplot2)
library(reshape)
setwd("/home/somros/Documents/itn100results/input/finalInput")
data <- read.table("abundance.csv", header=TRUE, sep=';', dec='.')
colnames(data) <- c("Regime", "Total", "Smallpelagic", "Mediumpelagic", "Largepelagic", "Smalldemersal",
                    "Mediumdemersal", "Largedemersal", "Toppiscivores", "totsd", "spsd", "mpsd", "lpsd",
                    "sdsd", "mdsd", "ldsd", "tcsd")
data$Regime <- as.character(data$Regime)
#Then turn it back into an ordered factor
data$Regime <- factor(data$Regime, levels=unique(data$Regime))
meanValues <- data[,c(1:9)]
sdValues <- data[,c(1,10:17)]

# transform to relative abundance

headers <- c("Regime","tot","sp", "mp", "lp", "sd", "md", "ld", "tp")

relativeAbundanceMean <- as.data.frame(matrix(nrow=nrow(meanValues), ncol=ncol(meanValues)))
for (i in 1:nrow(meanValues)) {
  relativeAbundanceMean[i,] <- ((meanValues[i,]/meanValues[1,])-1)*100
}

colnames(relativeAbundanceMean) <- headers
relativeAbundanceMean$Regime <- meanValues$Regime

# sd according to propagation of error

relativeAbundanceSd <- as.data.frame(matrix(nrow=nrow(sdValues), ncol=ncol(sdValues)))
for (i in 1:nrow(sdValues)) {
  relativeAbundanceSd[i,] <- abs(((sqrt((sdValues[i,]/meanValues[i,])^2+(sdValues[1,]/meanValues[1,])^2))-1)*100)
}

colnames(relativeAbundanceSd) <- headers
relativeAbundanceSd$Regime <- meanValues$Regime

meltmeanValues <- melt(relativeAbundanceMean, id.vars="Regime")
meltsdValues <- melt(relativeAbundanceSd, id.vars="Regime")
meltmeanValues$sd <- meltsdValues$value
meltAllData <- meltmeanValues

dodge <- position_dodge(0.9)

meltAllDataMod <- subset(meltAllData, variable!="tot")
meltAllDataMod$intensity <- rep(c("0", rep(c("1","2","3"),6)), 7)
#cleanData <- meltAllDataMod[rev(rownames(meltAllDataMod)),] # revert order of rows
rownames(cleanData)<-1:nrow(cleanData)
cleanData[,1] <- factor(cleanData[,1],levels(cleanData[,1])[c(length(cleanData[,1]):1)])


newPlot <- ggplot(data=subset(cleanData, Regime=="U_I2" | Regime=="U_I3" | Regime=="S500_I2" |
                                Regime=="S500_I3" | Regime=="S250_I2" | Regime=="S250_I3"),
                  aes(x=variable, y=value, group=Regime))+
  geom_point(position = dodge, size=.7)+
  geom_hline(yintercept = 0, linetype="dashed")+
  #geom_bar(position=dodge, stat="identity", width=.4)+
  geom_errorbar(aes(ymin=value-sd,ymax=value+sd),
                position=dodge, width=0.1, size=0.3)+
  scale_x_discrete(name="Functional group", labels=1:7)+
  scale_y_continuous(limits=c(-300,300),
                     breaks=seq(-300,300,50), 
                     expand=c(0,0), labels=seq(-300,300,50), "Abundance variation from baseline [%]")+
  scale_fill_manual(values=rep("black", 7))+
  
  #coord_flip()+
  theme(panel.background = element_rect(fill = 'white'))+
  #theme
  theme_bw()+
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())+
  theme(axis.title.x = element_text(size=10,vjust=0.5),
        axis.title.y = element_text(size=10,vjust=0.5))+
  theme(legend.title = element_text(size=10))+
  theme(axis.text.x=element_text(size=10,vjust=0.5))+
  theme(axis.text.y=element_text(size=10))+
  facet_grid( ~ Regime, scales="free")

newPlot 

ggsave("/home/somros/Documents/ITNFollowUp/picsWP/relativeAbundance.pdf", newPlot, 
       useDingbats=FALSE) # set better res pls
