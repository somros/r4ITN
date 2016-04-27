# need to find a way to automate

library(ggplot2)
library(reshape)
setwd("/home/somros/Documents/itn100results/input/finalInput")
biomass <- read.table("biomass.csv", header=TRUE, sep=';', dec='.')
colnames(biomass) <- c("Regime", "Total", "Smallpelagic", "Mediumpelagic", "Largepelagic", "Smalldemersal",
                    "Mediumdemersal", "Largedemersal", "Toppiscivores", "totsd", "spsd", "mpsd", "lpsd",
                    "sdsd", "mdsd", "ldsd", "tcsd")
biomass$Regime <- as.character(biomass$Regime)
#Then turn it back into an ordered factor
biomass$Regime <- factor(biomass$Regime, levels=unique(biomass$Regime))
meanValuesBM <- biomass[,c(1:9)]
sdValuesBM <- biomass[,c(1,10:17)]

# transform to relative biomass

headers <- c("Regime","tot","sp", "mp", "lp", "sd", "md", "ld", "tp")

relativeBiomassMean <- as.data.frame(matrix(nrow=nrow(meanValuesBM), ncol=ncol(meanValuesBM)))
for (i in 1:nrow(meanValuesBM)) {
  relativeBiomassMean[i,] <- (meanValuesBM[i,]/meanValuesBM[1,])
}

colnames(relativeBiomassMean) <- headers
relativeBiomassMean$Regime <- meanValuesBM$Regime

# sd according to propagation of error

relativeBiomassSd <- as.data.frame(matrix(nrow=nrow(sdValuesBM), ncol=ncol(sdValuesBM)))
for (i in 1:nrow(sdValuesBM)) {
  relativeBiomassSd[i,] <- abs(sqrt((sdValuesBM[i,]/meanValuesBM[i,])^2+(sdValuesBM[1,]/meanValuesBM[1,])^2))
}

colnames(relativeBiomassSd) <- headers
relativeBiomassSd$Regime <- meanValuesBM$Regime

meltmeanValuesBM <- melt(relativeBiomassMean, id.vars="Regime")
meltsdValuesBM <- melt(relativeBiomassSd, id.vars="Regime")
meltmeanValuesBM$sd <- meltsdValuesBM$value
meltAllDataBM <- meltmeanValuesBM

dodge <- position_dodge(0.9)

#meltAllDataMod <- subset(meltAllData, variable!="tot")
meltAllDataBM$intensity <- rep(c("0", rep(c("1","2","3"),6)), 8)
cleanDataBM <- meltAllDataBM#[rev(rownames(meltAllDataBM)),] # revert order of rows
rownames(cleanDataBM)<-1:nrow(cleanDataBM)
cleanDataBM[,1] = factor(cleanDataBM[,1],levels(cleanDataBM[,1])[c(length(cleanDataBM[,1]):1)])
cleanDataBM$metric <- rep("bm", nrow(cleanDataBM))


# abundance (same routine as biomass)


######################################################



abundance <- read.table("abundance.csv", header=TRUE, sep=';', dec='.')
colnames(abundance) <- c("Regime", "Total", "Smallpelagic", "Mediumpelagic", "Largepelagic", "Smalldemersal",
                       "Mediumdemersal", "Largedemersal", "Toppiscivores", "totsd", "spsd", "mpsd", "lpsd",
                       "sdsd", "mdsd", "ldsd", "tcsd")
abundance$Regime <- as.character(abundance$Regime)
#Then turn it back into an ordered factor
abundance$Regime <- factor(abundance$Regime, levels=unique(abundance$Regime))
meanValuesAB <- abundance[,c(1:9)]
sdValuesAB <- abundance[,c(1,10:17)]

# transform to relative abundance

headers <- c("Regime","tot","sp", "mp", "lp", "sd", "md", "ld", "tp")

relativeAbundanceMean <- as.data.frame(matrix(nrow=nrow(meanValuesAB), ncol=ncol(meanValuesAB)))
for (i in 1:nrow(meanValuesAB)) {
  relativeAbundanceMean[i,] <- (meanValuesAB[i,]/meanValuesAB[1,])
}

colnames(relativeAbundanceMean) <- headers
relativeAbundanceMean$Regime <- meanValuesAB$Regime

# sd according to propagation of error

relativeAbundanceSd <- as.data.frame(matrix(nrow=nrow(sdValuesAB), ncol=ncol(sdValuesAB)))
for (i in 1:nrow(sdValuesAB)) {
  relativeAbundanceSd[i,] <- abs(sqrt((sdValuesAB[i,]/meanValuesAB[i,])^2+(sdValuesAB[1,]/meanValuesAB[1,])^2))
}

colnames(relativeAbundanceSd) <- headers
relativeAbundanceSd$Regime <- meanValuesAB$Regime

meltmeanValuesAB <- melt(relativeAbundanceMean, id.vars="Regime")
meltsdValuesAB <- melt(relativeAbundanceSd, id.vars="Regime")
meltmeanValuesAB$sd <- meltsdValuesAB$value
meltAllDataAB <- meltmeanValuesAB

dodge <- position_dodge(0.9)

#meltAllDataMod <- subset(meltAllData, variable!="tot")
meltAllDataAB$intensity <- rep(c("0", rep(c("1","2","3"),6)), 8)
cleanDataAB <- meltAllDataAB#[rev(rownames(meltAllDataAB)),] # revert order of rows edit: why would I?
rownames(cleanDataAB)<-1:nrow(cleanDataAB)
cleanDataAB[,1] = factor(cleanDataAB[,1],levels(cleanDataAB[,1])[c(length(cleanDataAB[,1]):1)])
cleanDataAB$metric <- rep("ab", nrow(cleanDataBM))

# merging by row the two frames

allData <- rbind(cleanDataAB,cleanDataBM)
allData$Regime <- factor(allData$Regime, levels=allData$Regime)
dataPlotBM <- subset(cleanDataBM, Regime=="U_I2" | Regime=="U_I3" | Regime=="S500_I2" |
              Regime=="S500_I3" | Regime=="S250_I2" | Regime=="S250_I3")
dataPlotBM$Regime <- factor(dataPlotBM$Regime, levels=dataPlotBM$Regime)


newPlotBM <- ggplot(data=dataPlotBM,
                  aes(x=variable, y=value, group=Regime, fill=variable, color= variable))+
  geom_bar(position=dodge, stat="identity", width=.6, alpha=0.8)+
  #geom_point(position = dodge, size=.7)+
  geom_hline(yintercept = 1, linetype="dashed")+
  geom_errorbar(aes(ymin=value,ymax=value+sd),
                position=dodge, width=0.1, size=0.3)+
  scale_x_discrete(name="Functional group", labels=c("Tot",1:7))+
  scale_y_continuous(limits=c(0,8),
                     breaks=seq(0,8,1), 
                     expand=c(0,0), labels=seq(0,8,1), "Standard biomass")+
  scale_fill_manual(values=c("black", rep("white", 7)))+
  scale_color_manual(values=rep("black", 8))+
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
  facet_grid( ~ Regime, scales="free")+
  theme(strip.text.y = element_blank(), strip.background = element_blank())

newPlotBM 

# abundance

dataPlotAB <- subset(cleanDataAB, Regime=="U_I2" | Regime=="U_I3" | Regime=="S500_I2" |
                       Regime=="S500_I3" | Regime=="S250_I2" | Regime=="S250_I3")
dataPlotAB$Regime <- factor(dataPlotAB$Regime, levels=dataPlotAB$Regime)

newPlotAB <- ggplot(data=dataPlotAB,
                    aes(x=variable, y=value, group=Regime, fill=variable, color= variable))+
  geom_bar(position=dodge, stat="identity", width=.6, alpha=0.8)+
  #geom_point(position = dodge, size=.7)+
  geom_hline(yintercept = 1, linetype="dashed")+
  geom_errorbar(aes(ymin=value,ymax=value+sd),
                position=dodge, width=0.1, size=0.3)+
  scale_x_discrete(name="Functional group", labels=c("Tot",1:7))+
  scale_y_continuous(limits=c(0,8),
                     breaks=seq(0,8,1), 
                     expand=c(0,0), labels=seq(0,8,1), "Standard abundance")+
  scale_fill_manual(values=c("black", rep("white", 7)))+
  scale_color_manual(values=rep("black", 8))+
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
  facet_grid( ~ Regime, scales="free")+
  theme(strip.text.y = element_blank(), strip.background = element_blank())

newPlotAB 

ggsave("/home/somros/Documents/ITNFollowUp/picsWP/standardBiomass.pdf", newPlotBM, 
       useDingbats=FALSE)

ggsave("/home/somros/Documents/ITNFollowUp/picsWP/standardAbundance.pdf", newPlotAB, 
       useDingbats=FALSE)