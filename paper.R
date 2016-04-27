# 18/02/2016 Alberto Rovellini

# script to plot the abundance of the community as barchart factored by class and/or scenario.
# needs as input the mean and sd final abundance of each scenario (which is no throughput, hand-compiled)

library(ggplot2)
library(reshape)
setwd("C:/Users/Alberto/Documents/itn100results/input/finalInput")

# Abundance 

dataAbundance <- read.table("abundance.csv", header=TRUE, sep=';', dec='.')
colnames(dataAbundance) <- c("Regime", "Total", "Smallpelagic", "Mediumpelagic", "Largepelagic", "Smalldemersal",
                    "Mediumdemersal", "Largedemersal", "Toppiscivores", "totsd", "spsd", "mpsd", "lpsd",
                    "sdsd", "mdsd", "ldsd", "tcsd")
dataAbundance$Regime <- as.character(dataAbundance$Regime)
#Then turn it back into an ordered factor
dataAbundance$Regime <- factor(dataAbundance$Regime, levels=unique(dataAbundance$Regime))
meanValuesAbundance <- dataAbundance[,c(1:9)]
sdValuesAbundance <- dataAbundance[,c(1,10:17)]

meltmeanValuesAbundance <- melt(meanValuesAbundance, id.vars="Regime")
meltsdValuesAbundance <- melt(sdValuesAbundance, id.vars="Regime")
meltmeanValuesAbundance$sd <- meltsdValuesAbundance$value
meltAllDataAbundance <- meltmeanValuesAbundance

# Biomass

dataBiomass <- read.table("biomass.csv", header=TRUE, sep=';', dec='.')
colnames(dataBiomass) <- c("Regime", "Total", "Smallpelagic", "Mediumpelagic", "Largepelagic", "Smalldemersal",
                    "Mediumdemersal", "Largedemersal", "Toppiscivores", "totsd", "spsd", "mpsd", "lpsd",
                    "sdsd", "mdsd", "ldsd", "tcsd")
dataBiomass$Regime <- as.character(dataBiomass$Regime)
#Then turn it back into an ordered factor
dataBiomass$Regime <- factor(dataBiomass$Regime, levels=unique(dataBiomass$Regime))
meanValuesBiomass <- dataBiomass[,c(1:9)]
sdValuesBiomass <- dataBiomass[,c(1,10:17)]

meltmeanValuesBiomass <- melt(meanValuesBiomass, id.vars="Regime")
meltsdValuesBiomass <- melt(sdValuesBiomass, id.vars="Regime")
meltmeanValuesBiomass$sd <- meltsdValuesBiomass$value
meltAllDataBiomass <- meltmeanValuesBiomass

meltAllData <- cbind(meltAllDataAbundance, meltAllDataBiomass[,c(3,4)])
colnames(meltAllData) <- c("Regime", "variable", "Abundance", "sdAb", "Biomass", "sdBio")

# disaggregated

meltAllDataMod <- subset(meltAllData, variable!="Total")
meltAllDataMod$intensity <- rep(c("0", rep(c("1","2","3"),6)), 7)
meltAllDataMod <- subset(meltAllDataMod, Regime=="Base" | Regime=="U_I2" | Regime=="U_I3" | Regime=="S500_I2" | 
                                 Regime=="S500_I3" | Regime=="S250_I2" | Regime=="S250_I3" | 
                                 Regime=="C_I2" | Regime=="C_I3")

# aggregated

meltAllDataModTot <- subset(meltAllData, variable=="Total")
meltAllDataModTot$selectivity <- c("Base", rep("U", 3), rep("S500", 3), rep("C", 3), rep("M500", 3), rep("S250", 3),
                                rep("M250", 3))
meltAllDataModTot$intensity <- rep(c("0", rep(c("1","2","3"),6)))
meltAllDataModTot <- subset(meltAllDataModTot, Regime=="Base" | Regime=="U_I2" | Regime=="U_I3" | Regime=="S500_I2" | 
                                 Regime=="S500_I3" | Regime=="S250_I2" | Regime=="S250_I3" | 
                                 Regime=="C_I2" | Regime=="C_I3")
meltAllDataModTot$selectivity <- factor(meltAllDataModTot$selectivity, levels=unique(meltAllDataModTot$selectivity))


normal_scientific<-expression(0, 10^5, 2*10^5, 3*10^5, 4*10^5) # notation to be used in the plot

# limits for the errorbars, sd

library(RColorBrewer)
par(mar = c(0, 4, 0, 0))
display.brewer.all()
brewer.pal(9, "Set1")
dodge <- position_dodge(0)


# dotplot total

BiomassPlot <- ggplot(data=meltAllDataModTot, aes(x=selectivity, y=Biomass, group=selectivity))+
        geom_point(aes(shape=intensity, group=selectivity), position=dodge, size=3.6)+
        geom_errorbar(aes(ymin=Biomass-sdBio,ymax=Biomass+sdBio),
                      position=dodge, width=0.1, size=0.3)+
        geom_hline(yintercept=meltAllDataModTot[1,5], linetype=2)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,60),
                           breaks=seq(0,60,5), 
                           expand=c(0,0), labels=seq(0,60,5), "Community biomass [t]")+
        scale_shape_manual(values=c(1,2,0))+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank())+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,angle=45,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))

BiomassPlot

AbundancePlot <- ggplot(data=meltAllDataModTot, aes(x=selectivity, y=Abundance, group=selectivity))+
        geom_point(aes(shape=intensity, group=selectivity), position=dodge, size=3.6)+
        geom_errorbar(aes(ymin=Abundance-sdAb,ymax=Abundance+sdAb),
                      position=dodge, width=0.1, size=0.3)+
        geom_hline(yintercept=meltAllDataModTot[1,3], linetype=2)+
        scale_x_discrete(name="Fishing regime")+
        scale_y_continuous(limits=c(0,400000),
                           breaks=seq(0,400000,100000), 
                           expand=c(0,0), labels=normal_scientific, "Community abundance [ind]")+
        scale_shape_manual(values=c(1,2,0))+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank())+
        theme(axis.title.x = element_text(size=12,vjust=0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(legend.title = element_text(size=12))+
        theme(axis.text.x=element_text(size=12,angle=45,vjust=0.5))+
        theme(axis.text.y=element_text(size=12))

AbundancePlot





