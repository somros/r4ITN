# plotter for the size spectra, reads the already calculated .csv files. lite version
# new version

library(ggplot2)
library(abind)
library(reshape2)
setwd("/home/somros/Documents/itn100results/input/sizeSpectrumInputLog10")

base <- read.table("base.csv", header=TRUE, sep=' ', dec='.')
U_I2 <- read.table("U_I2.csv", header=TRUE, sep=' ', dec='.')
U_I3 <- read.table("U_I3.csv", header=TRUE, sep=' ', dec='.')
S500_I2 <- read.table("S500_I2.csv", header=TRUE, sep=' ', dec='.')
S500_I3 <- read.table("S500_I3.csv", header=TRUE, sep=' ', dec='.')
S250_I2 <- read.table("S250_I2.csv", header=TRUE, sep=' ', dec='.')
S250_I3 <- read.table("S250_I3.csv", header=TRUE, sep=' ', dec='.')

dataList <- list(base, U_I2, U_I3, S500_I2, S500_I3, S250_I2, S250_I3)
longest <- max(unlist(lapply(dataList, nrow)))

toolkit <- function(x) {
  x$dom <- c(x$dom[2:length(x$dom)], NA)
  y <- as.data.frame(mapply(c, x, as.data.frame(matrix(nrow = longest-nrow(x), ncol = ncol(x))))) # din din din din
  #y$ln_bin <- log(1:nrow(y)) # log of the bin instead
  y <- y[,c(1,2)]
  return(y)
}

longList <- lapply(dataList, toolkit)
logWeights <- longList[[1]][,1]
freqList <- lapply(longList, function(x) x[,2])
complete <- cbind.data.frame(logWeights, abind(freqList, along=2))
#complete <- complete[c(2:nrow(complete)),]
colnames(complete) <- c("logWeights", "base", "U_I2", "U_I3", "S500_I2", "S500_I3", "S250_I2", "S250_I3")
meltComplete <- melt(complete, id.vars = "logWeights", variable.name = "Regime", value.name = "Frequency")

# levels(factor(ensemble$dom)) # check which level is missing and remove it from the plot aesthetics
# ensemble$ln_bin <- log(1:nrow(ensemble))

plotData <- subset(meltComplete, Regime=="base" | Regime=="U_I2" |
                     Regime=="S500_I2" | Regime=="S250_I2")

spectrumPlot <- ggplot(data = plotData,
                       aes(x=logWeights, y=Frequency, group=Regime))+
  geom_line(data=plotData[!is.na(plotData$Frequency),], aes(linetype=Regime, color=Regime))+
  scale_x_continuous(name="log(weight bin [100g])", 
                     limits=c(0,11),
                     breaks=seq(0,11,1))+
  scale_y_continuous(name="log(number of individuals)", 
                     limits=c(0,12),
                     breaks=seq(0,12,1))+
  scale_color_manual(values=c("#000000", "#525252", "#737373", "#BDBDBD"))+
  scale_linetype_manual(values=c("solid", "dashed", "dotted", "longdash"))+
  #guides(linetype = guide_legend(override.aes = list(size=5)))+
  #labs(title="Community weight spectrum")+
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
  

spectrumPlot

#ggsave("/home/somros/Documents/ITNFollowUp/picsWP/sizeSpectrum/bin100/logWeight/sizeSpectraLineI3.pdf", spectrumPlot, useDingbats=FALSE ) # set better res pls

library(RColorBrewer)
par(mar = c(0, 4, 0, 0))
display.brewer.all()
doublePalette <- brewer.pal(9, "YlOrRd")
myPalette <- doublePalette[seq(3,length(doublePalette),2)]

linearRegression <- ggplot(data = plotData,
                       aes(x=logWeights, y=Frequency, group=Regime))+
  geom_ribbon(data=plotData[!is.na(plotData$Frequency),], aes(linetype=NULL, color=NULL),
              stat = "smooth", method = "lm", alpha=.1)+
  geom_line(data=plotData[!is.na(plotData$Frequency),], aes(linetype=Regime, color=Regime),
            stat = "smooth", method = "lm")+
  scale_x_continuous(name="ln(weight bin [100g])", 
                     limits=c(0,11),
                     breaks=seq(0,11,1))+
  scale_y_continuous(name="ln(number of individuals)", 
                     limits=c(0,14),
                     breaks=seq(0,14,1))+
  scale_color_manual(values=rep("black", 4))+
  scale_linetype_manual(values=c("solid", "dashed", "dotted", "longdash"))+
  #guides(linetype = guide_legend(override.aes = list(size=5)))+
  #labs(title="Community weight spectrum")+
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


linearRegression

ggsave("/home/somros/Documents/ITNFollowUp/picsWP/sizeSpectrum/bin100/logWeight/regressionsI2.pdf", linearRegression, useDingbats=FALSE ) # set better res pls



fitter <- function(x) {
  fitExperiment <- lm(x$ln_freq ~ x$ln_length)#, 
  #weights=x$ln_freq) # fitting the quadratic model to the AVERAGE bins
  intercept <- coef(summary(fitExperiment))[1,1]
  slope <- coef(summary(fitExperiment))[2,1]
  intErr <- coef(summary(fitExperiment))[1,2]
  slpErr <- coef(summary(fitExperiment))[2,2]
  rsquared <- summary(fitExperiment)$r.squared
  dof <- summary(fitExperiment)$df
  return(c(slope, intercept, slpErr, intErr, rsquared, dof))
}

metricsList <- lapply(dataList, fitter)
metricsFrame <- as.data.frame(abind(metricsList, along=0))[,-c(6,8)]
colnames(metricsFrame) <- c("slope", "intercept", "slpErr", "intErr", "rsquared", "dof")
write.csv(metricsFrame, 
          "/home/somros/Documents/itn100results/sizeSpectrum2000/spectrumMetricsTables/paper.csv")