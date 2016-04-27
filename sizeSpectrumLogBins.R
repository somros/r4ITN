# plotter for the size spectra, reads the already calculated .csv files. lite version
# new version

library(ggplot2)
library(abind)
library(reshape2)
setwd("/home/somros/Documents/itn100results/input/sizeSpectrumInput50")

base <- read.table("base.csv", header=TRUE, sep=' ', dec='.')
U_I2 <- read.table("U_i2.csv", header=TRUE, sep=' ', dec='.')
U_I3 <- read.table("U_i3.csv", header=TRUE, sep=' ', dec='.')
S500_I2 <- read.table("S500_i2.csv", header=TRUE, sep=' ', dec='.')
S500_I3 <- read.table("S500_i3.csv", header=TRUE, sep=' ', dec='.')
S250_I2 <- read.table("S250_i2.csv", header=TRUE, sep=' ', dec='.')
S250_I3 <- read.table("S250_i3.csv", header=TRUE, sep=' ', dec='.')

dataList <- list(base, U_I2, U_I3, S500_I2, S500_I3, S250_I2, S250_I3)
longest <- max(unlist(lapply(dataList, nrow)))

toolkit <- function(x) {
  x$dom <- c(x$dom[2:length(x$dom)], NA)
  y <- as.data.frame(mapply(c, x, as.data.frame(matrix(nrow = longest-nrow(x), ncol = ncol(x))))) # din din din din
  y$ln_bin <- log(1:nrow(y))
  y <- y[,c(2,5)]
  return(y)
}

longList <- lapply(dataList, toolkit)
logBins <- longList[[1]][,2]
freqList <- lapply(longList, function(x) x[,1])
abind(freqList, along=2)
complete <- cbind.data.frame(logBins, abind(freqList, along=2))
colnames(complete) <- c("logBins", "base", "U_I2", "U_I3", "S500_I2", "S500_I3", "S250_I2", "S250_I3")
meltComplete <- melt(complete, id.vars = "logBins", variable.name = "Regime", value.name = "Frequency")

# levels(factor(ensemble$dom)) # check which level is missing and remove it from the plot aesthetics
# ensemble$ln_bin <- log(1:nrow(ensemble))


spectrumPlot <- ggplot(data=subset(meltComplete, Regime=="base" | Regime=="U_I2" |
                                     Regime=="S500_I2" | Regime=="S250_I2"),
                       aes(x=logBins, y=Frequency, group=Regime))+
  geom_line(aes(linetype=Regime, color=Regime))+
  scale_x_continuous(name="log(weigth bin [50g])", 
                     limits=c(0,6),
                     breaks=seq(0,6,1))+
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

ggsave("/home/somros/Documents/ITNFollowUp/picsWP/sizeSpectraLineI2.pdf", spectrumPlot, useDingbats=FALSE ) # set better res pls










# to be automated

fitExperiment = lm(ensemble$ln_freq ~ ensemble$ln_length)#, 
#weights=ensemble$ln_freq) # fitting the quadratic model to the AVERAGE bins
#plot(ensemble$ln_length, ensemble$ln_freq)
newx = data.frame(bin = ensemble$ln_length)
pred <- predict(fitExperiment,newdata=newx) 
pdat <- data.frame(newx, pred, ymax=pred+ensemble$sd, ymin=pred-ensemble$sd) # create array of coordinates, y of the model is y, the bins are x (x could be whatever, just the length must be the same)
pdat <- with(data.frame(pred),
             data.frame(x = newx, y = fitExperiment))
est <- coef(summary(fitExperiment))[,1]
err <- coef(summary(fitExperiment))[,2]
rsq <- summary(fitExperiment)$r.squared
dof <- summary(fitExperiment)$df


# functions of the extreme lines

trueReg <- function(ln_length){(est[2])*ln_length+est[1]}

mmin <- function(ln_length){(est[2]-err[2])*ln_length+est[1]}
mmax <- function(ln_length){(est[2]+err[2])*ln_length+est[1]}
qmin <- function(ln_length){est[2]*ln_length+est[1]-err[1]}
qmax <- function(ln_length){est[2]*ln_length+est[1]+err[1]}

# ribbon plotter

spectrumPlot <- ggplot(subset(ensemble, dom==1 | dom==2| dom==3 | dom==5| dom==6| dom==7 | dom==4),
             aes(x = ln_length, y = ln_freq))+#, shape= factor(dom))) +
        geom_smooth(span=3, method=lm)+
        #stat_function(data=ensemble, fun= trueReg, linetype="solid", color="black")+
        #stat_function(data=ensemble, fun= mmin, linetype="dashed", color="grey")+
        #stat_function(data=ensemble, fun= mmax, linetype="dashed", color="grey")+
        #stat_function(data=ensemble, fun= qmin, linetype="dashed", color="grey")+
        #stat_function(data=ensemble, fun= qmax, linetype="dashed", color="grey")+
        #scale_x_continuous("log(weight class [20g])", breaks=seq(3,11,1),
                           #limits=c(2.99,11), labels=c(3:11))+
        scale_y_continuous(name="log(number of individuals)", 
                           limits=c(0,14),
                           breaks=seq(0,14,2))+
        scale_shape_manual(name="Class",
                           values=c(0,1,3,2,5,4,6),
                           labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                                    "Medium demersal", "Large demersals", "Top piscivores"))+
        guides(colour = guide_legend(override.aes = list(size=5)))+
        #labs(title="Community weight spectrum")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(linetype="dashed"))+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=12,vjust=-0.5),
              axis.title.y = element_text(size=12,vjust=0.5))+
        theme(axis.text.x=element_text(size=12))+
        theme(axis.text.y=element_text(size=12))

spectrumPlot

#ggsave("C:/Users/Alberto/Documents/LaTeX/latexdirectory/picsWP/sizeSpectrumBase.pdf", spectrumPlot, useDingbats=FALSE ) # set better res pls

##############################################################################

# scatterplot

spectrumPlot <- ggplot(refined[[1]], aes(x = ln_bin, y = ln_freq))+#, shape= factor(dom))) +
  geom_line()+
  geom_line(data=refined[[3]], aes(x = ln_bin, y = ln_freq, linetype="dashed"))+
  geom_line(data=refined[[5]], aes(x = ln_bin, y = ln_freq, linetype="dashed"))+
  #stat_function(data=ensemble, fun= trueReg, linetype="solid", color="black")+
  #stat_function(data=ensemble, fun= mmin, linetype="dashed", color="grey")+
  #stat_function(data=ensemble, fun= mmax, linetype="dashed", color="grey")+
  #stat_function(data=ensemble, fun= qmin, linetype="dashed", color="grey")+
  #stat_function(data=ensemble, fun= qmax, linetype="dashed", color="grey")+
  #scale_x_continuous("log(weight class [20g])", breaks=seq(3,11,1),
  #limits=c(2.99,11), labels=c(3:11))+
  scale_y_continuous(name="log(number of individuals)", 
                     limits=c(0,14),
                     breaks=seq(0,14,2))+
  scale_shape_manual(name="Class",
                     values=c(0,1,3,2,5,4,6),
                     labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                              "Medium demersal", "Large demersals", "Top piscivores"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  #labs(title="Community weight spectrum")+
  theme(panel.background = element_rect(fill = 'white'))+
  #theme
  theme_bw()+
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(linetype="dashed"))+
  theme(plot.title = element_text(size=14, vjust=2))+
  theme(axis.title.x = element_text(size=12,vjust=-0.5),
        axis.title.y = element_text(size=12,vjust=0.5))+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.text.y=element_text(size=12))

spectrumPlot

# use and trash

# spectrumPlot <- ggplot(subset(ensemble, dom==1 | dom==5| dom==6| dom==4),
#                        aes(x = ln_length, y = ln_freq, shape= factor(dom))) +
#         geom_point(size=3) +
#         stat_function(data=ensemble, fun= trueReg, linetype="solid", color="black")+
#         stat_function(data=ensemble, fun= mmin, linetype="dashed", color="grey")+
#         stat_function(data=ensemble, fun= mmax, linetype="dashed", color="grey")+
#         stat_function(data=ensemble, fun= qmin, linetype="dashed", color="grey")+
#         stat_function(data=ensemble, fun= qmax, linetype="dashed", color="grey")+
#         scale_x_continuous("log(weight class [20g])", breaks=seq(3,11,1),
#                            limits=c(2.99,11), labels=c(3:11))+
#         scale_y_continuous(name="log(number of individuals)", 
#                            limits=c(0,14),
#                            breaks=seq(0,14,2))+
#         scale_shape_manual(name="Class",
#                            values=c(0,2,5,4),
#                            labels=c("Small pelagic", "Small demersal",
#                                     "Medium demersal", "Large demersals"))+
#         guides(colour = guide_legend(override.aes = list(size=5)))+
#         #labs(title="Community weight spectrum")+
#         theme(panel.background = element_rect(fill = 'white'))+
#         #theme
#         theme_bw()+
#         theme(panel.grid.minor = element_blank(), 
#               panel.grid.major = element_line(linetype="dashed"))+
#         theme(plot.title = element_text(size=14, vjust=2))+
#         theme(axis.title.x = element_text(size=12,vjust=-0.5),
#               axis.title.y = element_text(size=12,vjust=0.5))+
#         theme(axis.text.x=element_text(size=12))+
#         theme(axis.text.y=element_text(size=12))
# 
# spectrumPlot

#ggsave("C:/Users/Alberto/Documents/LaTeX/latexdirectory/picsWP/sizeSpectrumM250_I3.pdf", spectrumPlot, useDingbats=FALSE ) # set better res pls

#coef(summary(fitExperiment))
#summary(fitExperiment)


