# plotter for the size spectra, reads the already calculated .csv files. lite version

library(ggplot2)
library(plyr)
setwd("C:/Users/Alberto/Documents/itn100results/input/sizeSpectrumInput/col")
base <- read.table("base.csv", header=TRUE, sep=' ', dec='.')
S500I2 <- read.table("S500_I2.csv", header=TRUE, sep=' ', dec='.')
S250I2 <- read.table("S250_I2.csv", header=TRUE, sep=' ', dec='.')
S500I3 <- read.table("S500_I3.csv", header=TRUE, sep=' ', dec='.')
S250I3 <- read.table("S250_I3.csv", header=TRUE, sep=' ', dec='.')

# # what about an if here?
#ensemble$dom <- c(ensemble$dom[2:length(ensemble$dom)], NA) # to be used ONLY if not yet, check the 
# # head of the ensemble file
# levels(factor(ensemble$dom)) # check which level is missing and remove it from the plot aesthetics
# 
# 
# fitExperiment = lm(ensemble$ln_freq ~ ensemble$ln_length)#, 
# #weights=ensemble$ln_freq) # fitting the quadratic model to the AVERAGE bins
# #plot(ensemble$ln_length, ensemble$ln_freq)
# newx = data.frame(bin = ensemble$ln_length)
# pred <- predict(fitExperiment,newdata=newx) 
# pdat <- data.frame(newx, pred, ymax=pred+ensemble$sd, ymin=pred-ensemble$sd) # create array of coordinates, y of the model is y, the bins are x (x could be whatever, just the length must be the same)
# pdat <- with(data.frame(pred),
#              data.frame(x = newx, y = fitExperiment))
# est <- coef(summary(fitExperiment))[,1]
# err <- coef(summary(fitExperiment))[,2]
# rsq <- summary(fitExperiment)$r.squared
# dof <- summary(fitExperiment)$df
# 
# 
# # functions of the extreme lines
# 
# trueReg <- function(ln_length){(est[2])*ln_length+est[1]}
# 
# mmin <- function(ln_length){(est[2]-err[2])*ln_length+est[1]}
# mmax <- function(ln_length){(est[2]+err[2])*ln_length+est[1]}
# qmin <- function(ln_length){est[2]*ln_length+est[1]-err[1]}
# qmax <- function(ln_length){est[2]*ln_length+est[1]+err[1]}

spectrumPlot <- ggplot(base, aes(x = ln_length, y = ln_freq, color="black"))+
        geom_smooth(method=lm)+
        geom_smooth(data=S500I2, aes(x = ln_length, y = ln_freq, linetype ="dotted", colour="red"), method=lm)+
        geom_smooth(data=S250I2, aes(x = ln_length, y = ln_freq), method=lm)+
        geom_smooth(data=S500I3, aes(x = ln_length, y = ln_freq), method=lm)+
        geom_smooth(data=S250I3, aes(x = ln_length, y = ln_freq), method=lm)+
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

# use and trash

spectrumPlot <- ggplot(subset(ensemble, dom==1 | dom==5| dom==6| dom==4),
                       aes(x = ln_length, y = ln_freq, shape= factor(dom))) +
        geom_point(size=3) +
        stat_function(data=ensemble, fun= trueReg, linetype="solid", color="black")+
        stat_function(data=ensemble, fun= mmin, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= mmax, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= qmin, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= qmax, linetype="dashed", color="grey")+
        scale_x_continuous("log(weight class [20g])", breaks=seq(3,11,1),
                           limits=c(2.99,11), labels=c(3:11))+
        scale_y_continuous(name="log(number of individuals)", 
                           limits=c(0,14),
                           breaks=seq(0,14,2))+
        scale_shape_manual(name="Class",
                           values=c(0,2,5,4),
                           labels=c("Small pelagic", "Small demersal",
                                    "Medium demersal", "Large demersals"))+
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

#ggsave("C:/Users/Alberto/Documents/LaTeX/latexdirectory/picsWP/sizeSpectrumM250_I3.pdf", spectrumPlot, useDingbats=FALSE ) # set better res pls

#coef(summary(fitExperiment))
#summary(fitExperiment)
