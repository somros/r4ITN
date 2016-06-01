# Alberto Rovellini, Victoria University of Wellington. 01.06.2016
# Alberto.Rovellini@vuw.ac.nz

# Script for the size spectra of the whole community, no disaggregation. Algorithm changed to calculate the
# actual log10. Requires automation, as all the others. 

library(ggplot2)
setwd("/home/somros/Documents/itn100results/sizeSpectrum2000/S250/i3")
listOfFiles<-list.files("/home/somros/Documents/itn100results/sizeSpectrum2000/S250/i3", 
                        recursive=TRUE, pattern="*.csv") 
lengthList<-length(listOfFiles)
read.special<-function(x) {
  read.table(x, header=TRUE, sep='\t', dec=',') # custom function to read the batches of .csv keeping the header
}
dataList <- lapply(listOfFiles, read.special)
#lastTimeStep <- function(data) subset(data, data$time=="2000.0") # isolates the last time step, comment out for complete analysis
#dataList <- lapply(dataList, lastTimeStep)


frequencies <-function(data) { # function to extract the frequencies for each replicate
  mass <- as.numeric(as.character(data$biomass)) # isolates the column with biomass
  breaks <- seq(0, ceiling(max(mass))+10, 100) # sets the breaks ranging over the biomass of the individuals
  length_classes <- c(1, breaks[2:(length(breaks)-1)]) 
  cat <- cut(mass, breaks, labels=length_classes, include.lowest=TRUE) 
  freq <- table(cat)
  freq <- cbind(freq)
  #freq[freq==0] <- NA # turn zeroes to NAs, redundant if next line is active
  freq[freq<2] <- NA # lower limit of resoulution
  ln_freq <- log10(freq) # lognorm transformation of the frequency data
  ln_length <- log10(length_classes) # lognormal transformation of the length classes
  freq_breaks <- data.frame(ln_length, ln_freq)
  #freq_breaks <- freq_breaks[c(2:nrow(freq_breaks)),]
  
}

# plotter region

freqs <- lapply(dataList, frequencies) # applies the function to the list of input files
listbreaks<-numeric(length=length(freqs))
for (i in 1:length(freqs)) {
  listbreaks[i]<-length(freqs[[i]][,1]) 
}
maxlength <- match(max(listbreaks), listbreaks) # extracts the index of the largest break
runs <- list()
for (j in 1:length(freqs)) { 
  runs[[j]] <- freqs[[j]][,2]
  runs[[j]] <- c(runs[[j]], rep(0, max(listbreaks)-length(runs[[j]])))
}
runs <- data.frame(matrix(unlist(runs), nrow=length(runs[[1]]), byrow=F),stringsAsFactors=FALSE)
runs[is.na(runs)] <- 0 # gets rid of NAs
mean_runs <- apply(runs, 1, mean)
sd_runs <- apply(runs, 1, sd)
ensemble <- data.frame(freqs[[maxlength]][,1], mean_runs, sd_runs) # data frame containing all the runs and the largest 
# breaks sequence. shorter lines filled with zeros

colnames(ensemble)<-c("log_length","log_freq", "sd")
ensemble$sd[ensemble$sd==0] <- NA
ensemble$log_freq[ensemble$log_freq==0] <- NA

write.table(ensemble, "/home/somros/Documents/itn100results/input/sizeSpectrumInputLog10/S250_I3.csv")





















# from here testing region #

fitting <- function(ln_length){population_coefs[1]+population_coefs[2]*ln_length+population_coefs[3]*ln_length^2} # stores the function

fitExperiment = lm(ensemble$ln_freq ~ ensemble$ln_length)#, 
# weights=1/(ensemble$sd^2)) # fitting the quadratic model to the AVERAGE bins
#plot(ensemble$ln_length, ensemble$ln_freq)
newx = data.frame(bin = ensemble$ln_length)
pred <- predict(fitExperiment,newdata=newx) 
pdat <- data.frame(newx, pred, ymax=pred+ensemble$sd, ymin=pred-ensemble$sd) # create array of coordinates, y of the model is y, the bins are x (x could be whatsoever, just the length must be the same)
pdat <- with(data.frame(pred),
             data.frame(x = newx, y = fitExperiment))
summary(fitExperiment)


p <- ggplot(ensemble, aes(x = log_length, y = log_freq))+
  geom_line() +
  geom_line(data = pdat, aes(x=bin, y=pred), colour = "blue") +
  #         stat_function(data=ensemble, fun=function(ln_length)-1.46159*ln_length+12.0059)+ # m min
  #         stat_function(data=ensemble, fun=function(ln_length)-1.34167*ln_length+12.0059)+ # m max
  #         stat_function(data=ensemble, fun=function(ln_length)-1.40163*ln_length+11.53879)+ # q min
  #         stat_function(data=ensemble, fun=function(ln_length)-1.40163*ln_length+12.47301)+ # q max
  #geom_ribbon(data = pdat, mapping = aes(x=bin, y=pred, ymax = ymax, ymin = ymin),  
  #alpha = 0.4, fill = "grey60")+
  scale_x_continuous("ln(weight class [20g])", breaks=seq(0,11,1),
                     limits=c(0,11), labels=c(0:11))+
  scale_y_continuous(name="ln(number of individuals)", 
                     limits=c(0,16),
                     breaks=c(0:16))+
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

p
# 
# coef(fitExperiment)
# summary(fitExperiment)
# write.table(ensemble, "C:/Users/Alberto/Documents/itn100results/input/sizeSpectrumInput/class_i1.csv")
# 
# #ggsave("C:/Users/Alberto/Documents/itn100results/R_output/unselective/ind/i3.pdf", p, useDingbats=FALSE )
