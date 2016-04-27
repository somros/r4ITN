# 07/09/2015

# script to compute and plot the disaggrgated size spectrum of the community. a frequency-weight bin
# routine is run on the individual.csv file of every replicate, and the log of the frequencies and
# the weight bins are computed. then the frequencies are averaged among all the runs.
# for each bin, in each run, it is also calculated which is the most abundant functional group. this
# is represented in the plot with the respective colours. among the replicates, the mode (and not the mean)
# is considered.
# the linear regression of the spectrum is also plotted, but it's computed in another script.

# update 19.03.2016: only operative script to calculate the dominating class per size bin
# needs thourough update and revision

# 19.03.2016 modified version t operate on 2000

library(ggplot2)
setwd("/home/somros/Documents/itn100results/sizeSpectrum2000/S250/i3")
list<-list.files("/home/somros/Documents/itn100results/sizeSpectrum2000/S250/i3", 
                 recursive=TRUE, pattern="*.csv") 
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.') # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special) # stores all the files into a huge list

frequencies <-function(data) { # function to extract the frequencies for each replicate
        mass <- data$biomass # isolates the column with biomass. infact, no need to factorize if the spectrum is for the whole
        # community, which still has to be defined anyway
        breaks <- seq(0, ceiling(max(mass))+10, 100) # sets the breaks ranging over the biomass of the individuals
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

classWriter <- function(data) { # writes the name of the most abundant class per weight bin
        mass <- data$biomass 
        breaks <- seq(0, ceiling(max(mass))+10, 100) # same bins as in the frequencies function
        length_classes <- c(1, breaks[2:(length(breaks)-1)]) # same again
        dataSort <- data[order(data$biomass),] # orders the datasets according to the biomass
        dominantClasses <- list() # prep lists for the loop
        bin <- list() # prep lists for the loop
        occurrences <- list() # prep lists for the loop
        dominant <- numeric(length=length(length_classes)) # prep vec for the loop
        
        for (i in 1:length(length_classes)) { 
                bin[[i]] <- dataSort[dataSort$biomass>=length_classes[i]-50 & dataSort$biomass<length_classes[i],]
                occurrences[[i]]<-table(unlist(bin[[i]]$class))
                if (max(occurrences[[i]])==0) {
                        dominant[[i]] <- NA
                } else {
                        dominant[i] <- names(occurrences[[i]][(which(occurrences[[i]]==max(occurrences[[i]])))])
                }
        }
        return(dominant)
        
}

# plotter region

dominantClasses <- lapply(data_list, classWriter) 

freqs <- lapply(data_list, frequencies) 

# routine to make all the frames of the same length

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

# same as above for the names

listbreaksClass<-numeric(length=length(freqs))
for (i in 1:length(dominantClasses)) {
        listbreaksClass[i]<-length(dominantClasses[[i]]) 
}
maxlengthClass <- match(max(listbreaksClass), listbreaksClass) # extracts the index of the largest break
classNames <- list()
for (j in 1:length(dominantClasses)) { 
        classNames[[j]] <- dominantClasses[[j]]
        classNames[[j]] <- c(classNames[[j]], rep(NA, max(listbreaksClass)-length(classNames[[j]])))
}

classEnumerator <- function(x) { # function to turn the names of the classes to numbers from 1 to 7
        x <- gsub("smallpelagic", 1, x)
        x <- gsub("mediumpelagic", 2, x)
        x <- gsub("largepelagic", 3, x)
        x <- gsub("smalldemersal", 4, x)
        x <- gsub("mediumdemersal", 5, x)
        x <- gsub("largedemersal", 6, x)
        x <- gsub("topcarnivores", 7, x)
        x <- as.numeric(x)
}

dominantClasses <- lapply(classNames, classEnumerator)
refined <- list()
for (i in (1:length(runs))) {
        refined[[i]] <- data.frame(runs[[i]], dominantClasses[[i]])
        refined
}
head(refined[[1]])
head(refined[[2]])


# runs <- data.frame(matrix(unlist(runs), nrow=length(runs[[1]]), byrow=F),stringsAsFactors=FALSE)
# runs[is.na(runs)] <- 0 # gets rid of NAs, check if legit lol

library("abind")

all.matrix <- abind(refined, along=3)
all.matrix[is.na(all.matrix)] <- 0 # gets rid of NAs, check if legit lol
mean_runs <- apply(all.matrix, c(1,2), mean) # gotcha
sd_runs <- apply(all.matrix, c(1,2), sd) # gotcha

Mode <- function(x, na.rm=TRUE) { 
        namesDoms <- names(which(table(x)==max(table(x))))
        if (length(namesDoms)>1) {
                namesDoms <- namesDoms[1]
        } else {
                namesDoms <- namesDoms
        }
        namesDoms
              
}

domList <- list() # workaround, creates a list of the dom vectors
for (i in 1:length(refined)) {
        domList[[i]] <- refined[[i]][,2]
}

modesDoms <- as.data.frame(abind(domList, along=2)) # binds the dom vectors in a dataframe

modesDom <- apply(modesDoms, 1, Mode) 

modesDomMod <- list()
for (i in 1:length(modesDom)) {
        if (is.null(modesDom[[i]])==TRUE) {
                modesDomMod[[i]] <- NA
        } else { modesDomMod[[i]] <- modesDom[[i]]}
}
modesDomMod <- unlist(modesDomMod, recursive=FALSE) # AYYYYYY LMAO NAILED IT KEK


ensemble <- data.frame(freqs[[maxlength]][,1], mean_runs[,1], modesDomMod, sd_runs) # data frame containing all the runs and the largest 
# breaks sequence. shooorter lines filled with zeros. now need average and plot and fit of the lm

ensemble <- ensemble[,c(1:4)]
colnames(ensemble)<-c("ln_length","ln_freq", "dom", "sd")
ensemble$sd[ensemble$sd==0] <- NA
# ensemble$dom <- c(ensemble$dom[2:length(ensemble$dom)], NA) # wat
ensemble$ln_freq[ensemble$ln_freq==0] <- NA




fitExperiment = lm(ensemble$ln_freq ~ ensemble$ln_length)#, 
#weights=1/(ensemble$sd^2)) # fitting the quadratic model to the AVERAGE bins
newx = data.frame(bin = ensemble$ln_length)
pred <- predict(fitExperiment,newdata=newx) 
pdat <- data.frame(newx, pred, ymax=pred+ensemble$sd, ymin=pred-ensemble$sd) # create array of coordinates, y of the model is y, the bins are x (x could be whatsoever, just the length must be the same)
pdat <- with(data.frame(pred),
             data.frame(x = newx, y = fitExperiment))
est <- coef(summary(fitExperiment))[,1]
err <- coef(summary(fitExperiment))[,2]

# functions of the extreme lines

trueReg <- function(ln_length){(est[2])*ln_length+est[1]}
mmin <- function(ln_length){(est[2]-err[2])*ln_length+est[1]}
mmax <- function(ln_length){(est[2]+err[2])*ln_length+est[1]}
qmin <- function(ln_length){est[2]*ln_length+est[1]-err[1]}
qmax <- function(ln_length){est[2]*ln_length+est[1]+err[1]}

library(RColorBrewer)
par(mar = c(0, 4, 0, 0))
display.brewer.all()
brewer.pal(9, "Set1")

write.table(ensemble, "/home/somros/Documents/itn100results/input/sizeSpectrumInput100/S250_I3.csv")


p <- ggplot(subset(ensemble, dom==1 | dom==2| dom==3 | dom==5| dom==6| dom==7 | dom==4),
            aes(x = ln_length, y = ln_freq, colour= factor(dom))) +
        geom_point(aes(size=2, position="jitter")) +
        stat_function(data=ensemble, fun= trueReg, linetype="solid", color="blue")+
        stat_function(data=ensemble, fun= mmin, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= mmax, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= qmin, linetype="dashed", color="grey")+
        stat_function(data=ensemble, fun= qmax, linetype="dashed", color="grey")+
        scale_x_continuous("ln(weight class [20g])", breaks=seq(3,11,1),
                           limits=c(2.99,11), labels=c(3:11))+
        scale_y_continuous(name="ln(number of individuals)", 
                           limits=c(0,14),
                           breaks=c(0:14))+
        scale_colour_manual(name="Functional groups",
                            values=c("#377EB8", "#E41A1C", "#4DAF4A","#FF7F00","#984EA3","#999999","#F781BF"),
                            labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                                    "Medium demersal", "Large demersals", "Top carnivores"))+
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

p


ggsave("C:/Users/Alberto/Documents/itn100results/R_output/base/ind/baseCol1.pdf", p, useDingbats=FALSE )
