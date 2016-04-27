# script to read and process the fishery output
# chart area, directory on the trial output of the "working" model, 09/07/2015

library(ggplot2)
library(reshape)
setwd("/home/somros/Documents/itn100results/size500_i3/fish")
list<-list.files("/home/somros/Documents/itn100results/size500_i3/fish", 
                 recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list<-length(list)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', skip=1) # custom function to read the batches of .csv keeping the header
}
data_list <- lapply(list, read.special)

total <- list()
for (i in 1:length(data_list)) {
        total[[i]] <- data.frame(c(1:nrow(data_list[[i]])), data_list[[i]])
        colnames(total[[i]]) <- c("Event", "Time", "Total","Smallpelagic","Mediumpelagic","Largepelagic",
                                  "Smalldemersal", "Mediumdemersal","Largedemersal","Mediumgrazer","Largegrazer",
                                  "Topcarnivore","NA1")
        drops <- c("NA1", "Time", "Mediumgrazer","Largegrazer")
        total[[i]] <- total[[i]][,!(names(total[[i]]) %in% drops)]
}

grouper <- function (targetFrame) { # function to sum rows two by two
        apply(targetFrame, 2, function(x) tapply(x, (seq_along(x)-1) %/% 5, sum)) 
}

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total <- lapply(total, grouper)
Event <- c(1:nrow(total[[1]])) # vector with the number of years or events
eventWriter <- function(z) {z[,1]<- Event # function to substitute the first column of the frames
                            return(z)}
total <- lapply(total, eventWriter)

##################################################################

library("abind")

all.matrix <- abind(total, along=3)
allData <- as.data.frame(apply(all.matrix, c(1,2), mean))
allData1 <- allData[c(1:(nrow(allData)-2)),]
#allDataPerc <- allData/allData$Total*100
#allDataPerc[,1]<-allData[,1]
#allDataPerc <- allDataPerc[c(2:nrow(allDataPerc)),]
#meltAll <- melt(allDataPerc, id.vars="Event", variable="variable", value="value")
meltAll <- melt(allData1, id.vars="Event", variable="variable", value="value")


runOne <- total[[1]]
meltOne <- melt(runOne, id.vars="Event")
trick<-expression(seq(0,7,0.5))

library(RColorBrewer)
par(mar = c(0, 4, 0, 0))
display.brewer.all()
brewer.pal(9, "Greys")


p<-ggplot(subset(meltAll,variable=="Smallpelagic" | variable=="Mediumpelagic" | 
                         variable== "Largepelagic" | variable== "Smalldemersal" | variable== "Mediumdemersal" | 
                         variable== "Largedemersal" | variable== "Mediumgrazer" | variable== "Largegrazer" | 
                         variable== "Topcarnivore"), aes(x=Event, y=value, fill=variable))+
        geom_area(aes(fill=variable), position='stack', alpha=0.9)+ 
        labs(x="Years", 
             y="Catch [t]")+
        scale_x_continuous("Years", breaks=seq(0,20,2),
                           limits=c(0,21), labels=seq(0,20,2), expand=c(0,0))+
        scale_y_continuous(limits=c(0,7000000),
                           breaks=seq(0,7000000,1000000), 
                           expand=c(0,0), labels=seq(0,7,1))+
        scale_fill_manual(name="Functional groups",
                            values=c("#F0F0F0", "#BDBDBD", "#969696", "#737373", "#525252", "#252525", "#000000"),
                            labels=c("Small pelagic", "Medium pelagic", "Large pelagic", "Small demersal",
                                     "Medium demersal", "Large demersals", "Top carnivores"))+
        #guides(colour = guide_legend(override.aes = list(size=5)))+
        #coord_trans(y="log10")+
        theme(panel.background = element_rect(fill = 'white'))+
        #theme
        theme_bw()+
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank())+
        theme(plot.title = element_text(size=14, vjust=2))+
        theme(axis.title.x = element_text(size=10,vjust=-0.5),
              axis.title.y = element_text(size=10,vjust=0.5))+
        theme(legend.title = element_text(size=10))+
        theme(axis.text.x=element_text(size=10))+
        theme(axis.text.y=element_text(size=10))
p

ggsave("/home/somros/Documents/ITNFollowUp/picsWP/compS500.pdf", p, useDingbats=FALSE ) # set better res pls

