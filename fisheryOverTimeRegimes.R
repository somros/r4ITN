# script to read and process the fishery output

library(ggplot2)
library(reshape)
library(abind)

# data from the 1st scenario
setwd('/home/somros/Documents/itn100results/unselective_i2/fish')
list1<-list.files('/home/somros/Documents/itn100results/unselective_i2/fish', 
                   recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list1<-length(list1)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', skip=1) # custom function to read the batches of .csv keeping the header
}
data_list1 <- lapply(list1, read.special)

total1 <- list()
for (i in 1:length(data_list1)) {
        total1[[i]] <- data.frame(c(1:nrow(data_list1[[i]])), data_list1[[i]])
        #total1[[i]] <- total1[[i]][c(2:38),]
        total1[[i]] <- total1[[i]][,c(1,3)]
        colnames(total1[[i]]) <- c("Event", "Total")
}

total1 <- abind(total1, along=3)
mean1 <- as.data.frame(apply(total1, c(1,2), mean))
sd1 <- as.data.frame(apply(total1, c(1,2), sd))

grouper <- function (targetFrame) { # function to sum rows two by two
        apply(targetFrame, 2, function(x) tapply(x, (seq_along(x)-1) %/% 5, sum)) 
}

propagation <- function(q) sqrt(sum((q)^2))

sdQuadraticGrouper <- function (target) {
        apply(target, 2, function(x) tapply(x, (seq_along(x)-1) %/% 5, propagation))
}

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total1 <- grouper(mean1)
sdev1 <- sdQuadraticGrouper(sd1)
Event <- c(1:nrow(total1)) # vector with the number of years or events
eventWriter <- function(z) {z[,1]<- Event # function to substitute the first column of the frames
                            return(z)}
total1 <- eventWriter(total1)
sdev1 <- eventWriter(sdev1)     


##################################################################


# data from the 2nd scenario

setwd('/home/somros/Documents/itn100results/size500_i2/fish')
list2<-list.files('/home/somros/Documents/itn100results/size500_i2/fish', 
                   recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list2<-length(list2)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', skip=1) # custom function to read the batches of .csv keeping the header
}
data_list2 <- lapply(list2, read.special)

total2 <- list()
for (i in 1:length(data_list2)) {
        total2[[i]] <- data.frame(c(1:nrow(data_list2[[i]])), data_list2[[i]])
        #total2[[i]] <- total2[[i]][c(2:38),]
        total2[[i]] <- total2[[i]][,c(1,3)]
        colnames(total2[[i]]) <- c("Event", "Total")
}

total2 <- abind(total2, along=3)
mean2 <- as.data.frame(apply(total2, c(1,2), mean))
sd2 <- as.data.frame(apply(total2, c(1,2), sd))

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total2 <- grouper(mean2)
sdev2 <- sdQuadraticGrouper(sd2)
Event <- c(1:nrow(total2)) # vector with the number of years or events
total2 <- eventWriter(total2)
sdev2 <- eventWriter(sdev2)

# data from the 3rd scenario

setwd('/home/somros/Documents/itn100results/class_i2/fish')
list3<-list.files('/home/somros/Documents/itn100results/class_i2/fish', 
                   recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list3<-length(list3)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', skip=1) # custom function to read the batches of .csv keeping the header
}
data_list3 <- lapply(list3, read.special)

total3 <- list()
for (i in 1:length(data_list3)) {
        total3[[i]] <- data.frame(c(1:nrow(data_list3[[i]])), data_list3[[i]])
        #total3[[i]] <- total3[[i]][c(2:38),]
        total3[[i]] <- total3[[i]][,c(1,3)]
        colnames(total3[[i]]) <- c("Event", "Total")
}

total3 <- abind(total3, along=3)
mean3 <- as.data.frame(apply(total3, c(1,2), mean))
sd3 <- as.data.frame(apply(total3, c(1,2), sd))

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total3 <- grouper(mean3)
sdev3 <- sdQuadraticGrouper(sd3)
Event <- c(1:nrow(total3)) # vector with the number of years or events
total3 <- eventWriter(total3)
sdev3 <- eventWriter(sdev3)

# data from the 4th scenario

setwd('/home/somros/Documents/itn100results/mixed_i2/fish')
list4<-list.files('/home/somros/Documents/itn100results/mixed_i2/fish', 
                  recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list4<-length(list4)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', skip=1) # custom function to read the batches of .csv keeping the header
}
data_list4 <- lapply(list4, read.special)

total4 <- list()
for (i in 1:length(data_list4)) {
        total4[[i]] <- data.frame(c(1:nrow(data_list4[[i]])), data_list4[[i]])
        #total4[[i]] <- total4[[i]][c(2:38),]
        total4[[i]] <- total4[[i]][,c(1,3)]
        colnames(total4[[i]]) <- c("Event", "Total")
}

total4 <- abind(total4, along=3)
mean4 <- as.data.frame(apply(total4, c(1,2), mean))
sd4 <- as.data.frame(apply(total4, c(1,2), sd))

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total4 <- grouper(mean4)
sdev4 <- sdQuadraticGrouper(sd4)
Event <- c(1:nrow(total4)) # vector with the number of years or events
total4 <- eventWriter(total4)
sdev4 <- eventWriter(sdev4)

# data from the 5th scenario

setwd('/home/somros/Documents/itn100results/size250_i2/fish')
list5<-list.files('/home/somros/Documents/itn100results/size250_i2/fish', 
                  recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list5<-length(list5)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', skip=1) # custom function to read the batches of .csv keeping the header
}
data_list5 <- lapply(list5, read.special)

total5 <- list()
for (i in 1:length(data_list5)) {
        total5[[i]] <- data.frame(c(1:nrow(data_list5[[i]])), data_list5[[i]])
        #total5[[i]] <- total5[[i]][c(2:38),]
        total5[[i]] <- total5[[i]][,c(1,3)]
        colnames(total5[[i]]) <- c("Event", "Total")
}

total5 <- abind(total5, along=3)
mean5 <- as.data.frame(apply(total5, c(1,2), mean))
sd5 <- as.data.frame(apply(total5, c(1,2), sd))

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total5 <- grouper(mean5)
sdev5 <- sdQuadraticGrouper(sd5)
Event <- c(1:nrow(total5)) # vector with the number of years or events
total5 <- eventWriter(total5)
sdev5 <- eventWriter(sdev5)

# data from the 6th scenario

setwd('/home/somros/Documents/itn100results/mixed250_i2/fish')
list6<-list.files('/home/somros/Documents/itn100results/mixed250_i2/fish', 
                  recursive=TRUE, pattern=".csv*") # lists all the file (might need to change to .csv)
length.list6<-length(list6)
read.special<-function(x) {
        read.table(x, header=TRUE, sep='\t', dec='.', skip=1) # custom function to read the batches of .csv keeping the header
}
data_list6 <- lapply(list6, read.special)

total6 <- list()
for (i in 1:length(data_list6)) {
        total6[[i]] <- data.frame(c(1:nrow(data_list6[[i]])), data_list6[[i]])
        #total6[[i]] <- total6[[i]][c(2:38),]
        total6[[i]] <- total6[[i]][,c(1,3)]
        colnames(total6[[i]]) <- c("Event", "Total")
}

total6 <- abind(total6, along=3)
mean6 <- as.data.frame(apply(total6, c(1,2), mean))
sd6 <- as.data.frame(apply(total6, c(1,2), sd))

# next lines are for the sum of the rows, trash them in case of 1 event per year.
##################################################################

total6 <- grouper(mean6)
sdev6 <- sdQuadraticGrouper(sd6)
Event <- c(1:nrow(total6)) # vector with the number of years or events
total6 <- eventWriter(total6)
sdev6 <- eventWriter(sdev6)

# put them together

allData <- data.frame(total1, total2[,2], total3[,2], total4[,2], total5[,2], total6[,2])
allData1 <- allData[c(1:(nrow(allData)-2)),]
colnames(allData1) <- c("Event", "U", "S500", "C", "M500", "S250", "M250")

# write.table(allData1, "C:/Users/Alberto/Documents/itn100results/input/fisheryTables/I3.csv")

allDataMelt <- melt(allData1, id.vars="Event")

allsd <- data.frame(sdev1, sdev2[,2], sdev3[,2], sdev4[,2], sdev5[,2], sdev6[,2])
allsd1 <- allsd[c(1:(nrow(allsd)-2)),]
colnames(allsd1) <- c("Event", "Usd", "S500sd", "Csd", "M500sd", "S250sd", "M250sd")
allsdMelt <- melt(allsd1, id.vars="Event")

# provisional, to be recoded entirely

allDataMelt$sd <- allsdMelt$value

# limits1 <- aes(ymax=allData1$U+allsd1$Usd, ymin=allData1$U-allsd1$Usd)
# limits2 <- aes(ymax=allData1$S500+allsd1$S500sd, ymin=allData1$S500-allsd1$S500sd) 
# limits3 <- aes(ymax=allData1$C+allsd1$Csd, ymin=allData1$C-allsd1$Csd) 
# limits4 <- aes(ymax=allData1$M500+allsd1$M500sd, ymin=allData1$M500-allsd1$M500sd)
# limits5 <- aes(ymax=allData1$S250+allsd1$S250sd, ymin=allData1$S250-allsd1$S250sd)
# limits6 <- aes(ymax=allData1$M250+allsd1$M250sd, ymin=allData1$M250-allsd1$M250sd)
                              

trick<-expression(seq(0,7000,1000))

# ribbon

regimesPlot <- ggplot(subset(allDataMelt, variable=="U" | variable=="S500" | variable=="S250"),
          aes(x=Event, y=value, group=variable))+
        geom_line(aes(linetype=variable), size=1)+
        geom_ribbon(aes(ymin=value-sd, ymax=value+sd), alpha=.3)+
        scale_x_continuous("Years", breaks=seq(0,20,2),
                           limits=c(0,21), labels=seq(0,20,2), expand=c(0,0))+
        scale_y_continuous("Catch [t]", limits=c(0,7000000),
                           breaks=seq(0,7000000,1000000), 
                           expand=c(0,0), labels=c(0:7))+
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

regimesPlot

# point by point 

dodge <- position_dodge(width=0.9)
regimesPlot <- ggplot(subset(allDataMelt, variable=="U" | variable=="S500" | variable=="S250"),
                      aes(x=Event, y=value, group=variable))+
  #geom_point(aes(shape=variable), size=1.2, position=dodge)+
  #geom_errorbar(data=subset(allDataMelt,  variable=="U" | variable=="S500" | variable=="S250"),
                #aes(ymin=value-sd, ymax=value+sd), position=dodge)+
  geom_pointrange(data=subset(allDataMelt,  variable=="U" | variable=="S500" | variable=="S250"),
                  aes(ymin=value-sd, ymax=value+sd, group=variable, shape=variable), 
                  position=dodge, size=.4)+
  geom_line(position = dodge)+
  scale_x_continuous("Years", breaks=seq(0,20,2),
                     limits=c(0,21), labels=seq(0,20,2), expand=c(0,0))+
  scale_y_continuous("Catch [t]", limits=c(0,7000000),
                     breaks=seq(0,7000000,1000000), 
                     expand=c(0,0), labels=c(0:7))+
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

regimesPlot


ggsave("/home/somros/Documents/ITNFollowUp/picsWP/regimesI2.pdf", regimesPlot, useDingbats=FALSE ) # set better res pls
