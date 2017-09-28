feeding<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Feeding Trial Data CompleteR.csv")

feeding<-feeding[,-c(5,6,7,14,15,16,17,18)]
feeding$trial_cue<-feeding$Trial.Cue..B.NB.
feeding$rearing_cue<-feeding$F1.Cue......
feeding<-feeding[,(-c(3,4))]

str(feeding)
attach(feeding)
### Add in fish ID to dataframe using paste to create unique labels
feeding$ID<-paste(Trial,Population,trial_cue,rearing_cue)
feeding2<-transform(feeding,ID2=as.numeric((factor(feeding$ID))))
detach(feeding)

### Create table
tb<-table(feeding2$ID,feeding2$Zooplankton)

### Turn table into a data frame - must use data.frame.matrix
data<-as.data.frame.matrix(tb)
rm(tb)

data$ID<-row.names(data)

### Join data sets. In order to not have replicates, must put unique in front of the large data set
library(plyr)
feed.data<-join(unique(feeding2[,-c(5,6,7,8,12)]),data)

### Create data.frame with pred regime and location and join to larger data set

Population<-c("Ant","AW","DeA","DeL","FC","Har","K2","NBLM","NL","Sch","SpH","WSU")
Pred<-c("P","NP","BG","NP","P","BG","NP","NP","P","P","BG","NP")
Location<-c("SC","B","SC","SC","B","SC","B","B","SC","SC","SC","B")

data2<-as.data.frame(Population)
data2$pred<-as.factor(Pred)
data2$location<-as.factor(Location)

rm(Population)
rm(Pred)
rm(Location)

feed.data2<-join(data2,feed.data)

### Create data sets with seperate predator regimes
inc1<-which(feed.data2$pred =="P")
feed.bass<-feed.data2[inc1,]
rm(inc1)

inc2<-which(feed.data2$pred == "NP")
feed.np<-feed.data2[inc2,]
rm(inc2)

inc3<-which(feed.data2$pred == "BG")
feed.BG<-feed.data2[inc3,]
rm(inc3)

library(car)
library(ggplot2)
library(Rmisc)


                                                  ##### Effect of Trial Cues #####

##### Bass, Rearing -
inc16<-which(feed.bass$rearing_cue =="-")
feed.B.M3<-feed.bass[inc16,]
rm(inc16)

BM3<-summarySE(feed.B.M3, measurevar="Daphnia", groupvars=("trial_cue"))
B5<-ggplot(BM3, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(),axis.title.x = element_blank()) +
  ggtitle("Bass, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

##### Bass, Rearing +
inc17<-which(feed.bass$rearing_cue =="+")
feed.B.P3<-feed.bass[inc17,]
rm(inc17)

BP3<-summarySE(feed.B.P3, measurevar="Daphnia", groupvars=("trial_cue"))
B6<-ggplot(BP3, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(),axis.title.x = element_blank()) +
  ggtitle("Bass, Rearing +") + theme(plot.title = element_text(hjust = 0.5))

########################################################################################################################
### BG, Rearing -
inc18<-which(feed.BG$rearing_cue == "-")
feed.BG.M3<-feed.BG[inc18,]
rm(inc18)

BGM3<-summarySE(feed.BG.M3, measurevar="Daphnia", groupvars=("trial_cue"))
BG5<-ggplot(BGM3, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(),axis.title.x = element_blank()) +
  ggtitle("BG, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

### BG, Rearing + 
inc19<-which(feed.BG$rearing_cue == "+")
feed.BG.P3<-feed.BG[inc19,]
rm(inc19)

BGP3<-summarySE(feed.BG.P3, measurevar="Daphnia", groupvars=("trial_cue"))
BG6<-ggplot(BGP3, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(),axis.title.x = element_blank()) +
  ggtitle("BG, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


#######################################################################################################################
### NP, Rearing -
inc20<-which(feed.np$rearing_cue =="-")
feed.np.M3<-feed.np[inc20,]
rm(inc20)

NPM3<-summarySE(feed.np.M3, measurevar="Daphnia", groupvars=("trial_cue"))
NP5<-ggplot(NPM3, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(),axis.title.x = element_blank()) +
  ggtitle("NP, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

### NP, Rearing +
inc21<-which(feed.np$rearing_cue =="+")
feed.np.P3<-feed.np[inc21,]
rm(inc21)

NPP3<-summarySE(feed.np.P3, measurevar="Daphnia", groupvars=("trial_cue"))
NP6<-ggplot(NPP3, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(),axis.title.x = element_blank()) +
  ggtitle("NP, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


grid.arrange(B5,B6,BG5,BG6,NP5,NP6, ncol=2)



