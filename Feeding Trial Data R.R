feeding<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Feeding Trial Data2_R.csv")

feeding<-feeding[,-c(5,6,7,14,15,16,17,18)]
feeding$trial_cue<-feeding$Trial.Cue..B.NB.
feeding$rearing_cue<-feeding$F1.Cue......
feeding<-feeding[,(-c(3,4))]

str(feeding)

### Add in fish ID to dataframe using paste to create unique labels
feeding$ID<-paste(Trial,Population,trial_cue,rearing_cue)
feeding2<-transform(feeding,ID2=as.numeric((factor(feeding$ID))))

### Create table
tb<-table(feeding2$ID,feeding2$Zooplankton)

### Turn table into a data frame - must use data.frame.matrix
data<-as.data.frame.matrix(tb)
str(data)

data$ID<-row.names(data)

### Join data sets. In order to not have replicates, must put unique in front of the large data set
library(plyr)
feed.data<-join(unique(feeding2[,-c(5,6,7,8,12)]),data)
feed.data$Population

### Create data.frame with pred regime and location and join to larger data set

Population<-c("Ant","AW","DeA","DeL","FC","Har","K2","NBLM","NL","Sch","SpH","WSU")
Pred<-c("P","NP","BG","NP","P","BG","NP","NP","P","P","BG","NP")
Location<-c("SC","B","SC","SC","B","SC","B","B","SC","SC","SC","B")

data2<-as.data.frame(Population)
data2$pred<-as.factor(Pred)
data2$location<-as.factor(Location)

feed.data2<-join(data2,feed.data)

str(feed.data2)

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

### Bass populations with changing rearing and trial cues
B1<-summarySE(feed.bass, measurevar="Daphnia", groupvars=c("Sex","trial_cue","rearing_cue"))
ggplot(B1, aes(x=trial_cue,y=Daphnia)) + geom_point(aes(colour = Sex, shape = rearing_cue), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass Pops") + theme(plot.title = element_text(hjust = 0.5))



##############################################################################################################
##############################################################################################################
                                   ##### Effect of Trial Cues #####
##############################################################################################################
##############################################################################################################
##### Bass, Rearing -
inc4<-which(feed.bass$rearing_cue =="-")
feed.B.M<-feed.bass[inc4,]
rm(inc4)

BM<-summarySE(feed.B.M, measurevar="Daphnia", groupvars=c("Sex","trial_cue"))
B1<-ggplot(BM, aes(x=trial_cue,y=Daphnia)) + geom_point(aes(colour = Sex), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

##### Bass, Rearing +
inc5<-which(feed.bass$rearing_cue =="+")
feed.B.P<-feed.bass[inc5,]
rm(inc5)

BP<-summarySE(feed.B.P, measurevar="Daphnia", groupvars=c("Sex","trial_cue"))
B2<-ggplot(BP, aes(x=trial_cue,y=Daphnia)) + geom_point(aes(colour = Sex), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


########################################################################################################################
### BG, Rearing -
inc6<-which(feed.BG$rearing_cue == "-")
feed.BG.M<-feed.BG[inc6,]
rm(inc6)

BGM<-summarySE(feed.BG.M, measurevar="Daphnia", groupvars=c("Sex","trial_cue"))
BG1<-ggplot(BGM, aes(x=trial_cue,y=Daphnia)) + geom_point(aes(colour = Sex), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

### BG, Rearing + 
inc7<-which(feed.BG$rearing_cue == "+")
feed.BG.P<-feed.BG[inc7,]
rm(inc7)

BGP<-summarySE(feed.BG.P, measurevar="Daphnia", groupvars=c("Sex","trial_cue"))
BG2<-ggplot(BGP, aes(x=trial_cue,y=Daphnia)) + geom_point(aes(colour = Sex), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


#######################################################################################################################
### NP, Rearing -
inc8<-which(feed.np$rearing_cue =="-")
feed.np.M<-feed.np[inc8,]
rm(inc8)

NPM<-summarySE(feed.np.M, measurevar="Daphnia", groupvars=c("Sex","trial_cue"))
NP1<-ggplot(NPM, aes(x=trial_cue,y=Daphnia)) + geom_point(aes(colour = Sex), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("NP, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

### NP, Rearing +
inc9<-which(feed.np$rearing_cue =="+")
feed.np.P<-feed.np[inc9,]
rm(inc9)

NPP<-summarySE(feed.np.P, measurevar="Daphnia", groupvars=c("Sex","trial_cue"))
NP2<-ggplot(NPP, aes(x=trial_cue,y=Daphnia)) + geom_point(aes(colour = Sex), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("NP, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


library(gridExtra)

grid.arrange(B1,B2,BG1,BG2,NP1,NP2, ncol=2)

##############################################################################################################
##############################################################################################################
                                    ##### Effect of Rearing Cues #####
##############################################################################################################
##############################################################################################################


inc10<-which(feed.bass$trial_cue =="NB")
feed.B.M2<-feed.bass[inc10,]
rm(inc10)

BM2<-summarySE(feed.B.M2, measurevar="Daphnia", groupvars=c("Sex", "rearing_cue"))
B3<-ggplot(BM2, aes(x=rearing_cue,y=Daphnia)) + geom_point(aes(colour = Sex), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass, Trial NB") + theme(plot.title = element_text(hjust = 0.5))

##### Bass, Rearing +
inc11<-which(feed.bass$trial_cue =="B")
feed.B.P2<-feed.bass[inc11,]
rm(inc11)

BP2<-summarySE(feed.B.P2, measurevar="Daphnia", groupvars=c("Sex","rearing_cue"))
B4<-ggplot(BP2, aes(x=rearing_cue,y=Daphnia)) + geom_point(aes(colour = Sex), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass, Trial B") + theme(plot.title = element_text(hjust = 0.5))


########################################################################################################################
### BG, Rearing -
inc12<-which(feed.BG$trial_cue == "NB")
feed.BG.M2<-feed.BG[inc12,]
rm(inc12)

BGM2<-summarySE(feed.BG.M2, measurevar="Daphnia", groupvars=c("Sex","rearing_cue"))
BG3<-ggplot(BGM2, aes(x=rearing_cue,y=Daphnia)) + geom_point(aes(colour = Sex), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG, Trial NB") + theme(plot.title = element_text(hjust = 0.5))

### BG, Rearing + 
inc13<-which(feed.BG$trial_cue == "B")
feed.BG.P2<-feed.BG[inc13,]
rm(inc13)

BGP2<-summarySE(feed.BG.P2, measurevar="Daphnia", groupvars=c("Sex","rearing_cue"))
BG4<-ggplot(BGP2, aes(x=rearing_cue,y=Daphnia)) + geom_point(aes(colour = Sex), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG, Trial B") + theme(plot.title = element_text(hjust = 0.5))


#######################################################################################################################
### NP, Rearing -
inc14<-which(feed.np$trial_cue =="NB")
feed.np.M2<-feed.np[inc14,]
rm(inc14)

NPM2<-summarySE(feed.np.M2, measurevar="Daphnia", groupvars=c("Sex","rearing_cue"))
NP3<-ggplot(NPM2, aes(x=rearing_cue,y=Daphnia)) + geom_point(aes(colour = Sex), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("NP, Trial NB") + theme(plot.title = element_text(hjust = 0.5))

### NP, Rearing +
inc15<-which(feed.np$trial_cue =="B")
feed.np.P2<-feed.np[inc15,]
rm(inc15)

NPP2<-summarySE(feed.np.P2, measurevar="Daphnia", groupvars=c("Sex","rearing_cue"))
NP4<-ggplot(NPP2, aes(x=rearing_cue,y=Daphnia)) + geom_point(aes(colour = Sex), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("NP, Trial B") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(B3,B4,BG3,BG4,NP3,NP4, ncol=2)



##############################################################################################################
##############################################################################################################
                         ##### Same as above, no sex differences, with error bars #####
##############################################################################################################
##############################################################################################################


                                         ##### Effect of Trial Cues #####

##### Bass, Rearing -
inc16<-which(feed.bass$rearing_cue =="-")
feed.B.M3<-feed.bass[inc16,]
rm(inc16)

BM3<-summarySE(feed.B.M3, measurevar="Daphnia", groupvars=("trial_cue"))
B5<-ggplot(BM3, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

##### Bass, Rearing +
inc17<-which(feed.bass$rearing_cue =="+")
feed.B.P3<-feed.bass[inc17,]
rm(inc17)

BP3<-summarySE(feed.B.P3, measurevar="Daphnia", groupvars=("trial_cue"))
B6<-ggplot(BP3, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


########################################################################################################################
### BG, Rearing -
inc18<-which(feed.BG$rearing_cue == "-")
feed.BG.M3<-feed.BG[inc18,]
rm(inc18)

BGM3<-summarySE(feed.BG.M3, measurevar="Daphnia", groupvars=("trial_cue"))
BG5<-ggplot(BGM3, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

### BG, Rearing + 
inc19<-which(feed.BG$rearing_cue == "+")
feed.BG.P3<-feed.BG[inc19,]
rm(inc19)

BGP3<-summarySE(feed.BG.P3, measurevar="Daphnia", groupvars=("trial_cue"))
BG6<-ggplot(BGP3, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


#######################################################################################################################
### NP, Rearing -
inc20<-which(feed.np$rearing_cue =="-")
feed.np.M3<-feed.np[inc20,]
rm(inc20)

NPM3<-summarySE(feed.np.M3, measurevar="Daphnia", groupvars=("trial_cue"))
NP5<-ggplot(NPM3, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("NP, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

### NP, Rearing +
inc21<-which(feed.np$rearing_cue =="+")
feed.np.P3<-feed.np[inc21,]
rm(inc21)

NPP3<-summarySE(feed.np.P3, measurevar="Daphnia", groupvars=("trial_cue"))
NP6<-ggplot(NPP3, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("NP, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


grid.arrange(B5,B6,BG5,BG6,NP5,NP6, ncol=2)

##############################################################################################################
##############################################################################################################
##### Effect of Rearing Cues #####
##############################################################################################################
##############################################################################################################


inc22<-which(feed.bass$trial_cue =="NB")
feed.B.M4<-feed.bass[inc22,]
rm(inc22)

BM4<-summarySE(feed.B.M4, measurevar="Daphnia", groupvars=("rearing_cue"))
B7<-ggplot(BM4, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass, Trial NB") + theme(plot.title = element_text(hjust = 0.5))

##### Bass, Rearing +
inc23<-which(feed.bass$trial_cue =="B")
feed.B.P4<-feed.bass[inc23,]
rm(inc23)

BP4<-summarySE(feed.B.P4, measurevar="Daphnia", groupvars=("rearing_cue"))
B8<-ggplot(BP4, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass, Trial B") + theme(plot.title = element_text(hjust = 0.5))


########################################################################################################################
### BG, Rearing -
inc24<-which(feed.BG$trial_cue == "NB")
feed.BG.M4<-feed.BG[inc24,]
rm(inc24)

BGM4<-summarySE(feed.BG.M4, measurevar="Daphnia", groupvars=("rearing_cue"))
BG7<-ggplot(BGM4, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG, Trial NB") + theme(plot.title = element_text(hjust = 0.5))

### BG, Rearing + 
inc25<-which(feed.BG$trial_cue == "B")
feed.BG.P4<-feed.BG[inc25,]
rm(inc25)

BGP4<-summarySE(feed.BG.P4, measurevar="Daphnia", groupvars=c("rearing_cue"))
BG8<-ggplot(BGP4, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG, Trial B") + theme(plot.title = element_text(hjust = 0.5))


#######################################################################################################################
### NP, Rearing -
inc26<-which(feed.np$trial_cue =="NB")
feed.np.M4<-feed.np[inc26,]
rm(inc26)

NPM4<-summarySE(feed.np.M4, measurevar="Daphnia", groupvars=("rearing_cue"))
NP7<-ggplot(NPM4, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("NP, Trial NB") + theme(plot.title = element_text(hjust = 0.5))

### NP, Rearing +
inc27<-which(feed.np$trial_cue =="B")
feed.np.P4<-feed.np[inc27,]
rm(inc27)

NPP4<-summarySE(feed.np.P4, measurevar="Daphnia", groupvars=("rearing_cue"))
NP8<-ggplot(NPP4, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("NP, Trial B") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(B7,B8,BG7,BG8,NP7,NP8, ncol=2)






