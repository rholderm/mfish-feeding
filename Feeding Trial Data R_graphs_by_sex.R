feeding<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Feeding Trial Data2_R.csv")

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

### Make all Immature fish male or female
sex<-as.character(feed.data2$Sex)
sex[(sex=="IM M")]<-"M"
sex[(sex=="IM F")]<-"F"
feed.data2$Sex<-sex
rm(sex)

### Remove IM fish
incim<-which(feed.data2$Sex!="IM")
feed.data2<-feed.data2[incim,]
rm(incim)

incim2<-which(feed.data2$Sex!="IM ")
feed.data2<-feed.data2[incim2,]
rm(incim2)

### Make data set with only male fish
incm<-which(feed.data2$Sex!="F")
feed.data.m<-feed.data2[incm,]
rm(incm)

### Make data set with only female fish
incf<-which(feed.data2$Sex!="M")
feed.data.f<-feed.data2[incf,]
rm(incf)

### Female predator regimes
inc1<-which(feed.data.f$pred =="P")
feed.bass.f<-feed.data.f[inc1,]
rm(inc1)

inc2<-which(feed.data.f$pred == "NP")
feed.np.f<-feed.data.f[inc2,]
rm(inc2)

inc3<-which(feed.data.f$pred == "BG")
feed.BG.f<-feed.data.f[inc3,]
rm(inc3)


### Male predator regimes
inc4<-which(feed.data.m$pred =="P")
feed.bass.m<-feed.data.m[inc4,]
rm(inc4)

inc5<-which(feed.data.m$pred == "NP")
feed.np.m<-feed.data.m[inc5,]
rm(inc5)

inc6<-which(feed.data.m$pred == "BG")
feed.BG.m<-feed.data.m[inc6,]
rm(inc6)


library(car)
library(ggplot2)
library(Rmisc)


###########################################################################################################################
                                                 
                                                   ### Effect of Trial cues ###
### Male fish

##### Bass, Rearing -
inc7<-which(feed.bass.m$rearing_cue =="-")
feed.B.Mm<-feed.bass.m[inc7,]
rm(inc7)

BM1<-summarySE(feed.B.Mm, measurevar="Daphnia", groupvars=("trial_cue"))
B1<-ggplot(BM1, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,60) +
  ggtitle("Bass, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

##### Bass, Rearing +
inc8<-which(feed.bass.m$rearing_cue =="+")
feed.B.Pm<-feed.bass.m[inc8,]
rm(inc8)

BP1<-summarySE(feed.B.Pm, measurevar="Daphnia", groupvars=("trial_cue"))
B2<-ggplot(BP1, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,60) +
  ggtitle("Bass, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


########################################################################################################################
### BG, Rearing -
inc9<-which(feed.BG.m$rearing_cue == "-")
feed.BG.Mm<-feed.BG.m[inc9,]
rm(inc9)

BGM1<-summarySE(feed.BG.Mm, measurevar="Daphnia", groupvars=("trial_cue"))
BG1<-ggplot(BGM1, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,60) +
  ggtitle("BG, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

### BG, Rearing + 
inc10<-which(feed.BG.m$rearing_cue == "+")
feed.BG.Pm<-feed.BG.m[inc10,]
rm(inc10)

BGP1<-summarySE(feed.BG.Pm, measurevar="Daphnia", groupvars=("trial_cue"))
BG2<-ggplot(BGP1, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,60) +
  ggtitle("BG, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


#######################################################################################################################
### NP, Rearing -
inc11<-which(feed.np.m$rearing_cue =="-")
feed.np.Mm<-feed.np.m[inc11,]
rm(inc11)

NPM1<-summarySE(feed.np.Mm, measurevar="Daphnia", groupvars=("trial_cue"))
NP1<-ggplot(NPM1, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,60) +
  ggtitle("NP, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

### NP, Rearing +
inc12<-which(feed.np.m$rearing_cue =="+")
feed.np.Pm<-feed.np.m[inc12,]
rm(inc12)

NPP1<-summarySE(feed.np.Pm, measurevar="Daphnia", groupvars=("trial_cue"))
NP2<-ggplot(NPP1, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,60) +
  ggtitle("NP, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


library(gridExtra)
grid.arrange(B1, B2, BG1, BG2, NP1, NP2, ncol=2, left="Daphnia Consumed", bottom="Trial cue", top="Effect of Trial Cues on Male Fish")







### Female fish

##### Bass, Rearing -
inc13<-which(feed.bass.f$rearing_cue =="-")
feed.B.Mf<-feed.bass.f[inc13,]
rm(inc13)

BM2<-summarySE(feed.B.Mf, measurevar="Daphnia", groupvars=("trial_cue"))
B3<-ggplot(BM2, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,85) +
  ggtitle("Bass, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

##### Bass, Rearing +
inc14<-which(feed.bass.f$rearing_cue =="+")
feed.B.Pf<-feed.bass.f[inc14,]
rm(inc14)

BP2<-summarySE(feed.B.Pf, measurevar="Daphnia", groupvars=("trial_cue"))
B4<-ggplot(BP2, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,85) +
  ggtitle("Bass, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


########################################################################################################################
### BG, Rearing -
inc15<-which(feed.BG.f$rearing_cue == "-")
feed.BG.Mf<-feed.BG.f[inc15,]
rm(inc15)

BGM2<-summarySE(feed.BG.Mf, measurevar="Daphnia", groupvars=("trial_cue"))
BG3<-ggplot(BGM2, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,85) +
  ggtitle("BG, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

### BG, Rearing + 
inc16<-which(feed.BG.f$rearing_cue == "+")
feed.BG.Pf<-feed.BG.f[inc16,]
rm(inc16)

BGP2<-summarySE(feed.BG.Pf, measurevar="Daphnia", groupvars=("trial_cue"))
BG4<-ggplot(BGP2, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,85) +
  ggtitle("BG, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


#######################################################################################################################
### NP, Rearing -
inc17<-which(feed.np.f$rearing_cue =="-")
feed.np.Mf<-feed.np.f[inc17,]
rm(inc17)

NPM2<-summarySE(feed.np.Mf, measurevar="Daphnia", groupvars=("trial_cue"))
NP3<-ggplot(NPM2, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,85) +
  ggtitle("NP, Rearing -") + theme(plot.title = element_text(hjust = 0.5))

### NP, Rearing +
inc18<-which(feed.np.f$rearing_cue =="+")
feed.np.Pf<-feed.np.f[inc18,]
rm(inc18)

NPP2<-summarySE(feed.np.Pf, measurevar="Daphnia", groupvars=("trial_cue"))
NP4<-ggplot(NPP2, aes(x=trial_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,85) +
  ggtitle("NP, Rearing +") + theme(plot.title = element_text(hjust = 0.5))


grid.arrange(B3, B4, BG3, BG4, NP3, NP4, ncol=2, left="Daphnia Consumed", bottom="Trial Cue", top="Effect of Trial Cues on Female Fish")




############################################################################################################################
                                              ### Effect of rearing Cues ###

### Male fish

inc19<-which(feed.bass.m$trial_cue =="NB")
feed.B.Mm2<-feed.bass.m[inc19,]
rm(inc19)

BM3<-summarySE(feed.B.Mm2, measurevar="Daphnia", groupvars=("rearing_cue"))
B5<-ggplot(BM3, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0, 60) +
  ggtitle("Bass, Trial NB") + theme(plot.title = element_text(hjust = 0.5))

##### Bass, Rearing +
inc20<-which(feed.bass.m$trial_cue =="B")
feed.B.Pm2<-feed.bass.m[inc20,]
rm(inc20)

BP3<-summarySE(feed.B.Pm2, measurevar="Daphnia", groupvars=("rearing_cue"))
B6<-ggplot(BP3, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0, 60) +
  ggtitle("Bass, Trial B") + theme(plot.title = element_text(hjust = 0.5))


########################################################################################################################
### BG, Rearing -
inc21<-which(feed.BG.m$trial_cue == "NB")
feed.BG.Mm2<-feed.BG.m[inc21,]
rm(inc21)

BGM3<-summarySE(feed.BG.Mm2, measurevar="Daphnia", groupvars=("rearing_cue"))
BG5<-ggplot(BGM3, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0, 60) +
  ggtitle("BG, Trial NB") + theme(plot.title = element_text(hjust = 0.5))

### BG, Rearing + 
inc22<-which(feed.BG.m$trial_cue == "B")
feed.BG.Pm2<-feed.BG.m[inc22,]
rm(inc22)

BGP3<-summarySE(feed.BG.Pm2, measurevar="Daphnia", groupvars=c("rearing_cue"))
BG6<-ggplot(BGP3, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0, 60) +
  ggtitle("BG, Trial B") + theme(plot.title = element_text(hjust = 0.5))


#######################################################################################################################
### NP, Rearing -
inc23<-which(feed.np.m$trial_cue =="NB")
feed.np.Mm2<-feed.np.m[inc23,]
rm(inc23)

NPM3<-summarySE(feed.np.Mm2, measurevar="Daphnia", groupvars=("rearing_cue"))
NP5<-ggplot(NPM3, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0, 60) +
  ggtitle("NP, Trial NB") + theme(plot.title = element_text(hjust = 0.5))

### NP, Rearing +
inc24<-which(feed.np.m$trial_cue =="B")
feed.np.Pm2<-feed.np.m[inc24,]
rm(inc24)

NPP3<-summarySE(feed.np.Pm2, measurevar="Daphnia", groupvars=("rearing_cue"))
NP6<-ggplot(NPP3, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0, 60) +
  ggtitle("NP, Trial B") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(B5,B6,BG5,BG6,NP5,NP6, ncol=2, left="Daphnia Consumed",bottom="Rearing Cue", top="Effect of Rearing Cues on Male fish")






### Female fish

inc25<-which(feed.bass.f$trial_cue =="NB")
feed.B.Mf2<-feed.bass.f[inc25,]
rm(inc25)

BM4<-summarySE(feed.B.Mf2, measurevar="Daphnia", groupvars=("rearing_cue"))
B7<-ggplot(BM4, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0, 85) +
  ggtitle("Bass, Trial NB") + theme(plot.title = element_text(hjust = 0.5))

##### Bass, Rearing +
inc26<-which(feed.bass.f$trial_cue =="B")
feed.B.Pf2<-feed.bass.f[inc26,]
rm(inc26)

BP4<-summarySE(feed.B.Pf2, measurevar="Daphnia", groupvars=("rearing_cue"))
B8<-ggplot(BP4, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0, 85) +
  ggtitle("Bass, Trial B") + theme(plot.title = element_text(hjust = 0.5))


########################################################################################################################
### BG, Rearing -
inc27<-which(feed.BG.f$trial_cue == "NB")
feed.BG.Mf2<-feed.BG.f[inc27,]
rm(inc27)

BGM4<-summarySE(feed.BG.Mf2, measurevar="Daphnia", groupvars=("rearing_cue"))
BG7<-ggplot(BGM4, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0, 85) +
  ggtitle("BG, Trial NB") + theme(plot.title = element_text(hjust = 0.5))

### BG, Rearing + 
inc28<-which(feed.BG.f$trial_cue == "B")
feed.BG.Pf2<-feed.BG.f[inc28,]
rm(inc28)

BGP4<-summarySE(feed.BG.Pf2, measurevar="Daphnia", groupvars=c("rearing_cue"))
BG8<-ggplot(BGP4, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0, 85) +
  ggtitle("BG, Trial B") + theme(plot.title = element_text(hjust = 0.5))


#######################################################################################################################
### NP, Rearing -
inc29<-which(feed.np.f$trial_cue =="NB")
feed.np.Mf2<-feed.np.f[inc29,]
rm(inc29)

NPM4<-summarySE(feed.np.Mf2, measurevar="Daphnia", groupvars=("rearing_cue"))
NP7<-ggplot(NPM4, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0, 85) +
  ggtitle("NP, Trial NB") + theme(plot.title = element_text(hjust = 0.5))

### NP, Rearing +
inc30<-which(feed.np.f$trial_cue =="B")
feed.np.Pf2<-feed.np.f[inc30,]
rm(inc30)

NPP4<-summarySE(feed.np.Pf2, measurevar="Daphnia", groupvars=("rearing_cue"))
NP8<-ggplot(NPP4, aes(x=rearing_cue,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0, 85) +
  ggtitle("NP, Trial B") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(B7,B8,BG7,BG8,NP7,NP8, ncol=2, left="Daphnia Consumed",bottom="Rearing Cue", top="Effect of Rearing Cues on Female fish")







