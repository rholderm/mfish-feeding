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

inc1<-which(feed.data2$trial_cue=="B")
feed.data.B<-feed.data2[inc1,]
rm(inc1)

inc2<-which(feed.data2$trial_cue=="NB")
feed.data.NB<-feed.data2[inc2,]
rm(inc2)

library(car)
library(ggplot2)
library(Rmisc)

### Bass trials

B1<-summarySE(feed.data.B, measurevar="Daphnia", groupvars=("Sex"))
B2<-ggplot(B1, aes(x=Sex,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,60) +
  ggtitle("Bass trial") + theme(plot.title = element_text(hjust = 0.5))

NB1<-summarySE(feed.data.NB, measurevar="Daphnia", groupvars=("Sex"))
NB2<-ggplot(NB1, aes(x=Sex,y=Daphnia)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Daphnia-se, ymax=Daphnia+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.y = element_blank(), axis.title.x = element_blank()) +
  ylim(0,60) +
  ggtitle("No bass trial") + theme(plot.title = element_text(hjust = 0.5))             

library(gridExtra)
grid.arrange(B2, NB2, ncol=2, left="Daphnia Consumed", bottom="Sex",top="Prey consumption difference between sexes")












