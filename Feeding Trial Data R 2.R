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

rm(Location)
rm(Population)
rm(Pred)

feed.data2<-join(data2,feed.data)


##############################################################################################################################
##############################################################################################################################
### Stats

feed.data2$Unk.Invert<- feed.data2$`Unk. Invert` + feed.data2$`Unk. Invert 1` + feed.data2$`Unk. Invert A` + feed.data2$`Unk. Invert1`
feed.data2$Zoops<- feed.data2$Bosmina + feed.data2$Ceriodaphnia + feed.data2$Chydorid + feed.data2$Cladoceran + feed.data2$Daphnia
feed.data2$Copepods<- feed.data2$Calanoid + feed.data2$Copepod + feed.data2$Cyclopoid
feed.data2$Insects<-feed.data2$Insect + feed.data2$`Insect?` + feed.data2$`Unk. insect` + feed.data2$`Unk. Insect`

feed.data3<-feed.data2[,-c(10,11,12,13,14,16,17,18,19,21,22,23,24,25,26,27,28,29,30,31)]
feed.data3$total_prey<-feed.data2$Unk.Invert+feed.data2$Zoops+feed.data2$Copepods+feed.data2$Insects

library(lme4)
library(car)

glm1<-glmer(Daphnia ~ trial_cue + rearing_cue + pred + Sex + (1| Population), data = feed.data3, family = poisson)
Anova(glm1)

glm2<-glmer(total_prey ~ trial_cue + rearing_cue + pred + Sex + rearing_cue*pred + (1|Population), data=feed.data3, family = poisson)
Anova(glm2)


str(feed.data3)
hist(log(feed.data3$prey))
###Fit Model with dredge

library(MuMIn)
# change na. action 
options(na.action = "na.fail")


dredge(glm1, rank = AICc)
dredge(glm2, rank = AICc)






