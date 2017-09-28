
length<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Fish Length Master 2R.csv")

length$pop<-length$Population
length<-length[,-c(1,6,7)]

env_factors$population
env_factors<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Environmental pond data for PCA_R.csv")
str(env_factors)

env_factors<-env_factors[-c(4,5),]
env_data<-env_factors[,-c(1,5,6,7,8,9,18)]
env_data$bg<-as.numeric(c("0","0","0","0","1","0","0","0","0","1","0","0","0","0","0","0","1","0","0","1","0","0","0"))
env_data$bass<-as.numeric(c("1","0","0","0","0","0","0","1","0","1","0","0","0","1","0","0","1","0","0","0","0","0","0"))
env_data$pop<-c("Ant","AW","CaK","Corc","DeA","DeL","DoD","FC","FS","Harkin","K2","K5","Larsen","LAW",
                "LHC","NBLM","NL","Sch","Sho","SpH","WatL","WSU","Yolo")
env_data$logzoops<-log(env_factors$total_zoops_plus_1)
env_data$logclad<-log(env_factors$total_cladocerans_plus_1)

library(plyr)
length.data<-join(length,env_data)
length.data$sex<-length.data$Sex
length.data$abundance<-as.numeric(length.data$mfish_abundance)
length.data<-length.data[,-c(4,7)]

#Remove Yolo
inc.yolo<-which(length.data$pop != "Yolo")
length.data<-length.data[inc.yolo,]
rm(inc.yolo)

### Remove IM fish ###
inc1<-which(length.data$sex != "IM")
length.data2<-length.data[inc1,]
inc2<-which(length.data2$sex != "IM M")
length.data2<-length.data2[inc2,]
rm(inc1) + rm(inc2)

length.data2$bass<-as.factor(length.data2$bass)

length.data2$Standard_Length<-length.data2$Standard.Length
length.data2$Bass<-length.data2$bass
str(length.data2)

library(ggplot2)
library(Rmisc)

length.data2$pred_regime
####################################################################################
### Length and abundance by population

length.data.ord<-length.data2
length.data.ord2<-length.data.ord[!(length.data.ord$pred_regime=="BG"),]
length.data.ord2$pop <- factor(length.data.ord2$pop, levels=c("Ant", "FC", "LAW", "NL","Sch","AW","CaK","Corc","DeL","DoD","FS","K2","K5","Larsen","LHC","NBLM","Sho","WatL","WSU"))

pop<-summarySE(length.data.ord2, measurevar="Standard_Length", groupvars=c("pop","pred_regime"))
ggplot(pop, aes(x=pop,y=Standard_Length))+geom_point(aes(colour=pred_regime), size=4) +
  geom_errorbar(aes(ymin=Standard_Length-se, ymax=Standard_Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("pop lengths") + theme(plot.title = element_text(hjust = 0.5))

ggplot(length.data.ord2, aes(x=pop,y=mfish_density))+geom_point(aes(colour=pred_regime), size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Density") + theme(plot.title = element_text(hjust = 0.5))

###################################################################################

Bass<-summarySE(length.data2, measurevar="Standard_Length", groupvars=c("sex","Bass"))
ggplot(Bass, aes(x=Bass,y=Standard_Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard_Length-se, ymax=Standard_Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Effect of Bass presence on Gambusia length") + theme(plot.title = element_text(hjust = 0.5))

BG<-summarySE(length.data2, measurevar="Standard_Length", groupvars=c("sex","bg"))
ggplot(BG, aes(x=bg,y=Standard_Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard_Length-se, ymax=Standard_Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Effect of BG presence on Gambusia length") + theme(plot.title = element_text(hjust = 0.5))

##### Create seperate data frames for SC and B #####
inc.sc<-which(length.data2$location == "SC")
length.SC<-length.data2[inc.sc,]
rm(inc.sc)

inc.B<-which(length.data2$location == "B")
length.B<-length.data2[inc.B,]
rm(inc.B)

library(car)
library(ggplot2)
library(Rmisc)

################################################################################################################################

####### Bishop graphs #######

##### Bass #####
B1<-summarySE(length.B, measurevar="Standard.Length", groupvars=c("sex","bass"))
ggplot(B1, aes(x=bass,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass") + theme(plot.title = element_text(hjust = 0.5))

##### Temp #####
length.B$Temperature<-length.B$temp_c
length.B$Standard_Length<-length.B$Standard.Length

B2<-summarySE(length.B, measurevar="Standard_Length", groupvars=c("sex","Temperature", "bass"))
ggplot(B2, aes(x=Temperature,y=Standard_Length))+geom_point(aes(colour = sex, shape = bass), size=4) +
  geom_errorbar(aes(ymin=Standard_Length-se, ymax=Standard_Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Effect of temperature (C) on length for Bishop sites") + theme(plot.title = element_text(hjust = 0.5))

##### Abundance #####

B3<-summarySE(length.B, measurevar="Standard.Length", groupvars=c("sex","abundance"))
ggplot(B3, aes(x=abundance,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Mosquitofish Abundance") + theme(plot.title = element_text(hjust = 0.5))

##### Log zoops #####

B4<-summarySE(length.B, measurevar="Standard.Length", groupvars=c("sex","logzoops"))
ggplot(B4, aes(x=logzoops,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Zooplankton") + theme(plot.title = element_text(hjust = 0.5))

##### Log cladocerans #####

B5<-summarySE(length.B, measurevar="Standard.Length", groupvars=c("sex","logclad"))
ggplot(B5, aes(x=logclad,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Cladoceran") + theme(plot.title = element_text(hjust = 0.5))



####### Santa Cruz Graphs #######

##### Bass #####

length.SC$Standard_Length<-length.SC$Standard.Length
length.SC$Bass<-length.SC$bass

SC1<-summarySE(length.SC, measurevar="Standard_Length", groupvars=c("sex","Bass"))
ggplot(SC1, aes(x=Bass,y=Standard_Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard_Length-se, ymax=Standard_Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Effect of Bass presence on Gambusia length") + theme(plot.title = element_text(hjust = 0.5))

##### BG ##### 
SC2<-summarySE(length.SC, measurevar="Standard.Length", groupvars=c("sex","bg"))
ggplot(SC2, aes(x=bg,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG") + theme(plot.title = element_text(hjust = 0.5))

##### Temp #####
SC3<-summarySE(length.SC, measurevar="Standard.Length", groupvars=c("sex","temp_c", "bass"))
ggplot(SC3, aes(x=temp_c,y=Standard.Length))+geom_point(aes(colour = sex, shape = bass), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Temp") + theme(plot.title = element_text(hjust = 0.5))

##### Abundance #####

SC4<-summarySE(length.SC, measurevar="Standard.Length", groupvars=c("sex","abundance"))
ggplot(SC4, aes(x=abundance,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Mosquitofish Abundance") + theme(plot.title = element_text(hjust = 0.5))

##### Log zoops #####

SC5<-summarySE(length.SC, measurevar="Standard.Length", groupvars=c("sex","logzoops"))
ggplot(SC5, aes(x=logzoops,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Zooplankton") + theme(plot.title = element_text(hjust = 0.5))

##### Log cladocerans #####

SC6<-summarySE(length.SC, measurevar="Standard.Length", groupvars=c("sex","logclad"))
ggplot(SC6, aes(x=logclad,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Cladoceran") + theme(plot.title = element_text(hjust = 0.5))



pred.bg<-as.character(length.data2$pred_regime)
pred.bg[(pred.bg=="BG")]<-"Bluegill"
length.data2$pred_regime<-pred.bg
length.data2$pred_regime<-factor(length.data2$pred_regime, levels=c("No predators", "Bluegill", "Bass"))



pred<-summarySE(length.data2, measurevar="Standard.Length", groupvars=c("sex","pred_regime"))
ggplot(pred, aes(x=pred_regime,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  labs(y="Standard Length",x="Predator Regime") + #change axis labels
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text.x=element_text(size=15),axis.title=element_text(size=18)) +
  ggtitle("Effect of predator presence on mosquitofish body length") + theme(plot.title = element_text(size=20,hjust = 0.5,face="bold"))





