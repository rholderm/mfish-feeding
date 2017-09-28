

length<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Fish Length Master R.csv")

length$pop<-length$Population
length<-length[,-c(1,6,7)]


env_factors<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Environmental pond data for PCA_R.csv")

# Will need to edit this and pop names once I get Sch lengths
env_factors<-env_factors[-c(4,5,20),]
env_data<-env_factors[-c(1,4,5,6,7,8,17)]
env_data$bg<-as.numeric(c("0","0","0","0","1","0","0","0","0","1","0","0","0","0","0","0","1","0","1","0","0","0"))
env_data$bass<-as.numeric(c("1","0","0","0","0","0","0","1","0","1","0","0","0","1","0","0","1","0","0","0","0","0"))
env_data$pop<-c("Ant","AW","CaK","Corc","DeA","DeL","DoD","FC","FS","Harkin","K2","K5","Larsen","LAW",
                   "LHC","NBLM","NL","Sho","SpH","WatL","WSU","Yolo")
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

library(Rmisc)

### Sex ###
tgc1<-summarySE(length.data, measurevar="Standard.Length", groupvars=c("sex"))
ggplot(tgc1, aes(x=sex,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Sex") + theme(plot.title = element_text(hjust = 0.5))

### Remove IM fish ###
inc1<-which(length.data$sex != "IM")
length.data2<-length.data[inc1,]
inc2<-which(length.data2$sex != "IM M")
length.data2<-length.data2[inc2,]
rm(inc1) + rm(inc2)

##### Bass #####
tgc2<-summarySE(length.data2, measurevar="Standard.Length", groupvars=c("sex","bass"))
ggplot(tgc2, aes(x=bass,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass") + theme(plot.title = element_text(hjust = 0.5))

##### BG ##### 
tgc3<-summarySE(length.data2, measurevar="Standard.Length", groupvars=c("sex","bg"))
ggplot(tgc3, aes(x=bg,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG") + theme(plot.title = element_text(hjust = 0.5))

##### BG only pops #####

inc.bg<-which(length.data2$bass != 1)
length.data.bg<-length.data2[inc.bg,]

tgc3<-summarySE(length.data2, measurevar="Standard.Length", groupvars=c("sex","bg"))
ggplot(tgc3, aes(x=bg,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG") + theme(plot.title = element_text(hjust = 0.5))


##### Temp #####
tgc4<-summarySE(length.data2, measurevar="Standard.Length", groupvars=c("sex","temp_c"))
ggplot(tgc4, aes(x=temp_c,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Temp") + theme(plot.title = element_text(hjust = 0.5))


##### Temp of no pred pops #####

inc3<-which(length.data2$bass == 0)
length.data3<-length.data2[inc3,]
inc4<-which(length.data3$bg == 0)
length.data3<-length.data3[inc4,]
rm(inc3) + rm(inc4)

tgc5<-summarySE(length.data3, measurevar="Standard.Length", groupvars=c("sex","temp_c"))
ggplot(tgc5, aes(x=temp_c,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Temp No Pred") + theme(plot.title = element_text(hjust = 0.5))

##### Temp of Bishop ponds #####

inc.B<-which(length.data2$location == "B")
length.data.B<-length.data2[inc.B,]
length.data.B$bass<-as.factor(length.data.B$bass)

tgc6<-summarySE(length.data.B, measurevar="Standard.Length", groupvars=c("sex","temp_c","bass"))
ggplot(tgc6, aes(x=temp_c,y=Standard.Length))+geom_point(aes(colour = sex, shape = bass), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Temp B") + theme(plot.title = element_text(hjust = 0.5))

##### Temp of SC ponds #####

inc.SC<-which(length.data2$location == "SC")
length.data.SC<-length.data2[inc.SC,]

tgc7<-summarySE(length.data.SC, measurevar="Standard.Length", groupvars=c("sex","temp_c"))
ggplot(tgc7, aes(x=temp_c,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Temp SC") + theme(plot.title = element_text(hjust = 0.5))

##### Abundance #####

tgc8<-summarySE(length.data2, measurevar="Standard.Length", groupvars=c("sex","abundance"))
ggplot(tgc8, aes(x=abundance,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Mosquitofish Abundance") + theme(plot.title = element_text(hjust = 0.5))

##### Log zoops #####

tgc9<-summarySE(length.data2, measurevar="Standard.Length", groupvars=c("sex","logzoops"))
ggplot(tgc9, aes(x=logzoops,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Zooplankton") + theme(plot.title = element_text(hjust = 0.5))

##### Log cladocerans #####

tgc10<-summarySE(length.data2, measurevar="Standard.Length", groupvars=c("sex","logclad"))
ggplot(tgc10, aes(x=logclad,y=Standard.Length))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=Standard.Length-se, ymax=Standard.Length+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Cladoceran") + theme(plot.title = element_text(hjust = 0.5))



