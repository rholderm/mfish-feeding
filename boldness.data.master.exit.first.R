
#     #     #     #     #     #     #     #     #     #
 # # # Looking at the fish that came out first # # #
#     #     #     #     #     #     #     #     #     #


boldness<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Predator Data Master2.csv")

M_bold<-boldness[boldness$F.Latency.time > boldness$M.latency.time,]
M_bold<-M_bold[,-c(2,3,4,5,6,7,8,9,10,11,12,15,16,17,18,19,20,21,22)]
M_bold$lat_time<-M_bold$M.latency.time
M_bold$length<-M_bold$M.Length
M_bold$pop<-M_bold$Population
M_bold$sex<-rep("M",249)
M_bold<-M_bold[,-c(1,2,3)]
str(M_bold)

F_bold<-boldness[boldness$M.latency.time > boldness$F.Latency.time,]
F_bold<-F_bold[,-c(2,3,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20,21,22)]
F_bold$lat_time<-F_bold$F.Latency.time
F_bold$length<-F_bold$F.Length
F_bold$pop<-F_bold$Population
F_bold$sex<-rep("F",127)
F_bold<-F_bold[,-c(1,2,3)]
str(F_bold)

bold.first <- rbind(M_bold, F_bold)
inc1<-which(bold.first$lat_time !="NA")
bold.first<-bold.first[inc1,]
rm(inc1)


##############################################################################################################################
### Add in env_data

env_factors<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Environmental pond data for PCA_R.csv")
str(env_factors)
env_data<-env_factors[,-c(1,3,4,5,6,7,8,17)]
env_data$logzoops<-log(env_factors$total_zoops_plus_1)
env_data$logcladoceran<-log(env_factors$total_cladocerans_plus_1)
env_data$abundance<-env_factors$mfish_abundance
env_data$pop<-c("Ant","AW","CaK","CCO","CCO Albino","Corcoran","DeA","DeL","Dod","FC","FS","Harkin","K2","K5","Larsen","LAW","LHC","NBLM","NL","Schwann","Sho","SpH","WatL","WSU","Yolo")
env_data<-env_data[-c(3,6,14,15,16),]
env_data$bg<-as.numeric(c("0","0","0","0","1","0","0","0","0","1","0","0","0","1","1","0","1","0","0","0"))
env_data$bass<-as.numeric(c("1","0","0","0","0","0","0","1","0","1","0","0","0","1","1","0","0","0","0","0"))
env_data$water_clarity<-as.factor(c("M","C","C","C","M","C","C","C","C","M","C","C","C","M","M","M","M","M","C","M"))

###Join env_data and boldness data using the common column "pop" (must have same name in both)
library(plyr)
bold.data2<-join(bold.first,env_data)


### Examining log_lat for pops (minus MVC)

library(lme4)
library(car)

inc4<-which(bold.data2$location !="MVC")
bold.data4<-bold.data2[inc4,]
rm(inc4)

bold.data4$log_lat<-log(bold.data4$lat_time)

mod1<-lmer(log_lat ~ abundance + logzoops + bg + bass + water_clarity + temp_c + sex + (1|pop), data = bold.data4)
Anova(mod1, type = "II")

library(MuMIn)
# change na. action 
options(na.action = "na.fail")
dredge(mod1, rank = AICc, extra = "R^2")







##############################################################################################################################
### Plotting lat_time

library(ggplot2)
library(Rmisc)

bold.data2$log_lat<-log(bold.data2$lat_time)




### By temp, With sex, no pop labels
tgc<-summarySE(bold.data2, measurevar="lat_time", groupvars=c("temp_c","pop","pred_regime","sex"))
ggplot(tgc, aes(x=temp_c,y=lat_time))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=lat_time-se, ymax=lat_time+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Temp") + theme(plot.title = element_text(hjust = 0.5))

###By temp, pop labels, no sex
tgc1<-summarySE(bold.data2, measurevar="lat_time", groupvars=c("temp_c","pop","pred_regime"))
ggplot(tgc1, aes(x=temp_c,y=lat_time))+geom_point() +
  geom_errorbar(aes(ymin=lat_time-se, ymax=lat_time+se)) + 
  geom_text(aes(label=pop), size=4,nudge_x = .3, nudge_y = -.2) +
theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Temp") + theme(plot.title = element_text(hjust = 0.5))

### By location
tgc2<-summarySE(bold.data2, measurevar="lat_time", groupvars=c("location"))
ggplot(tgc2, aes(x=location,y=lat_time))+geom_point() +
  geom_errorbar(aes(ymin=lat_time-se, ymax=lat_time+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Location") + theme(plot.title = element_text(hjust = 0.5))

### By pred regime with sex #NO MVC#
tgc3<-summarySE(bold.data4, measurevar="lat_time", groupvars=c("pred_regime","sex"))
ggplot(tgc3, aes(x=pred_regime,y=lat_time))+geom_point(aes(colour = sex), size=4) +
  geom_errorbar(aes(ymin=lat_time-se, ymax=lat_time+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Predator Regime") + theme(plot.title = element_text(hjust = 0.5))

### By pred abundance, no MVC

tgc4<-summarySE(bold.data4, measurevar="lat_time", groupvars=c("bass"))
ggplot(tgc4, aes(x=bass,y=lat_time))+geom_point() +
  geom_errorbar(aes(ymin=lat_time-se, ymax=lat_time+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass") + theme(plot.title = element_text(hjust = 0.5))

tgc5<-summarySE(bold.data4, measurevar="lat_time", groupvars=c("bg"))
ggplot(tgc5, aes(x=bg,y=lat_time))+geom_point() +
  geom_errorbar(aes(ymin=lat_time-se, ymax=lat_time+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG") + theme(plot.title = element_text(hjust = 0.5))


########################################
### Bishop pops only for Dave

inc2<-which(bold.data2$location == "B")
bold.data3<-bold.data2[inc2,]
inc3<-which(bold.data3$pop !="FC")
bold.data3<-bold.data3[inc3,]

tgc_dave <- summarySE(bold.data3, measurevar="lat_time", groupvars=c("temp_c","pop","pred_regime"))
tgc_dave2<- summarySE(bold.data3, measurevar="lat_time", groupvars=c("temp_c","pop","pred_regime","sex"))

ggplot(tgc_dave, aes(x=temp_c,y=lat_time))+geom_point(aes(colour = pop), size=4) +
  geom_errorbar(aes(ymin=lat_time-se, ymax=lat_time+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Temp") + theme(plot.title = element_text(hjust = 0.5))


### add text and shift text with geom_text and nudge_x or nudge_y
ggplot(tgc_dave2, aes(x=temp_c,y=lat_time))+geom_point(aes(colour = pop), size=4) +
  geom_errorbar(aes(ymin=lat_time-se, ymax=lat_time+se)) + 
  geom_text(aes(label=sex), size=4,nudge_x = .3, nudge_y = -.2)
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Temp") + theme(plot.title = element_text(hjust = 0.5))



