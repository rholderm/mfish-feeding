#Package to install

rr<-"http://www.math.mcmaster.ca/bolker/R"
install.packages(c("glmmADMB","coefplot2"),type="source",repos=rr)


boldness<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Predator Data Master.csv")


#############################################################################################################################
### Data manipulation


attach(boldness)
head(boldness)
data<-data.frame
levels(boldness$Population)

Population
head(boldness)

pop<-as.factor(rep(boldness$Population, 2))
pop


sex<-as.factor(c(rep("F", 387), rep("M", 387)))

sex

pair<-as.factor(c(rep(paste("P",as.character(1:387), sep=""),2)))
pair
pred<-as.factor(rep(boldness$Pred...., 2))
pred

pred<-as.character(pred)
pred[(pred=="+")]<-"bass"
pred[(pred=="-")]<-"np"
pred[(pred=="MVC")]<-"mv"
pred<-as.factor(pred)


location<-as.factor(rep(boldness$Location, 2))




abundance<-(rep(boldness$Mosquitofish.Abundance, 2))

lat_time<-c(F.Latency.time, M.latency.time)
lat_time


bold.data<-data.frame(pop, sex, pair, pred, location, abundance, lat_time)
bold.data

include<-which(lat_time !="NA") #Selecting rows that do not contain any NA
bold.data2<-bold.data[include,] #creating a new data frame that only contains the rows without NA's
attach(bold.data2)
head(bold.data2)
hist(bold.data2$lat_time)
str(bold.data2)

bold.data2$escape<-"1"
bold.data2$escape[which(bold.data2$lat_time==600)] <- "0"
attach(bold.data2)
head(bold.data2)
str(bold.data2)

env_factors<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Environmental pond data for PCA_R.csv")
str(env_factors)
env_data<-env_factors[,-c(1,2,3,4,6,8,13,17)]
env_data$logzoops<-log(env_factors$total_zoops_plus_1)
env_data$logcladoceran<-log(env_factors$total_cladocerans_plus_1)
env_data$pop<-env_factors$population
env_data$pop
env_data<-env_data[-c(3,6,13,14,15),]
env_data
env_data$bg_density<-as.numeric(c("0","0","0","0","1.25","0","0","0","0","16.25","0","0","0","1.67","0.5","0","69.5","0","0","0"))
env_data$bass_density<- as.numeric(c("2","0","0","0","0","0","0","5.5","0","0","0","0","0","0.83","5.5","0","0","0","0","0"))
str(env_data)

library(plyr)
bold.data3<-join(bold.data2, env_data)

rm(pop)
rm(sex)
rm(pair)
rm(pred)
rm(location)
rm(abundance)
rm(lat_time)


head(bold.data3)
levels(bold.data3$pred)
str(bold.data3)


env_data$predators<-c("1","0","0","0","0","0","2","0","0","1","0","2","0","0","0","1","0","0","1","1","0","2","0","0","0")

##################################################################################################################
### Kaplan Meier Analysis

library(survival)
library(dplyr)
library(OIsurv) # Aumatically loads KMsurv
library(ranger)
library(ggplot2)


head(bold.data3)
coxdata<-bold.data3
coxdata<-coxdata[,-c(7,8)]
coxdata$t1<-bold.data3$lat_time
coxdata$d1<-bold.data3$escape
coxdata$t1<-as.numeric(coxdata$t1)
coxdata$d1<-as.numeric(coxdata$d1)
head(coxdata)

y_surv <- Surv(coxdata$t1, coxdata$d1)
y_surv
fit1_surv <- survfit(y_surv ~ 1)
summary(fit1_surv)

cb <- confBands(y_surv, type = "hall")
plot(fit1_surv,main = 'Kaplan Meyer Plot with confidence bands')
lines(cb, col = "red",lty = 3)
legend(1000, 0.99, legend = c('K-M survival estimate','pointwise intervals', 'Hall-Werner conf bands'), lty = 1:3)

bold.data3$surv<-y_surv
str(bold.data3)

##################################################
### Cox Proportional Hazards Model
head(bold.data3)
cox_surv<-coxph(y_surv ~ pop + pair + fish length + sex, data=bold.data3)
summary(cox_surv)
library(car)
Anova(cox_surv, type='II')

#################################################
### With random effect

library(coxme)
head(bold.data3)
attach(bold.data3)
coxme_surv<-coxme(surv ~ location + pred + sex + temp_c + chlA + PO4 + NH4 + pH + s_cond + DO + logzoops + logcladoceran + (1|pair), data=bold.data3)

summary(coxme_surv)
Anova(coxme_surv)

head(bold.data3)


### look up hazard ratio


#########################################################################################################################
############ GLM
attach(bold.data3)
bold.data3$escape<-as.numeric(bold.data3$escape)
head(bold.data3)

glm1<-glm(escape ~ pop, family=binomial, data = bold.data3)

summary(glm1)
Anova(glm1)

library(multcomp)
summary(glht(glm1, mcp(pred="Tukey")))



### Change Ant name
Bold.data.Ant<-bold.data3
Bold.data.Ant$escape<-as.numeric(Bold.data.Ant$escape)
pop_ant<-Bold.data.Ant$pop
Bold.data.Ant<-Bold.data.Ant[,-1]
pop_ant<-as.character(pop_ant)
pop_ant[(pop_ant=="Ant")]<-"Telli"
pop_ant<-as.factor(pop_ant)
Bold.data.Ant$pop<-pop_ant

pop_ant


glm.ant<-glm(escape ~ pop, family = binomial, data = Bold.data.Ant)
summary(glm.ant)


################################################
########### GLMM


library(lme4)
library(nlme)
library(arm)
library(glmm)

######## Random intercept model
mod_lmer1<-lmer(escape ~ pred + (1|pop),data=bold.data3)

######## Random slope + intercept
mod_glmer1<-glmer(escape ~ pred +(1|pop),data=bold.data3,family="binomial")

sal <- glmm(escape ~ pred, random = list(~0 + pop), varcomps.names = c("pop"), 
            data = bold.data3, family.glmm = bernoulli.glmm, m = 10^4, debug = TRUE)
summary(sal)
confint(sal)



##########################################################################################################################
##### Data manipulation to get likelihood to escape

str(bold.data3)
bold.data3$escape<-as.numeric(bold.data3$escape)
bold.data3$pop
Ant<-bold.data3[c(1:15,386:400),]
AW<-bold.data3[c(16:35,401:420),]
CCO<-bold.data3[c(36:55,421:440),]
CCO_A<-bold.data3[c(56:72,441:459),]
DeA<-bold.data3[c(73:92,460:479),]
DeL<-bold.data3[c(93:112,480:499),]
DoD<-bold.data3[c(113:132,500:519),]
FC<-bold.data3[c(133:152,520:539),]
FS<-bold.data3[c(153:172,540:559),]
Har<-bold.data3[c(173:192,560:579),]
K2<-bold.data3[c(193:212,580:599),]
LHC<-bold.data3[c(213:232,600:619),]
NBLM<-bold.data3[c(233,252,620:639),]
NL<-bold.data3[c(253:265,640:652),]
Sch<-bold.data3[c(266,285,653:672),]
Sho<-bold.data3[c(286,305,673:692),]
SpH<-bold.data3[c(306:325,693:712),]
WatL<-bold.data3[c(326:345,713:732),]
WSU<-bold.data3[c(346:365,733:752),]
Yolo<-bold.data3[c(366:385,753:772),]


pop<-c("Ant","AW","CCO","CCOA","DeA","DeL","DoD","FC","FS","Har","K2","LHC","NBLM","NL","Sch","Sho","SpH","WatL","WSU","Yolo")
Escape_Probability<-c(mean(Ant$escape),mean(AW$escape),mean(CCO$escape),mean(CCO_A$escape),mean(DeA$escape),mean(DeL$escape),
                    mean(DoD$escape),mean(FC$escape),mean(FS$escape),mean(Har$escape),mean(K2$escape),mean(LHC$escape),
                    mean(NBLM$escape),mean(NL$escape),mean(Sch$escape),mean(Sho$escape),mean(SpH$escape),mean(WatL$escape),
                    mean(WSU$escape),mean(Yolo$escape))
Pred_Regime<-c("b","np","mvc","mvc","bg","np","np","b","np","bg","np","np","np","b","b","np","bg","np","np","mvc")
Location<-c("SC","B","MVC","MVC","SC","SC","SC","B","B","SC","B","B","B","SC","SC","SC","SC","SC","B","MVC")

data_escape<-data.frame(pop,Escape_Probability, Pred_Regime)
data_escape2<-join(data_escape, env_data)
data_escape2
str(data_escape2)
data_escape2$pop<-factor(data_escape2$pop, levels=c("Ant","NL","Sch","FC","DeL","DoD","Sho","WatL","AW","FS","K2","NBLM","LHC","WSU","DeA","Har","SpH","CCO","CCOA","Yolo"))

ggplot(data_escape2, aes(x=pop,y=Escape_Probability))+geom_point(aes(colour = Pred_Regime), size=3) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Escape") + theme(plot.title = element_text(hjust = 0.5))






####################################################################################################################
########## Lat time differences minus 600 time points


str(bold.data3)
include2<-which(bold.data3$escape !=0)
bold.data4<-bold.data3[include2,]
attach(bold.data4)
str(bold.data4)
glm2<-glm(lat_time ~ abundance + logzoops + logcladoceran + PO4 + NH4 + chlA + temp_c + bg_density + bass_density, data = bold.data4)
summary(glm2)


sum <- summarySE(bold.data4, measurevar="lat_time", groupvars=c("pop","pred"))
sum$pop<-factor(sum$pop, levels=c("Ant","NL","Schwann","FC","DeL","Dod","Sho","WatL","AW","FS","K2","NBLM","LHC","WSU","DeA","Harkin","SpH","CCO","CCO Albino","Yolo"))

ggplot(sum, aes(x=pop,y=lat_time))+geom_point(aes(colour = pred), size=4) +
  geom_errorbar(aes(ymin=lat_time-se, ymax=lat_time+se)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Escape") + theme(plot.title = element_text(hjust = 0.5))

