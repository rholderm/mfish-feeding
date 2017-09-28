

boldness2<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Predator Data Master2.csv")

##############################################################################################################################
####### Data Manipulation

attach(boldness2)
head(boldness2)
data2<-data.frame

pop<-as.factor(rep(boldness2$Population, 2))


sex<-as.factor(c(rep("F", 387), rep("M", 387)))
sex


pair<-as.factor(c(rep(paste("P",as.character(1:387), sep=""),2)))
pair


pred<-as.factor(rep(boldness2$Pred...., 2))
pred
pred<-as.character(pred)
pred[(pred=="+")]<-"bass"
pred[(pred=="-")]<-"np"
pred[(pred=="MVC")]<-"mv"
pred<-as.factor(pred)


location<-as.factor(rep(boldness2$Location, 2))
location


abundance<-(rep(boldness2$Mosquitofish.Abundance, 2))
abundance


lat_time<-c(F.Latency.time, M.latency.time)
lat_time


length<-c(F.Length, M.Length)
length


bold.data4<-data.frame(pop, sex, pair, pred, location, abundance, lat_time, length)
bold.data4


attach(bold.data4)
inc<-which(lat_time !="NA") #Selecting rows that do not contain any NA
bold.data4<-bold.data4[inc,] #creating a new data frame that only contains the rows without NA's
attach(bold.data4)
head(bold.data4)
hist(bold.data4$lat_time)
str(bold.data4)


bold.data4$escape<-"1"
bold.data4$escape[which(bold.data4$lat_time==600)] <- "0"
bold.data4$escape<-as.numeric(bold.data4$escape)
attach(bold.data4)
head(bold.data4)
str(bold.data4)

env_factors<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Environmental pond data for PCA_R.csv")
str(env_factors4)
env_data4<-env_factors4[,-c(1,2,3,4,6,8,13,17)]
env_data4$logzoops<-log(env_factors4$total_zoops_plus_1)
env_data4$logcladoceran<-log(env_factors4$total_cladocerans_plus_1)
env_data4$pop<-env_factors4$population
env_data4$pop
env_data4<-env_data4[-c(3,6,13,14,15),]
env_data4
env_data4$bg_density<-as.numeric(c("0","0","0","0","1.25","0","0","0","0","16.25","0","0","0","1.67","0.5","0","69.5","0","0","0"))
env_data4$bass_density<- as.numeric(c("2","0","0","0","0","0","0","5.5","0","0","0","0","0","0.83","5.5","0","0","0","0","0"))
env_data4$water_clarity<-as.factor(c("M","C","C","C","M","C","C","C","C","M","C","C","C","M","M","M","M","M","C","M"))
str(env_data4)

library(plyr)
bold.data5<-join(bold.data4, env_data4)

rm(pop)
rm(sex)
rm(pair)
rm(pred)
rm(location)
rm(abundance)
rm(lat_time)
rm(length)
detach(boldness2)
detach(bold.data4)


head(bold.data5)
str(bold.data5)



################################################################################################################
#### Plot of lat_time ~ pop with error bars

library(ggplot2)
library(Rmisc) 

tgc <- summarySE(bold.data5, measurevar="lat_time", groupvars=c("pop","pred"))
tgc$pop<-factor(tgc$pop, levels=c("Ant","NL","Schwann","FC","DeL","Dod","Sho","WatL","AW","FS","K2","NBLM","LHC","WSU","DeA","Harkin","SpH","CCO","CCO Albino","Yolo"))

ggplot(tgc, aes(x=pop,y=lat_time))+geom_point(aes(colour = pred), size=4) +
  geom_errorbar(aes(ymin=lat_time-se, ymax=lat_time+se)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Escape") + theme(plot.title = element_text(hjust = 0.5))



##################################################################################################################
### Kaplan Meier Analysis

library(survival)
library(dplyr)
library(OIsurv) # Aumatically loads KMsurv
library(ranger)
library(ggplot2)


head(bold.data5)
coxdata2<-bold.data5
coxdata2<-coxdata2[,-c(7,8)]
coxdata2$t1<-bold.data5$lat_time
coxdata2$d1<-bold.data5$escape
coxdata2$t1<-as.numeric(coxdata2$t1)
coxdata2$d1<-as.numeric(coxdata2$d1)
head(coxdata2)

y_surv2 <- Surv(coxdata2$t1, coxdata2$d1)
y_surv2
fit1_surv2 <- survfit(y_surv2 ~ 1)
summary(fit1_surv2)

cb2 <- confBands(y_surv2, type = "hall")
plot(fit1_surv2,main = 'Kaplan Meyer Plot with confidence bands')
lines(cb2, col = "red",lty = 3)
legend(1000, 0.99, legend = c('K-M survival estimate','pointwise intervals', 'Hall-Werner conf bands'), lty = 1:3)

bold.data5$surv<-y_surv2
str(bold.data5)


#################################################
### Cox With random effect

library(coxme)
library(rms)
library(car)
bold.data7<-bold.data5
detach(bold.data5)
attach(bold.data7)
str(bold.data7)
bold.data7$lat_time<-as.numeric(lat_time)
bold.data7$length<-as.numeric(length)
bold.data7$escape<-as.numeric(escape)
bold.data7$total_zoops<-as.numeric(total_zoops)
inc2<-which(bold.data7$length !="NA")
bold.data7<-bold.data7[inc2,]

coxme_surv2<-coxme(surv ~ pop + sex + length + (1|pair), data = bold.data7)


summary(coxme_surv2)
Anova(coxme_surv2)

cox<-coxph(surv ~ pop + sex + length, data = bold.data7)
Anova(cox)
plot(cox.zph(cox))

inc3<-which(bold.data7$PO4 !="NA")
bold.data8<-bold.data7[inc3,]

cox2<-coxph(surv ~ logzoops + logcladoceran + abundance + chlA + PO4 + pH + temp_c 
            + s_cond + DO + bg_density + bass_density, data=bold.data8)

Anova(cox2)
plot(cox.zph(cox2))


coxme_surv3<-coxme(surv ~ water_clarity + logzoops + logcladoceran + abundance + chlA + PO4 + pH + temp_c 
                     + s_cond + DO + bg_density + bass_density + (1|pair) + (1|pop), data = bold.data8)

summary(coxme_surv3)
Anova(coxme_surv3)



contrast()


########################################################################
####### Check correlation
env_factors_cor<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\2017\\Environmental pond data for PCA_R.csv")
env_factors_cor$logzoops<-log(env_factors_cor$total_zoops_plus_1)
env_factors_cor$logclad<-log(env_factors_cor$total_cladocerans_plus_1)
env_factors_cor_rm<-env_factors_cor[-c(3,6,13,14,15),]
env_factors_cor_rm$bg_density<-as.numeric(c("0","0","0","0","1.25","0","0","0","0","16.25","0","0","0","1.67","0.5","0","69.5","0","0","0"))
env_factors_cor_rm$bass_density<- as.numeric(c("2","0","0","0","0","0","0","5.5","0","0","0","0","0","0.83","5.5","0","0","0","0","0"))

env_factors_cor_rm2<-env_factors_cor_rm[-c(3,4,20),-c(1,2,4,5,6,7,8,11,13,17)]

env_factors_cor_rm2
cor_check<-cor(env_factors_cor_rm2)
View(cor_check)




################################################################################################################
#### GLMM 
bold.data9<-bold.data5
str(bold.data9)

######## Random intercept model
mod_lmer1<-lmer(lat_time ~ pop + (1|pair),data=bold.data9)

######## Random slope + intercept
mod_glmer1<-glmer(lat_time ~ pop +(1|pair),data=bold.data9,family="poisson")

sal <- glmm(escape ~ pop, random = list(~0 + pair), varcomps.names = c("pair"), 
            data = bold.data9, family.glmm = bernoulli.glmm, m = 10^4, debug = TRUE)


summary(sal)
confint(sal)

#sal <- glmm(escape ~ 0 + pop, random = list(~ 0 + Female,
#                                           ~ 0 + Male), varcomps.names = c("F", "M"), data = salamander,
#            family.glmm = bernoulli.glmm, m = 10^4, debug = TRUE)


#####################################################################
#### With multinomial distribution like in paper Sommer-Trembo et al.

bold.data6<-bold.data5
attach(bold.data6)
bold.data6$lat_time[(lat_time<=200)]<-1
bold.data6$lat_time[(lat_time>=201)]<-2
bold.data6$lat_time[(lat_time>=401)]<-3

hist(bold.data6$lat_time)


library(lme4)
library(nlme)
library(arm)
library(glmm)

######## Random intercept model
lmer1<-lmer(lat_time ~ pop + (1|pair),data=bold.data6)

######## Random slope + intercept
glmer1<-glmer(lat_time ~ pop +(1|pair),data=bold.data6, family = "poisson")

sal <- glmm(lat_time ~ pop, random = list(~0 + pair), varcomps.names = c("pair"), 
            data = bold.data6, family.glmm = poisson.glmm, m = 10^4, debug = TRUE)
summary(sal)
confint(sal)


######################################################################################################################
###### Find best model, this includes the 600+ fish



?glmulti
library(glmulti)

bold.data11<-bold.data5
str(bold.data11)
attach(bold.data11)
bold.data11<-bold.data11[,-c(21,22)]

bg<-as.numeric(c("0","0","0","0","1","0","0","0","0","1","0","0","0","1","1","0","1","0","0","0"))
bass<- as.numeric(c("1","0","0","0","0","0","0","1","0","0","0","0","0","0.1","1","0","0","0","0","0"))
pop<-as.factor(c("Ant","AW","CCO","CCO Albino","DeA","DeL","Dod","FC","FS","Harkin","K2","LHC","NBLM","NL","Schwann","Sho","SpH","WatL","WSU","Yolo"))
pred_info<-data.frame(bg,bass,pop)

library(plyr)
bold.data12<-join(bold.data11,pred_info)


multi<-glmulti(lat_time ~ abundance + logzoops + logcladoceran + chlA + PO4 + pH + temp_c + s_cond + DO + bg + bass,
               level = 1, data = bold.data12, crit = "aicc", fitfunction = "glm", confsetsize =  4096)

print(multi)
plot(multi)

tmp <- weightable(multi)
tmp <- tmp[tmp$aicc <= min(tmp$aicc) + 2,]
tmp

summary(multi@objects[[1]])

plot(multi, type="s")


###########################################################3
###### Best model wiithour 600+ fish

inc5<-which(bold.data12$escape !=0)
bold.data13<-bold.data12[inc5,]


multi2<-glmulti(lat_time ~ abundance + logzoops + logcladoceran + chlA + PO4 + pH + temp_c + s_cond + DO + bg + bass,
               level = 1, data = bold.data13, crit = "aicc", fitfunction = "glm", confsetsize =  4096)

print(multi2)
plot(multi2)

tmp2 <- weightable(multi2)
tmp2 <- tmp2[tmp2$aicc <= min(tmp2$aicc) + 2,]
tmp2

summary(multi2@objects[[1]])

plot(multi2, type="s")







