
#plot lat_time and size by sex to see if there is a relationship
#Remove terms that don't make biological sense





boldness<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Predator Data Master2.csv")

##############################################################################################################################

# Data Manipulation

attach(boldness)

pop<-as.factor(rep(boldness$Population, 2))

#Stack two columns
sex<-as.factor(c(rep("F", 387), rep("M", 387)))

pair<-as.factor(c(rep(paste("P",as.character(1:387), sep=""),2)))

pred<-as.factor(rep(boldness$Pred...., 2))
#Change name of items in column, must turn it into a character first
pred<-as.character(pred)
pred[(pred=="+")]<-"bass"
pred[(pred=="-")]<-"np"
pred[(pred=="MVC")]<-"mv"
pred<-as.factor(pred)

location<-as.factor(rep(boldness$Location, 2))

abundance<-(rep(boldness$Mosquitofish.Abundance, 2))

lat_time<-c(boldness$F.Latency.time, boldness$M.latency.time)
lat_time<-as.numeric(lat_time)

length<-c(boldness$F.Length, boldness$M.Length)
length<-as.numeric(length)


#Create data frame with all the factors in it
bold.data<-data.frame(pop, sex, pair, pred, location, abundance, lat_time, length)

#Deletet factors to prevent issues when running models later
rm(pop)
rm(sex)
rm(pair)
rm(pred)
rm(location)
rm(abundance)
rm(lat_time)
rm(length)
detach(boldness)

# Select rows without NA's and then create data frame without the NA's
inc<-which(bold.data$lat_time !="NA") 
bold.data<-bold.data[inc,]
rm(inc)

# Add in escape by creating dataframe with all 1's and then turning any with lat time = 600 into 0
bold.data$escape<-"1"
bold.data$escape[which(bold.data$lat_time==600)] <- "0"
bold.data$escape<-as.numeric(bold.data$escape)



#####################################################
### Add in environmental data

env_factors<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Environmental pond data for PCA_R.csv")
env_data<-env_factors[,-c(1,2,3,4,6,8,13,17)]
env_data$logzoops<-log(env_factors$total_zoops_plus_1)
env_data$logcladoceran<-log(env_factors$total_cladocerans_plus_1)
env_data$pop<-env_factors$population
env_data<-env_data[-c(3,6,13,14,15),]
env_data$bg<-as.numeric(c("0","0","0","0","1","0","0","0","0","1","0","0","0","1","1","0","1","0","0","0"))
env_data$bass<-as.numeric(c("1","0","0","0","0","0","0","1","0","0","0","0","0","0.1","1","0","0","0","0","0"))
env_data$water_clarity<-as.factor(c("M","C","C","C","M","C","C","C","C","M","C","C","C","M","M","M","M","M","C","M"))
env_data$total_zoops<-as.numeric(env_data$total_zoops)

###Join env_data and boldness data using the common column "pop" (must have same name in both)
library(plyr)
bold.data2<-join(bold.data,env_data)


#############################################################################################################################
### Is data correlated? In order to check must remove all factors and NA's from dataframe

cor_check<-bold.data2[,-c(1,2,3,4,5,23)]
inc2<-which(cor_check$PO4 !="NA")
cor_check<-cor_check[inc2,]
inc3<-which(cor_check$length !="NA")
cor_check<-cor_check[inc3,]
check<-cor(cor_check)
View(check)


#############################################################################################################################
#### Fit best model using glmulti with all fish (should NH4 be included???)


library(glmulti)

bold.data3<-bold.data2

### Set confetsize as either 2^(#factors) or just a large number so it keeps all models
multi<-glmulti(escape ~ abundance + logzoops + logcladoceran + chlA + PO4 + pH + temp_c + s_cond + DO + bg + bass,
               level = 1, data = bold.data3, crit = "aicc", fitfunction = "glm", confsetsize =  4096)

print(multi)
plot(multi)

tmp <- weightable(multi)
tmp <- tmp[tmp$aicc <= min(tmp$aicc) + 2,]
tmp

summary(multi@objects[[1]])

plot(multi, type="s")

############################################
### Fit best model without 600+ fish

bold.data4<-bold.data3
inc4<-which(bold.data4$escape !=0)
bold.data4<-bold.data4[inc4,]


multi2<-glmulti(lat_time ~ abundance + logzoops + logcladoceran + chlA + PO4 + pH + temp_c + s_cond + DO + bg + bass,
                level = 1, data = bold.data4, crit = "aicc", fitfunction = "glm", confsetsize =  4096)

print(multi2)
plot(multi2)

tmp2 <- weightable(multi2)
tmp2 <- tmp2[tmp2$aicc <= min(tmp2$aicc) + 2,]
tmp2

summary(multi2@objects[[1]])

plot(multi2, type="s")


#############################################################################################################################
### Cox Regression


######Kaplan Meier Analysis

library(survival)
library(dplyr)
library(OIsurv) # Aumatically loads KMsurv
library(ranger)
library(ggplot2)

bold.data5<-bold.data2
coxdata<-bold.data5

y_surv <- Surv(coxdata$lat_time, coxdata$escape)
y_surv
fit1_surv <- survfit(y_surv ~ 1)
summary(fit1_surv)

cb <- confBands(y_surv, type = "hall")
plot(fit1_surv,main = 'Kaplan Meyer Plot with confidence bands')
lines(cb, col = "red",lty = 3)
legend(1000, 0.99, legend = c('K-M survival estimate','pointwise intervals', 'Hall-Werner conf bands'), lty = 1:3)

bold.data5$surv<-y_surv


##################################################
### Cox Proportional Hazards Model with Random

library(coxme)
library(rms)
library(car)

bold.data6<-bold.data2
inc5<-which(bold.data6$length != "NA")
bold.data6<-bold.data6[inc5,]

coxme_surv<-coxme(surv ~ pop + sex + length + (1|pair), data = bold.data6)

summary(coxme_surv)
Anova(coxme_surv)

### Need to run this in order to get predict function to work. Don't mess with it!
predict.coxme <- function(object, newdata, 
  type=c("lp", "risk")) {
  # This is an early skeleton of the function
  type <-match.arg(type)
  n <- object$n
  Terms <-  object$terms
  
  if (!missing(newdata)) stop("newdata argument not yet supported")
  
  out <- object$linear.predictor
  if (type=="risk") out <- exp(out)
  if (!is.null(object$na.action))
    napredict(object$na.action, out)
  else out
}


bold.data6$pred.me.cap<-predict(coxme_surv)



# Bad! coxme_surv2<-coxme(surv ~ water_clarity + logzoops + logcladoceran + abundance + chlA + PO4 + pH + temp_c 
#                   + s_cond + DO + bg_density + bass_density + (1|pair) + (1|pop), data = bold.data5)

summary(coxme_surv2)
Anova(coxme_surv2)

### Can I use contrast as a type of post hoc?
?contrast




##########################################################################################################################
##### Data manipulation to get likelihood to escape (try chi squared test)


Ant<-bold.data2[c(1:15,386:400),]
AW<-bold.data2[c(16:35,401:420),]
CCO<-bold.data2[c(36:55,421:440),]
CCO_A<-bold.data2[c(56:72,441:459),]
DeA<-bold.data2[c(73:92,460:479),]
DeL<-bold.data2[c(93:112,480:499),]
DoD<-bold.data2[c(113:132,500:519),]
FC<-bold.data2[c(133:152,520:539),]
FS<-bold.data2[c(153:172,540:559),]
Har<-bold.data2[c(173:192,560:579),]
K2<-bold.data2[c(193:212,580:599),]
LHC<-bold.data2[c(213:232,600:619),]
NBLM<-bold.data2[c(233,252,620:639),]
NL<-bold.data2[c(253:265,640:652),]
Sch<-bold.data2[c(266,285,653:672),]
Sho<-bold.data2[c(286,305,673:692),]
SpH<-bold.data2[c(306:325,693:712),]
WatL<-bold.data2[c(326:345,713:732),]
WSU<-bold.data2[c(346:365,733:752),]
Yolo<-bold.data2[c(366:385,753:772),]


pop<-c("Ant","AW","CCO","CCOA","DeA","DeL","DoD","FC","FS","Har","K2","LHC","NBLM","NL","Sch","Sho","SpH","WatL","WSU","Yolo")
Escape_Probability<-c(mean(Ant$escape),mean(AW$escape),mean(CCO$escape),mean(CCO_A$escape),mean(DeA$escape),mean(DeL$escape),
                      mean(DoD$escape),mean(FC$escape),mean(FS$escape),mean(Har$escape),mean(K2$escape),mean(LHC$escape),
                      mean(NBLM$escape),mean(NL$escape),mean(Sch$escape),mean(Sho$escape),mean(SpH$escape),mean(WatL$escape),
                      mean(WSU$escape),mean(Yolo$escape))
Pred_Regime<-c("b","np","mvc","mvc","bg","np","np","b","np","bg","np","np","np","b","b","np","bg","np","np","mvc")
Location<-c("SC","B","MVC","MVC","SC","SC","SC","B","B","SC","B","B","B","SC","SC","SC","SC","SC","B","MVC")

data_escape<-data.frame(pop,Escape_Probability, Pred_Regime)
data_escape<-join(data_escape, env_data)
data_escape$pop<-factor(data_escape$pop, levels=c("Ant","NL","Sch","FC","DeL","DoD","Sho","WatL","AW","FS","K2","NBLM","LHC","WSU","DeA","Har","SpH","CCO","CCOA","Yolo"))

rm(Ant)
rm(AW)
rm(CCO)
rm(CCO_A)
rm(DeA)
rm(DeL)
rm(DoD)
rm(FC)
rm(FS)
rm(Har)
rm(K2)
rm(LHC)
rm(NBLM)
rm(NL)
rm(Sch)
rm(Sho)
rm(WatL)
rm(WSU)
rm(Yolo)
rm(pop)
rm(Escape_Probability)
rm(Pred_Regime)
rm(Location)


ggplot(data_escape, aes(x=pop,y=Escape_Probability))+geom_point(aes(colour = Pred_Regime), size=3) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Escape") + theme(plot.title = element_text(hjust = 0.5))




#########################################################################################################################
############ GLM

bold.data7<-bold.data2

glm1<-glm(escape ~ pop, family=binomial, data = bold.data7)

summary(glm1)
Anova(glm1)

library(multcomp)
summary(glht(glm1, mcp(pop="Tukey")))



### Change Ant name
Bold.data.Ant<-bold.data2
Bold.data.Ant$escape<-as.numeric(Bold.data.Ant$escape)
pop_ant<-Bold.data.Ant$pop
Bold.data.Ant<-Bold.data.Ant[,-1]
pop_ant<-as.character(pop_ant)
pop_ant[(pop_ant=="Ant")]<-"Telli"
pop_ant<-as.factor(pop_ant)
Bold.data.Ant$pop<-pop_ant


glm.ant<-glm(escape ~ pop, family = binomial, data = Bold.data.Ant)
summary(glm.ant)
Anova(glm.ant) ### Anova still says that pop is significant but summary does not show any as significant

################################################
########### GLMM


library(lme4)
library(nlme)
library(arm)
library(glmm)

######## Random intercept model
mod_lmer1<-lmer(escape ~ pred + (1|pop),data=bold.data7)

mod_lmer2<-lmer(lat_time ~ pop + (1|pair),data=bold.data7)

######## Random slope + intercept
mod_glmer1<-glmer(escape ~ pred +(1|pop),data=bold.data7,family="binomial")

sal <- glmm(escape ~ pred, random = list(~0 + pop), varcomps.names = c("pop"), 
            data = bold.data7, family.glmm = bernoulli.glmm, m = 10^4, debug = TRUE)

summary(sal)
confint(sal)

mod_glmer2<-glmer(lat_time ~ pop +(1|pair),data=bold.data7,family="poisson")

sal2 <- glmm(escape ~ pop, random = list(~0 + pair), varcomps.names = c("pair"), 
            data = bold.data7, family.glmm = bernoulli.glmm, m = 10^4, debug = TRUE)

summary(sal)
confint(sal)

#####################################################################
#### GLMM with multinomial distribution like in paper Sommer-Trembo et al. Help!!!

bold.data8<-bold.data5
attach(bold.data8)
bold.data8$lat_time[(lat_time<=200)]<-1
bold.data8$lat_time[(lat_time>=201)]<-2
bold.data8$lat_time[(lat_time>=401)]<-3

hist(bold.data8$lat_time)


library(lme4)
library(nlme)
library(arm)
library(glmm)

######## Random intercept model
lmer1<-lmer(lat_time ~ pop + (1|pair),data=bold.data8)

######## Random slope + intercept
glmer1<-glmer(lat_time ~ pop +(1|pair),data=bold.data8, family = "poisson")

sal <- glmm(lat_time ~ pop, random = list(~0 + pair), varcomps.names = c("pair"), 
            data = bold.data8, family.glmm = poisson.glmm, m = 10^4, debug = TRUE)
summary(sal)
confint(sal)




#############################################################################################################################
###### Plot of lat_time by pop with error bars, includes 600 fish

library(ggplot2)
library(Rmisc) 

tgc <- summarySE(bold.data5, measurevar="lat_time", groupvars=c("pop","pred"))
tgc$pop<-factor(tgc$pop, levels=c("Ant","NL","Schwann","FC","DeL","Dod","Sho","WatL","AW","FS","K2","NBLM","LHC","WSU","DeA","Harkin","SpH","CCO","CCO Albino","Yolo"))

ggplot(tgc, aes(x=pop,y=lat_time))+geom_point(aes(colour = pred), size=4) +
  geom_errorbar(aes(ymin=lat_time-se, ymax=lat_time+se)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Escape") + theme(plot.title = element_text(hjust = 0.5))



######################################################## NEED TO EDIT
### Plot of lat_time by pop wihtout 600 fish
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















