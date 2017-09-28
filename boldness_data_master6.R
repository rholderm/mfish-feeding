


boldness<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Predator Data Master2.csv")
#############################################################################################################################

# Data Manipulation

attach(boldness)

pop<-as.factor(rep(boldness$Population, 2))

#Stack two columns
sex<-as.factor(c(rep("F", 387), rep("M", 387)))



pair<-c(paste("Ant",as.character(c(1:15))),paste("AW", as.character(c(1:20))),paste("CCO", as.character(c(1:20))),
                   paste("CCOA", as.character(c(1:19))),paste("DeA", as.character(c(1:20))),paste("DeL", as.character(c(1:20))),
                   paste("Dod", as.character(c(1:20))),paste("FC", as.character(c(1:20))),paste("FS", as.character(c(1:20))),
                   paste("Har", as.character(c(1:20))),paste("K2", as.character(c(1:20))),paste("LHC", as.character(c(1:20))),
                   paste("NBLM", as.character(c(1:20))),paste("NL", as.character(c(1:13))),paste("Sch", as.character(c(1:20))),
                   paste("Sho", as.character(c(1:20))),paste("SpH", as.character(c(1:20))),paste("WatL", as.character(c(1:20))),
                   paste("WSU", as.character(c(1:20))),paste("Yolo", as.character(c(1:20))),paste("Ant",as.character(c(1:15))),paste("AW", as.character(c(1:20))),paste("CCO", as.character(c(1:20))),
                   paste("CCOA", as.character(c(1:19))),paste("DeA", as.character(c(1:20))),paste("DeL", as.character(c(1:20))),
                   paste("Dod", as.character(c(1:20))),paste("FC", as.character(c(1:20))),paste("FS", as.character(c(1:20))),
                   paste("Har", as.character(c(1:20))),paste("K2", as.character(c(1:20))),paste("LHC", as.character(c(1:20))),
                   paste("NBLM", as.character(c(1:20))),paste("NL", as.character(c(1:13))),paste("Sch", as.character(c(1:20))),
                   paste("Sho", as.character(c(1:20))),paste("SpH", as.character(c(1:20))),paste("WatL", as.character(c(1:20))),
                   paste("WSU", as.character(c(1:20))),paste("Yolo", as.character(c(1:20))))

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
rm(pop) + rm(sex) + rm(pair) + rm(pred) + rm(location) + rm(abundance) + rm(lat_time) + rm(length)
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
env_data$pop<-c("Ant","AW","CaK","CCO","CCO Albino","Corcoran","DeA","DeL","Dod","FC","FS","Harkin","K2","K5","Larsen","LAW","LHC","NBLM","NL","Schwann","Sho","SpH","WatL","WSU","Yolo")
env_data<-env_data[-c(3,6,14,15,16),]
env_data$bg<-as.numeric(c("0","0","0","0","1","0","0","0","0","1","0","0","0","1","1","0","1","0","0","0"))
env_data$bass<-as.numeric(c("1","0","0","0","0","0","0","1","0","1","0","0","0","1","1","0","0","0","0","0"))
env_data$water_clarity<-as.factor(c("M","C","C","C","M","C","C","C","C","M","C","C","C","M","M","M","M","M","C","M"))
env_data$total_zoops<-as.numeric(env_data$total_zoops)

###Join env_data and boldness data using the common column "pop" (must have same name in both)
library(plyr)
bold.data2<-join(bold.data,env_data)


#############################################################################################################################
#############################################################################################################################
### Cox Regression


######Kaplan Meier Analysis
library(survival)
library(dplyr)
library(OIsurv) # Aumatically loads KMsurv
library(ranger)
library(ggplot2)
bold.data3<-bold.data2
coxdata<-bold.data3
y_surv <- Surv(coxdata$lat_time, coxdata$escape)
fit1_surv <- survfit(y_surv ~ 1)
cb <- confBands(y_surv, type = "hall")
plot(fit1_surv,main = 'Kaplan Meyer Plot with confidence bands')
lines(cb, col = "red",lty = 3)
legend(1000, 0.99, legend = c('K-M survival estimate','pointwise intervals', 'Hall-Werner conf bands'), lty = 1:3)

bold.data3$surv<-y_surv
rm(y_surv)


##################################################
### Cox Proportional Hazards Model with Random

library(coxme)
library(rms)
library(car)
bold.data4<-bold.data3
inc2<-which(bold.data4$length != "NA")
bold.data4<-bold.data4[inc2,]
rm(inc2)
coxme_surv<-coxme(surv ~ pop + sex + length + (1|pair), data = bold.data4)
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


bold.data4$pred.me.cap<-predict(coxme_surv)


##############################################################################################################################
##############################################################################################################################
### Cox regression mean for each population

### Missing lengths on 2 Harkin fish (1 Pair), removed them
Ant<-bold.data4[c(1:15,385:399),]
AW<-bold.data4[c(16:35,400:419),]
CCO<-bold.data4[c(36:55,420:439),]
CCO_A<-bold.data4[c(56:72,440:458),]
DeA<-bold.data4[c(73:92,459:478),]
DeL<-bold.data4[c(93:112,479:498),]
DoD<-bold.data4[c(113:132,499:518),]
FC<-bold.data4[c(133:152,519:538),]
FS<-bold.data4[c(153:172,539:558),]
Har<-bold.data4[c(173:191,559:577),]
K2<-bold.data4[c(192:211,578:597),]
LHC<-bold.data4[c(212:231,598:617),]
NBLM<-bold.data4[c(232,251,618:637),]
NL<-bold.data4[c(252:264,638:650),]
Sch<-bold.data4[c(265,284,651:670),]
Sho<-bold.data4[c(285,304,671:690),]
SpH<-bold.data4[c(305:324,691:710),]
WatL<-bold.data4[c(325:344,711:730),]
WSU<-bold.data4[c(345:366,731:750),]
Yolo<-bold.data4[c(367:384,751:770),]
env_data$pop

env_data$pred.mean<-c(mean(Ant$pred.me.cap),mean(AW$pred.me.cap),mean(CCO$pred.me.cap),mean(CCO_A$pred.me.cap),mean(DeA$pred.me.cap),
                      mean(DeL$pred.me.cap),mean(DoD$pred.me.cap),mean(FC$pred.me.cap),mean(FS$pred.me.cap),mean(Har$pred.me.cap),
                      mean(K2$pred.me.cap),mean(LHC$pred.me.cap),mean(NBLM$pred.me.cap),mean(NL$pred.me.cap),mean(Sch$pred.me.cap),
                      mean(Sho$pred.me.cap),mean(SpH$pred.me.cap),mean(WatL$pred.me.cap),mean(WSU$pred.me.cap),mean(Yolo$pred.me.cap))


rm(Ant) + rm(AW) + rm(CCO) + rm(CCO_A) + rm(DeA) + rm(DeL) + rm(DoD) + rm(FC) + rm(FS) + rm(Har) + rm(K2) + rm(LHC) +
  rm(NBLM) + rm(NL) + rm(Sch) + rm(Sho) + rm(SpH) + rm(WatL) + rm(WSU) + rm(Yolo)


##########################################################################################################################
##### mean likelihood to escape for each pop (try chi squared test)

#### Apply or aggregate function ####


Ant2<-bold.data2[c(1:15,386:400),]
AW2<-bold.data2[c(16:35,401:420),]
CCO2<-bold.data2[c(36:55,421:440),]
CCO_A2<-bold.data2[c(56:72,441:459),]
DeA2<-bold.data2[c(73:92,460:479),]
DeL2<-bold.data2[c(93:112,480:499),]
DoD2<-bold.data2[c(113:132,500:519),]
FC2<-bold.data2[c(133:152,520:539),]
FS2<-bold.data2[c(153:172,540:559),]
Har2<-bold.data2[c(173:192,560:579),]
K22<-bold.data2[c(193:212,580:599),]
LHC2<-bold.data2[c(213:232,600:619),]
NBLM2<-bold.data2[c(233,252,620:639),]
NL2<-bold.data2[c(253:265,640:652),]
Sch2<-bold.data2[c(266,285,653:672),]
Sho2<-bold.data2[c(286,305,673:692),]
SpH2<-bold.data2[c(306:325,693:712),]
WatL2<-bold.data2[c(326:345,713:732),]
WSU2<-bold.data2[c(346:365,733:752),]
Yolo2<-bold.data2[c(366:385,753:772),]

pop<-c("Ant","AW","CCO","CCOA","DeA","DeL","DoD","FC","FS","Har","K2","LHC","NBLM","NL","Sch","Sho","SpH","WatL","WSU","Yolo")
Escape_Probability<-c(mean(Ant2$escape),mean(AW2$escape),mean(CCO2$escape),mean(CCO_A2$escape),mean(DeA2$escape),mean(DeL2$escape),
                      mean(DoD2$escape),mean(FC2$escape),mean(FS2$escape),mean(Har2$escape),mean(K22$escape),mean(LHC2$escape),
                      mean(NBLM2$escape),mean(NL2$escape),mean(Sch2$escape),mean(Sho2$escape),mean(SpH2$escape),mean(WatL2$escape),
                      mean(WSU2$escape),mean(Yolo2$escape))
Pred_Regime<-c("b","np","mvc","mvc","bg","np","np","b","np","bg","np","np","np","b","b","np","bg","np","np","mvc")
Location<-c("SC","B","MVC","MVC","SC","SC","SC","B","B","SC","B","B","B","SC","SC","SC","SC","SC","B","MVC")

data_escape<-data.frame(pop,Escape_Probability, Pred_Regime)
data_escape<-join(data_escape, env_data)
data_escape$pop<-factor(data_escape$pop, levels=c("Ant","NL","Sch","FC","DeL","DoD","Sho","WatL","AW","FS","K2","NBLM","LHC","WSU","DeA","Har","SpH","CCO","CCOA","Yolo"))

env_data$escape_prob<-data_escape$Escape_Probability

rm(Ant2) + rm(AW2) + rm(CCO2) + rm(CCO_A2) + rm(DeA2) + rm(DeL2) + rm(DoD2) + rm(FC2) + rm(FS2) + rm(Har2) + rm(K22) + rm(LHC2) +
  rm(NBLM2) + rm(NL2) + rm(Sch2) + rm(Sho2) + rm(SpH2) + rm(WatL2) + rm(WSU2) + rm(Yolo2) + rm(pop) + rm(Escape_Probability) + rm(Pred_Regime) + rm(Location)



##############################################################################################################################
##############################################################################################################################
### Examinig Cox Regression output


bold.data5<-bold.data4[,-c(1,2,3,4,5,23,24)]
bold.data.scale<-as.data.frame(scale(bold.data5))
bold.data.scale$pop<-bold.data4$pop
bold.data.scale$water_clarity<-bold.data4$water_clarity
bold.data.scale$escape<-bold.data5$escape
bold.data.scale$pair<-bold.data4$pair

head(bold.data.scale)
library(lme4)

##################################################
### Using whole data set

mod<-lmer(pred.me.cap ~ abundance + logzoops + bg + bass + water_clarity + (1|pop), data = bold.data.scale)
summary(mod)
Anova(mod, type = "II")

mod2<-lmer(pred.me.cap ~ abundance + logzoops + bg + bass + water_clarity + (1|pop) + (1|pair), data = bold.data.scale)
summary(mod2)
Anova(mod2, type = "II")

mod3<-lmer(pred.me.cap ~ pair + (1|pop), data = bold.data.scale)
Anova(mod3, type = "II")


###################################################
### Using means of cox

env_factors2<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\2017\\Environmental pond data for PCA_R.csv")
env_factors2<-env_factors[-c(3,6,13,14,15),]
env_data2<-env_data
env_data2$abundance<-env_factors2$mfish_abundance

lmod<-lm(pred.mean ~ abundance + logzoops + bg + bass + temp_c + water_clarity, data = env_data2)
summary(lmod)
Anova(lmod, type="II")

lmod2<-lm(pred.mean ~ abundance + logzoops + bg + bass + water_clarity, data = env_data2)

##############################################################################################################################
##############################################################################################################################
### Examining escape output

library(lme4)
library(nlme)
library(arm)
library(glmm)

bold.data.scale2<-bold.data.scale
inc6<-which(bold.data.scale$PO4 !="NA")
bold.data.scale2<-bold.data.scale2[inc6,]
?glmm

#esc <- glmm(escape ~ abundance + logzoops + bg + bass + temp_c + water_clarity, random = list(~0 + pop), varcomps.names = c("pop"), 
            #data = bold.data.scale2, family.glmm = binomial.glmm, m = 10^4, debug = TRUE)

#esc_prac<-glmm(escape ~ water_clarity, random = list(~0 + pop), varcomps.names = c("pop"), data = bold.data.scale2, family.glmm = binomial.glmm, m=10^4,doPQL=TRUE)


#summary(esc)
#confint(esc)
#Anova(esc)


########################################
### Run a fit model analysis: dredge on output of glmm, can also use output of cox regression

library(MuMIn)

#esc_fit<-dredge(esc_prac, beta = "none", evaluate = TRUE,rank = "AICc", trace > 1)




###############################################################################################################################
###############################################################################################################################
### Examining lat time of < 600 fish

library(lme4)
library(nlme)
library(arm)
library(glmm)

bold.data6<-bold.data4
inc3<-which(bold.data6$lat_time != 600)
bold.data6<-bold.data6[inc3,]
inc4<-which(bold.data6$abundance != "NA")
bold.data6<-bold.data6[inc4,]
rm(inc3)
rm(inc4)
bold.data6$lat_time<-as.integer(bold.data6$lat_time)

#lat <- glmm(lat_time ~ water_clarity, random = list(~0 + pop), varcomps.names = "pop", data = bold.data6, family.glmm = "poisson.glmm")

#lat <- glm(lat_time ~ abundance + logzoops + bg + bass + temp_c + water_clarity + temp_c, data = bold.data6, family = poisson)

Anova(lat)

plot(lat_time ~ pred.me.cap, data = bold.data6)


lat <- glmm(lat_time ~ water_clarity, random=list(~0 + pop), data=bold.data6, varcomps.names = "pop", family.glmm ="poisson.glmm")




library(MCMCglmm)
library(glmmADMB)
library(lme4)
glm<- glmer(lat_time~water_clarity+(1|pop),data=bold.data6,family=poisson)

