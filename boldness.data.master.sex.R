


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

#######################################################################################################################
### Cox start

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

###########################################################################################################################
### Create seperate dataframe for male and female fish

male<-which(bold.data4$sex == "M")
female<-which(bold.data4$sex == "F")

bold.male<-bold.data4[male,]
bold.female<-bold.data4[female,]

############################################# lm on cox

### Male

bold.male2<-bold.male
bold.male2<-na.omit(bold.male2)
str(bold.male2)
bold.male.sc<-bold.male2[,-c(1,2,3,4,5,20,21,23,24)]
bold.male.scale<-as.data.frame(scale(bold.male.sc))
bold.male.scale$bass<-bold.male2$bass
bold.male.scale$bg<-bold.male2$bg
bold.male.scale$pop<-bold.male2$pop
bold.male.scale$water_clarity<-bold.male2$water_clarity
bold.male.scale$escape<-bold.male2$escape
bold.male.scale$pair<-bold.male2$pair
bold.male.scale$sex<-bold.male2$sex


library(lme4)

mod.m1<-lmer(pred.me.cap ~ abundance + logzoops + bg + bass + water_clarity + temp_c + (1|pop), data = bold.male.scale)
Anova(mod.m1, type = "II")

mod.m2<-lmer(pred.me.cap ~ abundance + logzoops + bg + bass + water_clarity + (1|pop), data = bold.male.scale)
Anova(mod.m2, type = "II")


### Female


bold.female2<-bold.female
bold.female2<-na.omit(bold.female2)
str(bold.female2)
bold.female.sc<-bold.female2[,-c(1,2,3,4,5,20,21,23,24)]
bold.female.scale<-as.data.frame(scale(bold.female.sc))
bold.female.scale$bass<-bold.female2$bass
bold.female.scale$bg<-bold.female2$bg
bold.female.scale$pop<-bold.female2$pop
bold.female.scale$water_clarity<-bold.female2$water_clarity
bold.female.scale$escape<-bold.female2$escape
bold.female.scale$pair<-bold.female2$pair
bold.female.scale$sex<-bold.female2$sex


mod.f1<-lmer(pred.me.cap ~ abundance + logzoops + bg + bass + water_clarity + temp_c + (1|pop), data = bold.female.scale)
Anova(mod.f1, type = "II")

mod.f2<-lmer(pred.me.cap ~ abundance + logzoops + bg + bass + water_clarity + (1|pop), data = bold.female.scale)
Anova(mod.f2, type = "II")



######################################## Escape likelihood

### Male

esc.m1<-glmer(escape ~ abundance + logzoops + bg + bass + water_clarity + temp_c + (1|pop), data = bold.male.scale, family = binomial)
Anova(esc.m1)

esc.m2<-glmer(escape ~ abundance + logzoops + bg + bass + water_clarity + (1|pop), data = bold.male.scale, family = binomial)
Anova(esc.m2)

tgc.m.esc<-summarySE(bold.male.scale, measurevar="escape", groupvars=c("bass"))
ggplot(tgc.m.esc, aes(x=bass,y=escape))+geom_point() +
  geom_errorbar(aes(ymin=escape-se, ymax=escape+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG") + theme(plot.title = element_text(hjust = 0.5))

### female

esc.f1<-glmer(escape ~ abundance + logzoops + bg + bass + water_clarity + temp_c + (1|pop), data = bold.female.scale, family = binomial)
Anova(esc.m1)

esc.f2<-glmer(escape ~ abundance + logzoops + bg + bass + water_clarity + (1|pop), data = bold.female.scale, family = binomial)
Anova(esc.m2)

tgc.f.esc<-summarySE(bold.female.scale, measurevar="escape", groupvars=c("bass"))
ggplot(tgc.f.esc, aes(x=bass,y=escape))+geom_point() +
  geom_errorbar(aes(ymin=escape-se, ymax=escape+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG") + theme(plot.title = element_text(hjust = 0.5))


##################################### > 600 fish

### Male

inc3<-which(bold.male2$lat_time !=600)
bold.male3<-bold.male2[inc3,]

bold.male3$loglat<-log(bold.male3$lat_time)


mod.m3<-lmer(loglat ~ abundance + logzoops + bg + bass + water_clarity + temp_c + (1|pop), data = bold.male3)
Anova(mod.m3, type = "II")

mod.m4<-lmer(loglat ~ abundance + logzoops + bg + bass + water_clarity + (1|pop), data = bold.male3)
Anova(mod.m4, type = "II")

tgc1<-summarySE(bold.male3, measurevar="loglat", groupvars=c("bass"))
ggplot(tgc1, aes(x=bass,y=loglat))+geom_point() +
  geom_errorbar(aes(ymin=loglat-se, ymax=loglat+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass") + theme(plot.title = element_text(hjust = 0.5))

tgc2<-summarySE(bold.male3, measurevar="loglat", groupvars=c("bg"))
ggplot(tgc2, aes(x=bg,y=loglat))+geom_point() +
  geom_errorbar(aes(ymin=loglat-se, ymax=loglat+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG") + theme(plot.title = element_text(hjust = 0.5))

### Female

inc4<-which(bold.female2$lat_time !=600)
bold.female3<-bold.female2[inc4,]

bold.female3$loglat<-log(bold.female3$lat_time)

mod.f3<-lmer(loglat ~ abundance + logzoops + bg + bass + water_clarity + temp_c + (1|pop), data = bold.female3)
Anova(mod.f3, type = "II")

mod.f4<-lmer(loglat ~ abundance + logzoops + bg + bass + water_clarity + (1|pop), data = bold.female3)
Anova(mod.f4, type = "II")

tgc3<-summarySE(bold.female3, measurevar="loglat", groupvars=c("bass"))
ggplot(tgc3, aes(x=bass,y=loglat))+geom_point() +
  geom_errorbar(aes(ymin=loglat-se, ymax=loglat+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("Bass") + theme(plot.title = element_text(hjust = 0.5))

tgc4<-summarySE(bold.female3, measurevar="loglat", groupvars=c("bg"))
ggplot(tgc4, aes(x=bg,y=loglat))+geom_point() +
  geom_errorbar(aes(ymin=loglat-se, ymax=loglat+se)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle("BG") + theme(plot.title = element_text(hjust = 0.5))

