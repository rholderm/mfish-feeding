

 ##### Run models using only SC populations #####

#      #     #     #     #     #     #     #     #


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
### Get only SC pops

bold.data5<-bold.data4
inc3<-which(bold.data5$location =="SC")
bold.data5<-bold.data5[inc3,]



##############################################################################################################################
### Examine cox output

bold.data6<-bold.data5[,-c(1,2,3,4,5,23,24)]
bold.data.scale<-as.data.frame(scale(bold.data6))
bold.data.scale$pop<-bold.data5$pop
bold.data.scale$water_clarity<-bold.data5$water_clarity
bold.data.scale$escape<-bold.data5$escape
bold.data.scale$pair<-bold.data5$pair
inc4<-which(bold.data.scale$PO4 !="NA")
bold.data.scale<-bold.data.scale[inc4,]
rm(inc4)

head(bold.data.scale)
library(lme4)

##################################################
### Using whole data set

mod1<-lmer(pred.me.cap ~ abundance + logzoops + bg + bass + water_clarity + (1|pop), data = bold.data.scale)
summary(mod1)
Anova(mod1, type = "II")

mod2<-lmer(pred.me.cap ~ abundance + logzoops + bg + bass + water_clarity + temp_c + (1|pop), data = bold.data.scale)
summary(mod2)
Anova(mod2, type = "II")

mod3<-lmer(pred.me.cap ~ pair + (1|pop), data = bold.data.scale)
Anova(mod3, type = "II")

library(MuMIn)
# change na. action 
options(na.action = "na.fail")

dredge(mod2, rank = AICc)

###############################################################################################################################
### Escape likelihood

library(lme4)
library(car)

bold.data.scale2<-bold.data.scale
inc5<-which(bold.data.scale$PO4 !="NA")
bold.data.scale2<-bold.data.scale2[inc5,]
rm(inc5)

esc1<-glmer(escape ~ abundance + logzoops + bg + bass + water_clarity + (1|pop), data = bold.data.scale2, family = binomial)
Anova(esc1)

esc2<-glmer(escape ~ abundance + logzoops + bg + bass + water_clarity + temp_c + (1|pop), data = bold.data.scale2, family = binomial)
Anova(esc2)

dredge(esc1, rank = AICc)


###############################################################################################################################
### Examining lat time of < 600 fish


bold.data7<-bold.data5
inc6<-which(bold.data7$lat_time != 600)
bold.data7<-bold.data7[inc6,]
inc7<-which(bold.data7$PO4 != "NA")
bold.data6<-bold.data7[inc7,]
rm(inc6)
rm(inc7)
bold.data8<-bold.data7[,-c(1,2,3,4,5,7,23,24)]
bold.data.scale3<-as.data.frame(scale(bold.data8))
bold.data.scale3$water_clarity<-bold.data7$water_clarity
bold.data.scale3$pop<-bold.data7$pop
bold.data.scale3$lat_time<-bold.data7$lat_time

lat<- glmer(lat_time ~ abundance + logzoops + bg + bass + water_clarity + (1|pop),data=bold.data.scale3,family=poisson)
Anova(lat)

dredge(lat, rank = AICc)










