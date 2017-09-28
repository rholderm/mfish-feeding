


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
length.data$length<-length.data$Standard.Length
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


###### lmer for male fish#####


inc3<-which(length.data2$sex == "M")
length.male<-length.data2[inc3,]
length.male$loglength<-log(length.male$length)
rm(inc3)


library(lme4)
library(car)

mod1<-lmer(loglength ~ abundance + logzoops + bg + bass + temp_c + (1|pop), data = length.male)
Anova(mod1, type = "II")

mod2<-lmer(loglength ~ bg + bass + temp_c + (1|pop), data = length.male)
Anova(mod2, type = "II")

mod3<-lmer(loglength ~ bg + bass + (1|pop), data = length.male)
Anova(mod3, type = "II")


library(MuMIn)
# change na. action 
options(na.action = "na.fail")

dredge(mod1, rank = AICc)

my.models<-model.sel(mod1,mod2,mod3,rank=AICc)
my.models
importance(my.models) 


##### lmer for female fish #####

inc4<-which(length.data2$sex == "F")
length.female<-length.data2[inc4,]
length.female$loglength<-log(length.female$length)
rm(inc4)

mod4<-lmer(loglength ~ abundance + logzoops + bg + bass + temp_c + (1|pop), data = length.female)
Anova(mod4, type = "II")

mod5<-lmer(loglength ~ bg + bass + temp_c + (1|pop), data = length.female)
Anova(mod5, type = "II")

mod6<-lmer(loglength ~ bg + bass + (1|pop), data = length.female)
Anova(mod6, type = "II")

dredge(mod4, rank = AICc)

my.models2<-model.sel(mod4,mod5,mod6,rank=AICc)
my.models2
importance(my.models2) 

?MuMIn::model.avg

####################################################################################
### Looking at SC and B ponds seperately - temp acts differently in each


#################### Bishop ################################

##### Male fish #####

inc.b<-which(length.male$location == "B")
length.male.B<-length.male[inc.b,]
rm(inc.b)

mod7<-lmer(length ~ abundance + logzoops + bass + temp_c + (1|pop), data = length.male.B)
Anova(mod7, type = "II")

mod8<-lmer(loglength ~ abundance + logzoops + bass + temp_c + (1|pop), data = length.male.B)
Anova(mod8, type = "II")

dredge(mod7, rank = AICc, extra = "R^2")

##### Female fish #####

inc.bf<-which(length.female$location == "B")
length.female.B<-length.female[inc.bf,]
rm(inc.bf)

mod9<-lmer(length ~ abundance + logzoops + bass + temp_c + (1|pop), data = length.female.B)
Anova(mod8, type = "II")

mod10<-lmer(loglength ~ abundance + logzoops + bass + temp_c + (1|pop), data = length.female.B)
Anova(mod9, type = "II")

dredge(mod9, rank = AICc, extra = "R^2")


#################### Santa Cruz ################################

##### Male fish #####

inc.sc<-which(length.male$location == "SC")
length.male.SC<-length.male[inc.sc,]
rm(inc.sc)


mod11<-lmer(length ~ abundance + logzoops + bass + temp_c + (1|pop), data = length.male.SC)
Anova(mod10, type = "II")

mod12<-lmer(loglength ~ abundance + logzoops + bass + temp_c + (1|pop), data = length.male.SC)
Anova(mod11, type = "II")

mod_pred<-lmer(length ~ abundance + logzoops + bass + bg + (1|pop), data = length.male.SC)
Anova(mod_pred, type = "II")

dredge(mod_pred, rank = AICc)

#library(multcomp)
#summary(glht(mod_pred, mcp(pop="Tukey")))


##### Female fish #####

inc.scf<-which(length.female$location == "SC")
length.female.SC<-length.female[inc.scf,]
rm(inc.scf)

mod13<-lmer(length ~ abundance + logzoops + bass + temp_c + (1|pop), data = length.female.SC)
Anova(mod12, type = "II")

mod14<-lmer(loglength ~ abundance + logzoops + bass + temp_c + (1|pop), data = length.female.SC)
Anova(mod13, type = "II")

mod_pred2<-lmer(length ~ bass + bg + (1|pop), data = length.female.SC)
Anova(mod_pred2)

dredge(mod_pred2, rank = AICc)












