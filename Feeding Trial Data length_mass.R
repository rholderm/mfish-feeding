feeding<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Feeding Trial Data CompleteR.csv")

### Remove unwanted rows, change name of rows
feed<-feeding[,-c(5,6,7,14,15)]
feed$trial_cue<-feed$Trial.Cue..B.NB.
feed$rearing_cue<-feed$F1.Cue......
feed<-feed[,(-c(3,4))]

str(feed)
attach(feed)

### Add in fish ID to dataframe using paste to create unique labels
feed$ID<-paste(Trial,Population,trial_cue,rearing_cue)
feeding2<-transform(feed,ID2=as.numeric((factor(feed$ID))))
detach(feed)

feeding2$Magnification<-as.numeric(as.character(feeding2$Magnification))
feeding2$Zoop.Length<-as.numeric(as.character(feeding2$Zoop.Length))

### Convert scope length to actual length
feeding2$actual_lengths<-feeding2$Zoop.Length/(feeding2$Magnification*10)



### Create table
tb<-table(feeding2$ID,feeding2$Zooplankton)

### Turn table into a data frame - must use data.frame.matrix
data<-as.data.frame.matrix(tb)
rm(tb)

data$ID<-row.names(data)

### Join data sets. In order to not have replicates, must put unique in front of the large data set
library(plyr)
feed.data<-join(unique(feeding2[,-c(5,6,7,8,12,13)]),data)

### Create data.frame with pred regime and location and join to larger data set

Population<-c("Ant","AW","DeA","DeL","FC","Har","K2","NBLM","NL","Sch","SpH","WSU")
Pred<-c("P","NP","BG","NP","P","BG","NP","NP","P","P","BG","NP")
Location<-c("SC","B","SC","SC","B","SC","B","B","SC","SC","SC","B")

data2<-as.data.frame(Population)
data2$pred<-as.factor(Pred)
data2$location<-as.factor(Location)

rm(Location)
rm(Population)
rm(Pred)

feed.data2<-join(data2,feed.data)

### Create columns with total zoops, copepods, etc.

feed.data2$Unk.Invert<- feed.data2$`Unk. Invert` + feed.data2$`Unk. Invert 1` + feed.data2$`Unk. Invert A` + feed.data2$`Unk. Invert1`
feed.data2$Zoops<- feed.data2$Bosmina + feed.data2$Ceriodaphnia + feed.data2$Chydorid + feed.data2$Cladoceran + feed.data2$Daphnia
feed.data2$Copepods<- feed.data2$Calanoid + feed.data2$Copepod + feed.data2$Cyclopoid
feed.data2$Insects<-feed.data2$Insect + feed.data2$`Insect?` + feed.data2$`Unk. insect` + feed.data2$`Unk. Insect`

feed.data3<-feed.data2[,-c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)]
feed.data3$total_prey<-feed.data2$Unk.Invert+feed.data2$Zoops+feed.data2$Copepods+feed.data2$Insects

str(feed.data3)

######################################## Bring in zooplankton sample lengths #################################################

feeding_zoop<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Feeding Trial zoop data.csv")

### Remove zooplankton that were measured from the 4th sample, only look at the zoops from the first sample
incf<-which(feeding_zoop$Sample.. !="4")
feeding_zoop<-feeding_zoop[incf,]
rm(incf)

fz<-feeding_zoop[,-c(2,3,4,5,7,10,11,12,13,14)]

fz$Length<-as.numeric(fz$Length)

### Remove zooplankton that were not measured
inc<-which(fz$Length !="NA")
fz2a<-fz[inc,]
rm(inc)

###### Convert lengths to actual lengths

fz2a$actual_lengths<-fz2a$Length/(fz2a$Magnification*10)
fz2a$scope_length<-fz2a$Length
fz2a$Length<-fz2a$actual_lengths

######################### Create table with species by trial

na.mean<-function(x){mean(x,na.rm=T)}
mean_mass<-tapply(fz2a$Length, INDEX = list(fz2a$Trial,fz2a$Species), FUN = na.mean)
mean_m<-as.data.frame(mean_mass)
mean_m$trial<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")
#mean_m2<-mean_m[-c(4,8,9,10)]

#x<-merge(fz3,fm, by="Trial", all.x=T, sort=F)

#fm<-feeding2[,-c(6,12)]
#fm$Length<-fm$Zoop.Length

###### Add in values for copepods and cladoceran ######

z<-tapply(fz2a$Length, INDEX = list(fz2a$Trial,fz2a$Species), FUN = na.mean)
z<-as.data.frame(z)
z$cal<-z$Calanoid
z$cyc<-z$Cyclopoid
z$copepod<- rowMeans(z[12:13], na.rm=TRUE)

copepod<-z$copepod
copepod<-as.data.frame(copepod)
copepod$Length<-copepod$copepod
t<-1:24
copepod$Trial<-t
copepod2<-copepod[,-1]
c<-rep("Copepod",24)
copepod2$Species<-c


z$cer<-z$Ceriodaphnia
z$chy<-z$Chydorid
z$dap<-z$Daphnia
z$Cladoceran<-rowMeans(z[15:17],na.rm=TRUE)

Cladoceran<-z$Cladoceran
Cladoceran<-as.data.frame(Cladoceran)
Cladoceran$Length<-Cladoceran$Cladoceran
Cladoceran$Trial<-t
Cladoceran2<-Cladoceran[,-1]
cl<-rep("Cladoceran",24)
Cladoceran2$Species<-cl

fz3<-fz2a[,-c(4,5,6)]
fz2<-rbind(fz3,copepod2,Cladoceran2)
rm(t,c,cl)

###### Get average lenght by seperating out the trials and averaging them individually
na.mean<-function(x){mean(x,na.rm=T)}

inc1<-which(fz2$Trial=="1")
fzT1<-fz2[inc1,]
rm(inc1)

m1<-tapply(fzT1$Length, INDEX = list(fzT1$Species), FUN = na.mean)
mean1<-as.data.frame(m1)
mean1$Trial<-"1"
rm(m1)
mean1$Length<-as.numeric(mean1$m1)
mean1<-mean1[,-1]

inc2<-which(fz2$Trial=="2")
fzT2<-fz2[inc2,]
rm(inc2)

m2<-tapply(fzT2$Length, INDEX = list(fzT2$Species), FUN = na.mean)
mean2<-as.data.frame(m2)
mean2$Trial<-"2"
rm(m2)
mean2$Length<-as.numeric(mean2$m2)
mean2<-mean2[,-1]

inc3<-which(fz2$Trial=="3")
fzT3<-fz2[inc3,]
rm(inc3)

m3<-tapply(fzT3$Length, INDEX = list(fzT3$Species), FUN = na.mean)
mean3<-as.data.frame(m3)
mean3$Trial<-"3"
rm(m3)
mean3$Length<-as.numeric(mean3$m3)
mean3<-mean3[,-1]


inc4<-which(fz2$Trial=="4")
fzT4<-fz2[inc4,]
rm(inc4)

m4<-tapply(fzT4$Length, INDEX = list(fzT4$Species), FUN = na.mean)
mean4<-as.data.frame(m4)
mean4$Trial<-"4"
rm(m4)
mean4$Length<-as.numeric(mean4$m4)
mean4<-mean4[,-1]


inc5<-which(fz2$Trial=="5")
fzT5<-fz2[inc5,]
rm(inc5)

m5<-tapply(fzT5$Length, INDEX = list(fzT5$Species), FUN = na.mean)
mean5<-as.data.frame(m5)
mean5$Trial<-"5"
rm(m5)
mean5$Length<-as.numeric(mean5$m5)
mean5<-mean5[,-1]


inc6<-which(fz2$Trial=="6")
fzT6<-fz2[inc6,]
rm(inc6)

m6<-tapply(fzT6$Length, INDEX = list(fzT6$Species), FUN = na.mean)
mean6<-as.data.frame(m6)
mean6$Trial<-"6"
rm(m6)
mean6$Length<-as.numeric(mean6$m6)
mean6<-mean6[,-1]


inc7<-which(fz2$Trial=="7")
fzT7<-fz2[inc7,]
rm(inc7)

m7<-tapply(fzT7$Length, INDEX = list(fzT7$Species), FUN = na.mean)
mean7<-as.data.frame(m7)
mean7$Trial<-"7"
rm(m7)
mean7$Length<-as.numeric(mean7$m7)
mean7<-mean7[,-1]


inc8<-which(fz2$Trial=="8")
fzT8<-fz2[inc8,]
rm(inc8)

m8<-tapply(fzT8$Length, INDEX = list(fzT8$Species), FUN = na.mean)
mean8<-as.data.frame(m8)
mean8$Trial<-"8"
rm(m8)
mean8$Length<-as.numeric(mean8$m8)
mean8<-mean8[,-1]


inc9<-which(fz2$Trial=="9")
fzT9<-fz2[inc9,]
rm(inc9)

m9<-tapply(fzT9$Length, INDEX = list(fzT9$Species), FUN = na.mean)
mean9<-as.data.frame(m9)
mean9$Trial<-"9"
rm(m9)
mean9$Length<-as.numeric(mean9$m9)
mean9<-mean9[,-1]


inc10<-which(fz2$Trial=="10")
fzT10<-fz2[inc10,]
rm(inc10)

m10<-tapply(fzT10$Length, INDEX = list(fzT10$Species), FUN = na.mean)
mean10<-as.data.frame(m10)
mean10$Trial<-"10"
rm(m10)
mean10$Length<-as.numeric(mean10$m10)
mean10<-mean10[,-1]


inc11<-which(fz2$Trial=="11")
fzT11<-fz2[inc11,]
rm(inc11)

m11<-tapply(fzT11$Length, INDEX = list(fzT11$Species), FUN = na.mean)
mean11<-as.data.frame(m11)
mean11$Trial<-"11"
rm(m11)
mean11$Length<-as.numeric(mean11$m11)
mean11<-mean11[,-1]


inc12<-which(fz2$Trial=="12")
fzT12<-fz2[inc12,]
rm(inc12)

m12<-tapply(fzT12$Length, INDEX = list(fzT12$Species), FUN = na.mean)
mean12<-as.data.frame(m12)
mean12$Trial<-"12"
rm(m12)
mean12$Length<-as.numeric(mean12$m12)
mean12<-mean12[,-1]


inc13<-which(fz2$Trial=="13")
fzT13<-fz2[inc13,]
rm(inc13)

m13<-tapply(fzT13$Length, INDEX = list(fzT13$Species), FUN = na.mean)
mean13<-as.data.frame(m13)
mean13$Trial<-"13"
rm(m13)
mean13$Length<-as.numeric(mean13$m13)
mean13<-mean13[,-1]


inc14<-which(fz2$Trial=="14")
fzT14<-fz2[inc14,]
rm(inc14)

m14<-tapply(fzT14$Length, INDEX = list(fzT14$Species), FUN = na.mean)
mean14<-as.data.frame(m14)
mean14$Trial<-"14"
rm(m14)
mean14$Length<-as.numeric(mean14$m14)
mean14<-mean14[,-1]


inc15<-which(fz2$Trial=="15")
fzT15<-fz2[inc15,]
rm(inc15)

m15<-tapply(fzT15$Length, INDEX = list(fzT15$Species), FUN = na.mean)
mean15<-as.data.frame(m15)
mean15$Trial<-"15"
rm(m15)
mean15$Length<-as.numeric(mean15$m15)
mean15<-mean15[,-1]


inc16<-which(fz2$Trial=="16")
fzT16<-fz2[inc16,]
rm(inc16)

m16<-tapply(fzT16$Length, INDEX = list(fzT16$Species), FUN = na.mean)
mean16<-as.data.frame(m16)
mean16$Trial<-"16"
rm(m16)
mean16$Length<-as.numeric(mean16$m16)
mean16<-mean16[,-1]


inc17<-which(fz2$Trial=="17")
fzT17<-fz2[inc17,]
rm(inc17)

m17<-tapply(fzT17$Length, INDEX = list(fzT17$Species), FUN = na.mean)
mean17<-as.data.frame(m17)
mean17$Trial<-"17"
rm(m17)
mean17$Length<-as.numeric(mean17$m17)
mean17<-mean17[,-1]


inc18<-which(fz2$Trial=="18")
fzT18<-fz2[inc18,]
rm(inc18)

m18<-tapply(fzT18$Length, INDEX = list(fzT18$Species), FUN = na.mean)
mean18<-as.data.frame(m18)
mean18$Trial<-"18"
rm(m18)
mean18$Length<-as.numeric(mean18$m18)
mean18<-mean18[,-1]


inc19<-which(fz2$Trial=="19")
fzT19<-fz2[inc19,]
rm(inc19)

m19<-tapply(fzT19$Length, INDEX = list(fzT19$Species), FUN = na.mean)
mean19<-as.data.frame(m19)
mean19$Trial<-"19"
rm(m19)
mean19$Length<-as.numeric(mean19$m19)
mean19<-mean19[,-1]


inc20<-which(fz2$Trial=="20")
fzT20<-fz2[inc20,]
rm(inc20)

m20<-tapply(fzT20$Length, INDEX = list(fzT20$Species), FUN = na.mean)
mean20<-as.data.frame(m20)
mean20$Trial<-"20"
rm(m20)
mean20$Length<-as.numeric(mean20$m20)
mean20<-mean20[,-1]


inc21<-which(fz2$Trial=="21")
fzT21<-fz2[inc21,]
rm(inc21)

m21<-tapply(fzT21$Length, INDEX = list(fzT21$Species), FUN = na.mean)
mean21<-as.data.frame(m21)
mean21$Trial<-"21"
rm(m21)
mean21$Length<-as.numeric(mean21$m21)
mean21<-mean21[,-1]


inc22<-which(fz2$Trial=="22")
fzT22<-fz2[inc22,]
rm(inc22)

m22<-tapply(fzT22$Length, INDEX = list(fzT22$Species), FUN = na.mean)
mean22<-as.data.frame(m22)
mean22$Trial<-"22"
rm(m22)
mean22$Length<-as.numeric(mean22$m22)
mean22<-mean22[,-1]


inc23<-which(fz2$Trial=="23")
fzT23<-fz2[inc23,]
rm(inc23)

m23<-tapply(fzT23$Length, INDEX = list(fzT23$Species), FUN = na.mean)
mean23<-as.data.frame(m23)
mean23$Trial<-"23"
rm(m23)
mean23$Length<-as.numeric(mean23$m23)
mean23<-mean23[,-1]


inc24<-which(fz2$Trial=="24")
fzT24<-fz2[inc24,]
rm(inc24)

m24<-tapply(fzT24$Length, INDEX = list(fzT24$Species), FUN = na.mean)
mean24<-as.data.frame(m24)
mean24$Trial<-"24"
rm(m24)
mean24$Length<-as.numeric(mean24$m24)
mean24<-mean24[,-1]

### Create data set with all trial zooplankton mean lengths

means<-rbind(mean1,mean2,mean3,mean4,mean5,mean6,mean7,mean8,mean9,mean10,mean11,mean12,mean13,mean14,mean15,mean16,mean17,
             mean18,mean19,mean20,mean21,mean22,mean23,mean24)
rm(mean1,mean2,mean3,mean4,mean5,mean6,mean7,mean8,mean9,mean10,mean11,mean12,mean13,mean14,mean15,mean16,mean17,
   mean18,mean19,mean20,mean21,mean22,mean23,mean24)
rm(fzT1,fzT2,fzT3,fzT4,fzT5,fzT6,fzT7,fzT8,fzT9,fzT10,fzT11,fzT12,fzT13,fzT14,fzT15,fzT16,fzT17,fzT18,fzT19,fzT20,fzT21,fzT22,fzT23,fzT24)

### Add in species column to zooplankton mean length dataset

species<-list("Bosmina","Calanoid","Ceriodaphnia","Chironamid","Chydorid","Cyclopoid","Daphnia","Immature_Copepod",
              "Insect","Macro_Invert","Naplius","Copepod","Cladoceran")

sp2<-rep(species,24)

means$Zooplankton<-sp2
rownames(means) <- c()
rm(species,sp2)


### Add in actual lengths to gut samples with missing values
feed2<-feed
feed2$Mass<-as.character(feed2$Mass)
feed2$Mass[(feed2$Mass=="Unk.")]<-"NA"
feed2$Mass[(feed2$Mass=="")]<-"NA"
feed2$Mass<-as.numeric(feed2$Mass)

feed2$actual_lengths<-feed2$Zoop.Length/(feed2$Magnification*10)
feed2$scope_length<-feed2$Zoop.Length
feed2$Zoop.Length<-feed2$actual_lengths

x<-join(feed2,means)
str(x)



x$Zoop.Length[is.na(x$Zoop.Length)] <- "A"

x$Zoop.Length <- ifelse(test = x$Zoop.Length != "A", yes = x$Zoop.Length, no = x$Length)
x$Zoop.Length<-as.numeric(x$Zoop.Length)

#### Check missing values
y<-x
y$Zoop.Length[is.na(y$Zoop.Length)] <- "A"

ch<-which(y$Zoop.Length=="A")
ch2<-y[ch,]
rm(ch)


#### Remove unknowns, add in missing values
str(x)
x$Zooplankton<-as.character(x$Zooplankton)
x$Zooplankton[(x$Zooplankton=="Insect?")]<-"Insect"
x$Zooplankton[(x$Zooplankton=="Unk. Insect")]<-"Insect"
x$Zooplankton[(x$Zooplankton=="Unk. insect")]<-"Insect"
x$Zooplankton[(x$Zooplankton=="Unk. Bug")]<-"Insect"
x$Zooplankton[(x$Zooplankton=="Unk. Invert A")]<-"Invert"
x$Zooplankton[(x$Zooplankton=="Unk. Invert1")]<-"Invert"
x$Zooplankton[(x$Zooplankton=="Unk. Invert 1")]<-"Invert"
x$Zooplankton[(x$Zooplankton=="Unk. Invert")]<-"Invert"
x$Zooplankton[(x$Zooplankton=="Unk")]<-"Unk."
x$Zooplankton<-as.factor(x$Zooplankton)


unk<-which(x$Zooplankton !="Unk.")
x2<-x[unk,]
rm(unk)


###################################### Only crustacean zooplankton in the data frame #####################################

incz<-which(x2$Zooplankton != "Chironamid")
xzoops<-x2[incz,]
incz2<-which(xzoops$Zooplankton != "Insect")
xzoops2<-xzoops[incz2,]
incz3<-which(xzoops2$Zooplankton != "Invert")
xzoops3<-xzoops2[incz3,]
rm(incz,incz2,incz3)


#### Check for missing values, should only have fish that ate nothing left here

y2<-xzoops3
y2$Zoop.Length[is.na(y2$Zoop.Length)] <- "A"

ch2<-which(y2$Zoop.Length=="A")
ch3<-y2[ch2,]
rm(ch2)



#############################################################################################################################
#############################################################################################################################
                                          ### Start of length mass regression ###
#############################################################################################################################
#############################################################################################################################

z2<-xzoops3[-c(6,8,12,13,14)]


incb<-which(z2$Zooplankton=="Bosmina")
bos<-z2[incb,]
rm(incb)
bos$gut_m<-exp(4.9344+4.849*log(bos$Zoop.Length))

incca<-which(z2$Zooplankton=="Calanoid")
cal<-z2[incca,]
rm(incca)
cal$gut_m<-exp(.9227+2.4235*log(cal$Zoop.Length))

inccer<-which(z2$Zooplankton=="Ceriodaphnia")
cer<-z2[inccer,]
rm(inccer)
cer$gut_m<-exp(1.915+2.02*log(cer$Zoop.Length))

incchy<-which(z2$Zooplankton=="Chydorid")
chy<-z2[incchy,]
rm(incchy)
chy$gut_m<-exp(4.49+3.93*log(chy$Zoop.Length))

inccyc<-which(z2$Zooplankton=="Cyclopoid")
cyc<-z2[inccyc,]
rm(inccyc)
cyc$gut_m<-exp(1.3135+2.9005*log(cyc$Zoop.Length))

incdap<-which(z2$Zooplankton=="Daphnia")
dap<-z2[incdap,]
rm(incdap)
dap$gut_m<-exp((log(1.5*10^-8))+2.84*log((dap$Zoop.Length)*1000))

inccla<-which(z2$Zooplankton=="Cladoceran")
cla<-z2[inccla,]
rm(inccla)
cla$gut_m<-exp((log(1.5*10^-8))+2.84*log((cla$Zoop.Length)*1000))

inccop<-which(z2$Zooplankton=="Copepod")
cop<-z2[inccop,]
rm(inccop)
cop$gut_m<-exp(.9227+2.4235*log(cop$Zoop.Length))

incnp<-which(z2$Zooplankton=="")
np<-z2[incnp,]
rm(incnp)
np$gut_m<-0

incnp2<-which(z2$Zooplankton==" ")
np2<-z2[incnp2,]
rm(incnp2)
np2$gut_m<-0

levels(z2$Zooplankton)
z3<-rbind(bos,cal,cer,chy,cyc,dap,np,np2,cop,cla)


#############################################################################################################################
#############################################################################################################################
                                          ### Get gut mass for each fish ### 
#############################################################################################################################
#############################################################################################################################
str(aggdata2)
agg<-z3[,-c(4,5,6)]

aggdata <-aggregate(z3$gut_m, by=list(z3$ID),FUN=mean, na.rm=TRUE)
aggdata$gut_m<-aggdata$x
aggdata$ID<-aggdata$Group.1
aggdata2<-aggdata[,-c(1,2)]

ID<-read.table(text = aggdata2$ID, sep = " ", colClasses = "character")
ID$ID<-aggdata2$ID

aggdata3<-join(ID,aggdata2)

aggdata3$Trial<-aggdata3$V1
aggdata3$Population<-aggdata3$V2
aggdata3$trial_cue<-aggdata3$V3
aggdata3$rearing_cue<-aggdata3$V4

aggdata4<-aggdata3[,-c(1,2,3,4)]




