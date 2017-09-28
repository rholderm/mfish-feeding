
env_factors<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Environmental pond data for PCA_R.csv")


head(env_factors)
attach(env_factors)


##### Delete rows out of data frame
env2<-env_factors[-c(4,5,25),]
env2
attach(env2)

#### Delete columns out of data frame
pcdata<-env2[,-c(1,2,13)]
head(env2)
head(pcdata)
attach(pcdata)

env2$population

########################################################################################################################

####Environmental graphs

env3<-env2
population<-env2$population
env3$logzoops<-log(pcdata$total_zoops_plus_1)
env3$logcladoceran<-log(pcdata$total_cladocerans_plus_1)
env3$population<-env2$population
env3$pred_regime<-env2$pred_regime
env3$population<-factor(env3$population, levels=c("Ant","NL","Schwann","CaK","Corcoran","DeL","Dod","Larsen","Sho","WatL","DeA","Harkin","SpH","CCO","CCA_A","Yolo","AW","FS","K2","K5","NBLM","LHC","WSU","LAW","FC"))
attach(env3)
head(env3)

### Create Table with average for each predator regime

env_tb<-env3[,-c(4,6,8,18,19)]
env_tb<-env_tb[order(pred_regime),]
str(env_tb)

env_bass<-env_tb[c(1,2,3,4,5),]
env_BG<-env_tb[c(6,7,8),]
env_NP<-env_tb[-c(1,2,3,4,5,6,7,8),]


bass<-env_bass[,-c(1,2,10)]
b<-colMeans(bass)
bass<-rbind(bass,b)
bass$population<-c("Ant","FC","LAW","NL","Schwann","Bass_Avg")
bass$pred_regime<-c("Bass","Bass","Bass","Bass","Bass","Bass")

BG<-env_BG[,-c(1,2,10)]
g<-colMeans(BG)
BG<-rbind(BG,g)
BG$population<-c("DeA","Harkin","SpH","BG_Avg")
BG$pred_regime<-c("BG","BG","BG","BG")

NP<-env_NP[,-c(1,2,10)]
n<-colMeans(NP)
NP<-rbind(NP,n)
NP$population<-c("AW","CaK","Corcoran","DeL","Dod","FS","K2","K5","Larsen","LHC","NBLM","Sho","WatL","WSU","NP_Avg")
NP$pred_regime<-c("NP","NP","NP","NP","NP","NP","NP","NP","NP","NP","NP","NP","NP","NP","NP")

tb<-rbind(bass,BG,NP)


###Zoop abundance
ggplot(env3, aes(x=population,y=logzoops))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
theme(legend.position = c(.9,.8))+ggtitle("Zooplankton Abundance") + theme(plot.title = element_text(hjust = 0.5))

###Cladoceran abundance
ggplot(env3, aes(x=population,y=logcladoceran))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("Cladoceran Abundance") + theme(plot.title = element_text(hjust = 0.5))

###mfish abundance
ggplot(env3, aes(x=population,y=mfish_abundance))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.7,.8))+ggtitle("Mosquitofish Abundance") + theme(plot.title = element_text(hjust = 0.5))

###chlA
ggplot(env3, aes(x=population,y=chlA))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("ChlA") + theme(plot.title = element_text(hjust = 0.5))

###PO4
ggplot(env3, aes(x=population,y=PO4))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("PO4") + theme(plot.title = element_text(hjust = 0.5))

###NH4
ggplot(env3, aes(x=population,y=NH4))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("NH4") + theme(plot.title = element_text(hjust = 0.5))

###pH
ggplot(env3, aes(x=population,y=pH))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("pH") + theme(plot.title = element_text(hjust = 0.5))

###temp
ggplot(env3, aes(x=population,y=temp_c))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.1,.8))+ggtitle("Temperature (C)") + theme(plot.title = element_text(hjust = 0.5))

###s. cond
ggplot(env3, aes(x=population,y=s_cond))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("S. Cond") + theme(plot.title = element_text(hjust = 0.5))

###DO
ggplot(env3, aes(x=population,y=DO))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("DO") + theme(plot.title = element_text(hjust = 0.5))

###Do percent
ggplot(env3, aes(x=population,y=DO_percent))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.7,.8))+ggtitle("DO Percent") + theme(plot.title = element_text(hjust = 0.5))


############################################################################################################################

#### Boldness and environmental graphs

boldness_data<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Field Sample ZoopsR.csv")
head(boldness_data)
bold_rm<-boldness_data[-c(4,5,25),]
env3$mean_lat<-bold_rm$mean.latency.time
env3$F_M_mean_lat<-bold_rm$F.M.Mean.latency.time
head(env3)


###Zoop abundance
ggplot(env3, aes(x=logzoops,y=mean_lat))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("Zooplankton Abundance") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3)

###Cladoceran abundance
ggplot(env3, aes(x=logcladoceran,y=mean_lat))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("Cladoceran Abundance") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3)

###mfish abundance
ggplot(env3, aes(x=mfish_abundance,y=mean_lat))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.7,.8))+ggtitle("Mosquitofish Abundance") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3)

###chlA
ggplot(env3, aes(x=chlA,y=mean_lat))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("ChlA") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3)

###PO4
ggplot(env3, aes(x=PO4,y=mean_lat))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("PO4") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3)

###NH4
ggplot(env3, aes(x=NH4,y=mean_lat))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("NH4") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3)

###pH
ggplot(env3, aes(x=pH,y=mean_lat))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("pH") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3)

###temp
ggplot(env3, aes(x=temp_c,y=mean_lat))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.1,.8))+ggtitle("Temperature (C)") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3)

###s. cond
ggplot(env3, aes(x=s_cond,y=mean_lat))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("S. Cond") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3)

###DO
ggplot(env3, aes(x=DO,y=mean_lat))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.9,.8))+ggtitle("DO") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3)

###Do percent
ggplot(env3, aes(x=DO_percent,y=mean_lat))+geom_point(aes(colour = location,shape = pred_regime), size=4) +
  theme(legend.position = c(.7,.8))+ggtitle("DO Percent") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3)






########################################################################################################################

###### PCA analysis
#####
#### Start of PCA: In order to make the data frame into a matrix I must get rid of all characters, this assigns the population names as the row names so I can delete the column population
row.names(pcdata)

row.names(pcdata)<-env2$population
pcdata

pcdata.rm<-pcdata[-23,-2]
str(pcdata.rm)
#### Check if data is correlated and remove correlated columns
cor(pcdata.rm)
View(cor(pcdata.rm))
head(pcdata.rm)
pcdata.rm<-pcdata.rm[,-c(3,5,13)]
pcdata<-pcdata.rm
pcdata
row.names(pcdata.rm)<-env2$population
env2$population

####normalize data for PCA
pcdata
pcdata2.scale<-scale(pcdata)

head(pcdata2.scale)
summary(pcdata2.scale)
pcdata2.scale


#### perform pca analysis!
pca<-prcomp(pcdata2.scale, center = TRUE,scale. = TRUE)

#### creates summary of the PCA
pca.sum<-summary(pca)

plot(pca)

PCA.loadings<-as.data.frame(pca.sum$rotation)
pred.pca<-as.data.frame(predict(pca))
head(pred.pca)

####################################################################################################################
### PCA basic plot


####Pch=16 makes dots filled in, pch controls the shape of the dots

plot(pred.pca$PC1,pred.pca$PC2,col=env2$location,pch=16)
text(pred.pca$PC1,pred.pca$PC2, rownames(pcdata.rm), pos=4,col="darkgreen")

arrows(rep(0,length(colnames(pred.pca))),rep(0,length(colnames(pred.pca))), 5*(PCA.loadings$PC1), 5*(PCA.loadings$PC2), col="black", length = 0.1, lwd=2)

#### Add txt to graph, need to repeat this 10 times
text((5*PCA.loadings$PC1[1]),5*(PCA.loadings$PC2[1])-.3,labels="abundance", cex=1, col="black",font=2)
text(5*PCA.loadings$PC1[2]+.6, 5*(PCA.loadings$PC2[2])+.1,labels="cladoceran", cex=1, col="black",font=2)
text(5*PCA.loadings$PC1[3]-.1, 5*(PCA.loadings$PC2[3])-.3,labels="zoops", cex=1, col="black",font=2)
text(5*PCA.loadings$PC1[4]+.1, 5*(PCA.loadings$PC2[4])-.1,labels="chlA", cex=1, col="black",font=2)
text(5*PCA.loadings$PC1[5]+.2, 5*(PCA.loadings$PC2[5]),labels="PO4", cex=1, col="black",font=2)
text(5*PCA.loadings$PC1[6]+.2, 5*(PCA.loadings$PC2[6]),labels="NH4", cex=1, col="black",font=2)
text(5*PCA.loadings$PC1[7]+.1, 5*(PCA.loadings$PC2[7])-.1,labels="pH", cex=1, col="black",font=2)
text(5*PCA.loadings$PC1[8], 5*(PCA.loadings$PC2[8])-.1,labels="temp", cex=1, col="black",font=2)
text(5*PCA.loadings$PC1[9], 5*(PCA.loadings$PC2[9])-.2,labels="s. cond", cex=1, col="black",font=2)
text(5*PCA.loadings$PC1[10]-.1, 5*(PCA.loadings$PC2[10])-.1,labels="DO", cex=1, col="black",font=2)

PCA.loadings

#####Add legend
legend("topleft",legend = c("Bishop","Santa Cruz"), pch=16, col = c("black", "limegreen"))





####################################################################################################################
###### PCA ggplot
#####
#### ggplot,looking at env data vs location,population,pred regime

attach(env2)
library("ggplot2", lib.loc="~/R/win-library/3.3")
env2$pop

pcaplot<-ggplot(pred.pca, aes(PC1,PC2)) + geom_point(aes(colour = location,shape = pred_regime), size=3) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3) +
  annotate("segment", x=0, y=0, xend= (5*PCA.loadings$PC1), yend= (5*PCA.loadings$PC2), arrow=arrow (length=unit(0.3, "cm")), size=1) +
  annotate("text", x=PCA.loadings$PC1[1], y=PCA.loadings$PC2[1]-2.2, label="abundance", fontface=2) +
  annotate("text", x=PCA.loadings$PC1[2]+1.8, y=PCA.loadings$PC2[2]+.6, label="cladoceran", fontface=2) +
  annotate("text", x=PCA.loadings$PC1[3]+1.1, y=PCA.loadings$PC2[3]+.3, label="zoops", fontface=2) +
  annotate("text", x=PCA.loadings$PC1[4]+2.0, y=PCA.loadings$PC2[4]-.5, label="chlA", fontface=2) +
  annotate("text", x=PCA.loadings$PC1[5]+2.0, y=PCA.loadings$PC2[5]+1.0, label="PO4", fontface=2) +
  annotate("text", x=PCA.loadings$PC1[6]+1.6, y=PCA.loadings$PC2[6]+1.5, label="NH4", fontface=2) +
  annotate("text", x=PCA.loadings$PC1[7]+1.3, y=PCA.loadings$PC2[7]-1.9, label="pH", fontface=2) +
  annotate("text", x=PCA.loadings$PC1[8]-0.3, y=PCA.loadings$PC2[8]-1.4, label="temp", fontface=2) +
  annotate("text", x=PCA.loadings$PC1[9]+1.5, y=PCA.loadings$PC2[9]-0.7, label="s.cond", fontface=2) +
  annotate("text", x=PCA.loadings$PC1[10]+1.0, y=PCA.loadings$PC2[10]-1.9, label="DO", fontface=2) +
  ggtitle("Environmental Data") + theme(plot.title = element_text(hjust = 0.5))

pcaplot



#Sets point color, shape, and size. If the info is within the aes then it creates a legend for it
# geom_point(aes(colour = location,shape = pred_regime), size=3)


#add labels to ponts and change the position of the labels
#geom_text(aes(label=row.names(pcdata.rm)), size=4, hjust=1.4,vjust=.3)


################################################################################################################
#######
######
#####
#### Basic PCA and ggplot PCA with boldness data

env4<-env3[-c(3,4,12,13,14),]
env4$population
attach(env4)
row.names(env4)<-env4$population

##### Boldness PCA normal plot
plot(pred.pca.bold$PC1,pred.pca.bold$PC2,col=env4$location,pch=16) +
arrows(rep(0,length(colnames(pred.pca.bold))),rep(0,length(colnames(pred.pca.bold))), 5*(PCA.bold.loadings$PC1), 5*(PCA.bold.loadings$PC2), col="black", length = 0.1, lwd=2) +
text(pred.pca.bold$PC1,pred.pca.bold$PC2, rownames(env4), pos=3,col="darkgreen") +
text(5*PCA.bold.loadings$PC1[1]+.02, 5*(PCA.bold.loadings$PC2[1]+.04),labels="abundance", cex=1, col="black",font=2) +
text(5*PCA.bold.loadings$PC1[2], 5*(PCA.bold.loadings$PC2[2]+.03),labels="cladoceran", cex=1, col="black",font=2) +
text(5*PCA.bold.loadings$PC1[3]-.1, 5*(PCA.bold.loadings$PC2[3]),labels="zoops", cex=1, col="black",font=2) +
text(5*PCA.bold.loadings$PC1[4], 5*(PCA.bold.loadings$PC2[4])-.09,labels="chlA", cex=1, col="black",font=2) +
text(5*PCA.bold.loadings$PC1[5], 5*(PCA.bold.loadings$PC2[5]+.03),labels="PO4", cex=1, col="black",font=2) +
text(5*PCA.bold.loadings$PC1[6], 5*(PCA.bold.loadings$PC2[6]-.03),labels="NH4", cex=1, col="black",font=2) +
text(5*PCA.bold.loadings$PC1[7], 5*(PCA.bold.loadings$PC2[7])+.12,labels="pH", cex=1, col="black",font=2) +
text(5*PCA.bold.loadings$PC1[8]+.1, 5*(PCA.bold.loadings$PC2[8])+.1,labels="temp", cex=1, col="black",font=2) +
text(5*PCA.bold.loadings$PC1[9], 5*(PCA.bold.loadings$PC2[9]-.03),labels="s. cond", cex=1, col="black",font=2) +
text(5*PCA.bold.loadings$PC1[10], 5*(PCA.bold.loadings$PC2[10]+.03),labels="DO", cex=1, col="black",font=2) +
text(5*PCA.bold.loadings$PC1[11], 5*(PCA.bold.loadings$PC2[11]-.02),labels="mean lat", cex=1, col="black",font=2)



######### Boldness PCA ggplot2
pcdata.bold<-pcdata
pcdata.bold$mean_lat<-env3$mean_lat
pcdata.bold<-pcdata.bold[-c(3,4,12,13,14),]
pcdata.bold.scale<-scale(pcdata.bold)
pcdata.bold.scale
summary(pcdata.bold.scale)

pca.bold<-prcomp(pcdata.bold.scale, center = TRUE,scale. = TRUE)
pca.sum.bold<-summary(pca.bold)

PCA.bold.loadings<-as.data.frame(pca.sum.bold$rotation)
pred.pca.bold<-as.data.frame(predict(pca.bold))

PCA.bold.loadings
pca.sum.bold



pcaplot.bold<-ggplot(pred.pca.bold, aes(PC1,PC2)) + geom_point(aes(colour = location,shape = pred_regime), size=3) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(aes(label=env4$population), size=4, hjust=1.4,vjust=.3) +
  annotate("segment", x=0, y=0, xend= (5*PCA.bold.loadings$PC1), yend= (5*PCA.bold.loadings$PC2), arrow=arrow (length=unit(0.3, "cm")), size=1) +
  annotate("text", x=PCA.bold.loadings$PC1[1]+1.1, y=PCA.bold.loadings$PC2[1]+0.2, label="abundance", fontface=2) +
  annotate("text", x=PCA.bold.loadings$PC1[2]-1.3, y=PCA.bold.loadings$PC2[2]+2.2, label="cladoceran", fontface=2) +
  annotate("text", x=PCA.bold.loadings$PC1[3]-1.2, y=PCA.bold.loadings$PC2[3]+1.5, label="zoops", fontface=2) +
  annotate("text", x=PCA.bold.loadings$PC1[4]-1.8, y=PCA.bold.loadings$PC2[4]-0.3, label="chlA", fontface=2) +
  annotate("text", x=PCA.bold.loadings$PC1[5]-2.0, y=PCA.bold.loadings$PC2[5]+0.1, label="PO4", fontface=2) +
  annotate("text", x=PCA.bold.loadings$PC1[6]-1.7, y=PCA.bold.loadings$PC2[6]-0.6, label="NH4", fontface=2) +
  annotate("text", x=PCA.bold.loadings$PC1[7]-0.7, y=PCA.bold.loadings$PC2[7]+0.1, label="pH", fontface=2) +
  annotate("text", x=PCA.bold.loadings$PC1[8]+0.6, y=PCA.bold.loadings$PC2[8]+0.9, label="temp", fontface=2) +
  annotate("text", x=PCA.bold.loadings$PC1[9]-1.2, y=PCA.bold.loadings$PC2[9]-2.0, label="s.cond", fontface=2) +
  annotate("text", x=PCA.bold.loadings$PC1[10]-0.05, y=PCA.bold.loadings$PC2[10]+2.3, label="DO", fontface=2) +
  annotate("text", x=PCA.bold.loadings$PC1[11]+0.2, y=PCA.bold.loadings$PC2[11]-0.5, label="mean lat", fontface=2) +
  ggtitle("Environmental Data") + theme(plot.title = element_text(hjust = 0.5))

pcaplot.bold

ggplot(pred.pca.bold, aes(PC1,PC2)) + geom_point(aes(colour = location, shape=pred_regime), size=3)

env4$population
env4



###################################################################################################################


############# PCA with BG and Bass desnities added in 
env4<-env2
env4$bg_density<- c("0","0","0","0","1.25","0","0","0","0","16.25","0","0","0","0","0","0","1.67","0.5","0","69.5","0","0")
env4$bass_density<- c("2","0","0","0","0","0","0","5.5","0","0","0","0","0","0","0","0","0.83","5.5","0","0","0","0")
attach(env4)
pcdata4<-env4
pcdata4<-pcdata4[,-c(1,2,4,6,8,13,17)]
pcdata4$mfish_abundance<-as.numeric(pcdata4$mfish_abundance)
pcdata4$total_zoops<-as.numeric(pcdata4$total_zoops)
pcdata4$bg_density<-as.numeric(pcdata4$bg_density)
pcdata4$bass_density<-as.numeric(pcdata4$bass_density)
row.names(pcdata4)<-env4$population



pcdata4.scale<-scale(pcdata4)
pca4<-prcomp(pcdata4.scale, center = TRUE,scale. = TRUE)
pca.sum4<-summary(pca4)
PCA.loadings4<-as.data.frame(pca.sum4$rotation)
pred.pca4<-as.data.frame(predict(pca4))


pcaplot4<-ggplot(pred.pca4, aes(PC1,PC2)) + geom_point(aes(colour = location,shape = pred_regime), size=3) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(aes(label=row.names(pcdata4)), size=4, hjust=1.4,vjust=.3) +
  annotate("segment", x=0, y=0, xend= (5*PCA.loadings4$PC1), yend= (5*PCA.loadings4$PC2), arrow=arrow (length=unit(0.3, "cm")), size=1) +
  annotate("text", x=5*PCA.loadings4$PC1[1], y=5*PCA.loadings4$PC2[1], label="abundance", fontface=2) +
  annotate("text", x=5*PCA.loadings4$PC1[2], y=5*PCA.loadings4$PC2[2], label="cladoceran", fontface=2) +
  annotate("text", x=5*PCA.loadings4$PC1[3], y=5*PCA.loadings4$PC2[3], label="zoops", fontface=2) +
  annotate("text", x=5*PCA.loadings4$PC1[4], y=5*PCA.loadings4$PC2[4], label="chlA", fontface=2) +
  annotate("text", x=5*PCA.loadings4$PC1[5], y=5*PCA.loadings4$PC2[5], label="PO4", fontface=2) +
  annotate("text", x=5*PCA.loadings4$PC1[6], y=5*PCA.loadings4$PC2[6], label="NH4", fontface=2) +
  annotate("text", x=5*PCA.loadings4$PC1[7], y=5*PCA.loadings4$PC2[7], label="pH", fontface=2) +
  annotate("text", x=5*PCA.loadings4$PC1[8], y=5*PCA.loadings4$PC2[8], label="temp", fontface=2) +
  annotate("text", x=5*PCA.loadings4$PC1[9], y=5*PCA.loadings4$PC2[9], label="s.cond", fontface=2) +
  annotate("text", x=5*PCA.loadings4$PC1[10], y=5*PCA.loadings4$PC2[10], label="DO", fontface=2) +
  annotate("text", x=5*PCA.loadings4$PC1[11], y=5*PCA.loadings4$PC2[11], label="BG", fontface=2) +
  annotate("text", x=5*PCA.loadings4$PC1[12], y=5*PCA.loadings4$PC2[12], label="Bass", fontface=2) +
  ggtitle("Environmental Data") + theme(plot.title = element_text(hjust = 0.5))

pcaplot4



#################################################################################################################
###### PCA plot minus Bishop locations (with pred info)

env5<-env4[-c(2,8,9,11,12,14,15,16,22),]
pcdata5<-pcdata4[-c(2,8,9,11,12,14,15,16,22),]

pcdata5.scale<-scale(pcdata5)
pca5<-prcomp(pcdata5.scale, center = TRUE,scale. = TRUE)
pca.sum5<-summary(pca5)
PCA.loadings5<-as.data.frame(pca.sum5$rotation)
pred.pca5<-as.data.frame(predict(pca5))
attach(env5)

pcaplot5<-ggplot(pred.pca5, aes(PC1,PC2)) + geom_point(aes(colour = pred_regime), size=3) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(aes(label=row.names(pcdata5)), size=4, hjust=1.4,vjust=.3) +
  annotate("segment", x=0, y=0, xend= (5*PCA.loadings5$PC1), yend= (5*PCA.loadings5$PC2), arrow=arrow (length=unit(0.3, "cm")), size=1) +
  annotate("text", x=5*PCA.loadings5$PC1[1], y=5*PCA.loadings5$PC2[1], label="abundance", fontface=2) +
  annotate("text", x=5*PCA.loadings5$PC1[2], y=5*PCA.loadings5$PC2[2], label="cladoceran", fontface=2) +
  annotate("text", x=5*PCA.loadings5$PC1[3], y=5*PCA.loadings5$PC2[3], label="zoops", fontface=2) +
  annotate("text", x=5*PCA.loadings5$PC1[4], y=5*PCA.loadings5$PC2[4], label="chlA", fontface=2) +
  annotate("text", x=5*PCA.loadings5$PC1[5], y=5*PCA.loadings5$PC2[5], label="PO4", fontface=2) +
  annotate("text", x=5*PCA.loadings5$PC1[6], y=5*PCA.loadings5$PC2[6], label="NH4", fontface=2) +
  annotate("text", x=5*PCA.loadings5$PC1[7], y=5*PCA.loadings5$PC2[7], label="pH", fontface=2) +
  annotate("text", x=5*PCA.loadings5$PC1[8], y=5*PCA.loadings5$PC2[8], label="temp", fontface=2) +
  annotate("text", x=5*PCA.loadings5$PC1[9], y=5*PCA.loadings5$PC2[9], label="s.cond", fontface=2) +
  annotate("text", x=5*PCA.loadings5$PC1[10], y=5*PCA.loadings5$PC2[10], label="DO", fontface=2) +
  annotate("text", x=5*PCA.loadings5$PC1[11], y=5*PCA.loadings5$PC2[11], label="BG", fontface=2) +
  annotate("text", x=5*PCA.loadings5$PC1[12], y=5*PCA.loadings5$PC2[12], label="Bass", fontface=2) +
  ggtitle("Environmental Data") + theme(plot.title = element_text(hjust = 0.5))

pcaplot5


##################################################################################################################
#### PCA plot minus bishop without pred info

env6<-env2[-c(2,8,9,11,12,14,15,16,22),]
pcdata6<-pcdata.rm[-c(2,8,9,11,12,14,15,16,22),]


pcdata6.scale<-scale(pcdata6)
pca6<-prcomp(pcdata6.scale, center = TRUE,scale. = TRUE)
pca.sum6<-summary(pca6)
PCA.loadings6<-as.data.frame(pca.sum6$rotation)
pred.pca6<-as.data.frame(predict(pca6))
attach(env6)



pcaplot6<-ggplot(pred.pca6, aes(PC1,PC2)) + geom_point(aes(colour = pred_regime), size=3) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(aes(label=row.names(pcdata6)), size=4, hjust=1.4,vjust=.3) +
  annotate("segment", x=0, y=0, xend= (5*PCA.loadings6$PC1), yend= (5*PCA.loadings6$PC2), arrow=arrow (length=unit(0.3, "cm")), size=1) +
  annotate("text", x=5*PCA.loadings6$PC1[1], y=5*PCA.loadings6$PC2[1], label="abundance", fontface=2) +
  annotate("text", x=5*PCA.loadings6$PC1[2], y=5*PCA.loadings6$PC2[2], label="cladoceran", fontface=2) +
  annotate("text", x=5*PCA.loadings6$PC1[3], y=5*PCA.loadings6$PC2[3], label="zoops", fontface=2) +
  annotate("text", x=5*PCA.loadings6$PC1[4], y=5*PCA.loadings6$PC2[4], label="chlA", fontface=2) +
  annotate("text", x=5*PCA.loadings6$PC1[5], y=5*PCA.loadings6$PC2[5], label="PO4", fontface=2) +
  annotate("text", x=5*PCA.loadings6$PC1[6], y=5*PCA.loadings6$PC2[6], label="NH4", fontface=2) +
  annotate("text", x=5*PCA.loadings6$PC1[7], y=5*PCA.loadings6$PC2[7], label="pH", fontface=2) +
  annotate("text", x=5*PCA.loadings6$PC1[8], y=5*PCA.loadings6$PC2[8], label="temp", fontface=2) +
  annotate("text", x=5*PCA.loadings6$PC1[9], y=5*PCA.loadings6$PC2[9], label="s.cond", fontface=2) +
  annotate("text", x=5*PCA.loadings6$PC1[10], y=5*PCA.loadings6$PC2[10], label="DO", fontface=2) +
  ggtitle("Environmental Data") + theme(plot.title = element_text(hjust = 0.5))

pcaplot6




#########################################################################################################
### PCA plot minus WatL, Schwann, Corcoran (with pred info)

env7<-env4[-c(4,18,21),]
pcdata7<-pcdata4[-c(4,18,21),]


pcdata7.scale<-scale(pcdata7)
pca7<-prcomp(pcdata7.scale, center = TRUE,scale. = TRUE)
pca.sum7<-summary(pca7)
PCA.loadings7<-as.data.frame(pca.sum7$rotation)
pred.pca7<-as.data.frame(predict(pca7))
attach(env7)

pcaplot7<-ggplot(pred.pca7, aes(PC1,PC2)) + geom_point(aes(colour = location,shape = pred_regime), size=3) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(aes(label=row.names(pcdata7)), size=4, hjust=1.4,vjust=.3) +
  annotate("segment", x=0, y=0, xend= (5*PCA.loadings7$PC1), yend= (5*PCA.loadings7$PC2), arrow=arrow (length=unit(0.3, "cm")), size=1) +
  annotate("text", x=5*PCA.loadings7$PC1[1], y=5*PCA.loadings7$PC2[1], label="abundance", fontface=2) +
  annotate("text", x=5*PCA.loadings7$PC1[2], y=5*PCA.loadings7$PC2[2], label="cladoceran", fontface=2) +
  annotate("text", x=5*PCA.loadings7$PC1[3], y=5*PCA.loadings7$PC2[3], label="zoops", fontface=2) +
  annotate("text", x=5*PCA.loadings7$PC1[4], y=5*PCA.loadings7$PC2[4], label="chlA", fontface=2) +
  annotate("text", x=5*PCA.loadings7$PC1[5], y=5*PCA.loadings7$PC2[5], label="PO4", fontface=2) +
  annotate("text", x=5*PCA.loadings7$PC1[6], y=5*PCA.loadings7$PC2[6], label="NH4", fontface=2) +
  annotate("text", x=5*PCA.loadings7$PC1[7], y=5*PCA.loadings7$PC2[7], label="pH", fontface=2) +
  annotate("text", x=5*PCA.loadings7$PC1[8], y=5*PCA.loadings7$PC2[8], label="temp", fontface=2) +
  annotate("text", x=5*PCA.loadings7$PC1[9], y=5*PCA.loadings7$PC2[9], label="s.cond", fontface=2) +
  annotate("text", x=5*PCA.loadings7$PC1[10], y=5*PCA.loadings7$PC2[10], label="DO", fontface=2) +
  annotate("text", x=5*PCA.loadings7$PC1[11], y=5*PCA.loadings7$PC2[11], label="BG", fontface=2) +
  annotate("text", x=5*PCA.loadings7$PC1[12], y=5*PCA.loadings7$PC2[12], label="Bass", fontface=2) +
  ggtitle("Environmental Data") + theme(plot.title = element_text(hjust = 0.5))

pcaplot7



###########################################################################################################
#### PCA plot minus corcoran schwann, watl, no pred info


env8<-env2[-c(4,18,21),]
pcdata8<-pcdata.rm[-c(4,18,21),]


pcdata8.scale<-scale(pcdata8)
pca8<-prcomp(pcdata8.scale, center = TRUE,scale. = TRUE)
pca.sum8<-summary(pca8)
PCA.loadings8<-as.data.frame(pca.sum8$rotation)
pred.pca8<-as.data.frame(predict(pca8))
attach(env8)


pcaplot8<-ggplot(pred.pca8, aes(PC1,PC2)) + geom_point(aes(colour = location, shape = pred_regime), size=3) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(aes(label=row.names(pcdata8)), size=4, hjust=1.4,vjust=.3) +
  annotate("segment", x=0, y=0, xend= (5*PCA.loadings8$PC1), yend= (5*PCA.loadings8$PC2), arrow=arrow (length=unit(0.3, "cm")), size=1) +
  annotate("text", x=5*PCA.loadings8$PC1[1], y=5*PCA.loadings8$PC2[1], label="abundance", fontface=2) +
  annotate("text", x=5*PCA.loadings8$PC1[2], y=5*PCA.loadings8$PC2[2], label="cladoceran", fontface=2) +
  annotate("text", x=5*PCA.loadings8$PC1[3], y=5*PCA.loadings8$PC2[3], label="zoops", fontface=2) +
  annotate("text", x=5*PCA.loadings8$PC1[4], y=5*PCA.loadings8$PC2[4], label="chlA", fontface=2) +
  annotate("text", x=5*PCA.loadings8$PC1[5], y=5*PCA.loadings8$PC2[5], label="PO4", fontface=2) +
  annotate("text", x=5*PCA.loadings8$PC1[6], y=5*PCA.loadings8$PC2[6], label="NH4", fontface=2) +
  annotate("text", x=5*PCA.loadings8$PC1[7], y=5*PCA.loadings8$PC2[7], label="pH", fontface=2) +
  annotate("text", x=5*PCA.loadings8$PC1[8], y=5*PCA.loadings8$PC2[8], label="temp", fontface=2) +
  annotate("text", x=5*PCA.loadings8$PC1[9], y=5*PCA.loadings8$PC2[9], label="s.cond", fontface=2) +
  annotate("text", x=5*PCA.loadings8$PC1[10], y=5*PCA.loadings8$PC2[10], label="DO", fontface=2) +
  ggtitle("Environmental Data") + theme(plot.title = element_text(hjust = 0.5))

pcaplot8



################################################################################################################
################################################################################################################
################################################################################################################
### Cluster analysis

cldata<-env4[,-c(1,2,4,6,8,13,17)]

############################ Partitioning method

wss <- (nrow(cldata)-1)*sum(apply(cldata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cldata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")


fit <- kmeans(cldata, 5) # 5 cluster solution
# get cluster means 
aggregate(cldata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
cldata2 <- data.frame(cldata, fit$cluster)



########################### Model Based Clustering

library(mclust)
fit <- Mclust(cldata)
plot(fit) # plot results 
summary(fit) # display the best model






