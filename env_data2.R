
env_factors<-read.csv("C:\\Users\\rhold\\Documents\\UCSC\\Palkovacs Lab Work\\R\\Environmental pond data for PCA_R.csv")

########################################################################################################################
###### PCA analysis
#####
####
env2<-env_factors[-c(4,5,25),-c(2,4,5,7,17)]
pcdata<-env2[,-c(1,3,4,9)]
row.names(pcdata)<-env2$population
pcdata$logzoops<-log(env2$total_zoops_plus_1)
pcdata$logcladoceran<-log(env2$total_cladocerans_plus_1)
pcdata$bg<-as.numeric(c("0","0","0","0","1","0","0","0","0","1","0","0","0","0","0","0","1","1","0","1","0","0"))
pcdata$bass<-as.numeric(c("1","0","0","0","0","0","0","1","0","1","0","0","0","1","0","0","1","1","0","0","0","0"))

pcdata.scale<-scale(pcdata)

pca1<-prcomp(pcdata.scale, center = TRUE,scale. = TRUE)

#### creates summary of the PCA
pca.sum1<-summary(pca1)
PCA.loadings1<-as.data.frame(pca.sum1$rotation)
pred.pca1<-as.data.frame(predict(pca1))

library(ggplot2)

pcaplot1<-ggplot(pred.pca1, aes(PC1,PC2)) + geom_point(aes(colour = env2$location), size=3) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(aes(label=row.names(pcdata)), size=4, hjust=1.4,vjust=.3) +
  annotate("segment", x=0, y=0, xend= (5*PCA.loadings1$PC1), yend= (5*PCA.loadings1$PC2), arrow=arrow (length=unit(0.3, "cm")), size=1) +
  annotate("text", x=5*PCA.loadings1$PC1[1], y=5*PCA.loadings1$PC2[1], label="abundance", fontface=2) +
  annotate("text", x=5*PCA.loadings1$PC1[2], y=5*PCA.loadings1$PC2[2], label="chlA", fontface=2) +
  annotate("text", x=5*PCA.loadings1$PC1[3], y=5*PCA.loadings1$PC2[3], label="PO4", fontface=2) +
  annotate("text", x=5*PCA.loadings1$PC1[4], y=5*PCA.loadings1$PC2[4], label="NH4", fontface=2) +
  annotate("text", x=5*PCA.loadings1$PC1[5], y=5*PCA.loadings1$PC2[5], label="pH", fontface=2) +
  annotate("text", x=5*PCA.loadings1$PC1[6], y=5*PCA.loadings1$PC2[6], label="temp", fontface=2) +
  annotate("text", x=5*PCA.loadings1$PC1[7], y=5*PCA.loadings1$PC2[7], label="s_cond", fontface=2) +
  annotate("text", x=5*PCA.loadings1$PC1[8], y=5*PCA.loadings1$PC2[8], label="DO", fontface=2) +
  annotate("text", x=5*PCA.loadings1$PC1[9], y=5*PCA.loadings1$PC2[9], label="logzoops", fontface=2) +
  annotate("text", x=5*PCA.loadings1$PC1[10], y=5*PCA.loadings1$PC2[10], label="logclad", fontface=2) +
  annotate("text", x=5*PCA.loadings1$PC1[11], y=5*PCA.loadings1$PC2[11], label="BG", fontface=2) +
  annotate("text", x=5*PCA.loadings1$PC1[12], y=5*PCA.loadings1$PC2[12], label="Bass", fontface=2) +
  ggtitle("Environmental Data") + theme(plot.title = element_text(hjust = 0.5))

pcaplot1



#################################################################################################################
###### PCA plot minus Bishop locations

inc1<-which(env2$location != "B")
env3<-env2[inc1,]
pcdata2<-env3[,-c(1,3,4,9)]
row.names(pcdata2)<-env3$population
pcdata2.scale<-scale(pcdata2)
pca2<-prcomp(pcdata2.scale, center = TRUE,scale. = TRUE)
pca.sum2<-summary(pca2)
PCA.loadings2<-as.data.frame(pca.sum2$rotation)
pred.pca2<-as.data.frame(predict(pca2))


pcaplot5<-ggplot(pred.pca2, aes(PC1,PC2)) + geom_point() + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(aes(label=row.names(pcdata2)), size=4, hjust=1.4,vjust=.3) +
  annotate("segment", x=0, y=0, xend= (5*PCA.loadings2$PC1), yend= (5*PCA.loadings2$PC2), arrow=arrow (length=unit(0.3, "cm")), size=1) +
  annotate("text", x=5*PCA.loadings2$PC1[1], y=5*PCA.loadings2$PC2[1], label="abundance", fontface=2) +
  annotate("text", x=5*PCA.loadings2$PC1[2], y=5*PCA.loadings2$PC2[2], label="cladoceran", fontface=2) +
  annotate("text", x=5*PCA.loadings2$PC1[3], y=5*PCA.loadings2$PC2[3], label="zoops", fontface=2) +
  annotate("text", x=5*PCA.loadings2$PC1[4], y=5*PCA.loadings2$PC2[4], label="chlA", fontface=2) +
  annotate("text", x=5*PCA.loadings2$PC1[5], y=5*PCA.loadings2$PC2[5], label="PO4", fontface=2) +
  annotate("text", x=5*PCA.loadings2$PC1[6], y=5*PCA.loadings2$PC2[6], label="NH4", fontface=2) +
  annotate("text", x=5*PCA.loadings2$PC1[7], y=5*PCA.loadings2$PC2[7], label="pH", fontface=2) +
  annotate("text", x=5*PCA.loadings2$PC1[8], y=5*PCA.loadings2$PC2[8], label="temp", fontface=2) +
  annotate("text", x=5*PCA.loadings2$PC1[9], y=5*PCA.loadings2$PC2[9], label="s.cond", fontface=2) +
  annotate("text", x=5*PCA.loadings2$PC1[10], y=5*PCA.loadings2$PC2[10], label="DO", fontface=2) +
  annotate("text", x=5*PCA.loadings2$PC1[11], y=5*PCA.loadings2$PC2[11], label="BG", fontface=2) +
  annotate("text", x=5*PCA.loadings2$PC1[12], y=5*PCA.loadings2$PC2[12], label="Bass", fontface=2) +
  ggtitle("Environmental Data") + theme(plot.title = element_text(hjust = 0.5))

pcaplot5



