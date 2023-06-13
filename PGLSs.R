Sys.setenv(LANG = "en")

library("ape")
library(nlme)
library(piecewiseSEM)
library(viridis)
library(ggplot2)

#### Trait data loading ####

dataPrange <- as.data.frame(read.csv("TerrestrialExtant.csv", sep = ";"))
dataPrange<-aggregate(cbind(BodyMass,SkeletalMass)  ~ Taxon , data=dataPrange,mean)
names(dataPrange)[names(dataPrange)=="Specimen_Source"]<- "Specimen"
rownames(dataPrange) <- dataPrange$Taxon
dataPrange$Lifestyle <- "Te"
dataPrange$SkeletalMassMin <- dataPrange$SkeletalMassMax <- dataPrange$BodyMassMin <- dataPrange$BodyMassMax <- dataPrange$Specimen <- dataPrange$Abrev <- NA

dataPrange$Lifestyle <-as.factor(dataPrange$Lifestyle)
dataPrange$Cet <-dataPrange$Cet4Reg  <-"0"; dataPrange$Data4Plot <- "1"

DataEstim <- as.data.frame(read.csv("AquaticAndEstimations.csv", sep = ";"))
DataEstim$Lifestyle <-as.factor(DataEstim$Lifestyle)


#### Tree loading and tip adjustments ####

treeAll <- read.nexus("MamPhy_fullPosterior_BDvr_DNAonly_4098sp_topoFree_NDexp_MCC_v2_target.tre")# from Upham et al. (2019): http://vertlife.org/data/mammals/
allLabels <- treeAll$tip.label; shortLabels <- c()
for (i in 1:length(allLabels)){
  shortLabels[i] <- paste(unlist(strsplit(allLabels[i], split = "_"))[1], unlist(strsplit(allLabels[i], split = "_"))[2],sep="_")
}

#Species match corrections
shortLabels[grep("Sorex_ari",shortLabels)] <- "Soricid_sp"
shortLabels[grep("Peromyscus_fraterculus",shortLabels)] <- "Peromyscus_sp"
shortLabels[grep("Tamias_striatus",shortLabels)] <- "Eutamias_sp"
shortLabels[grep("Spermophilus_pygmaeus",shortLabels)] <- "Spermophilus_sp"
shortLabels[grep("Thomomys_talpoides",shortLabels)] <- "Thomomys_sp"
shortLabels[grep("Castor_fiber",shortLabels)] <- "Castor_sp"
shortLabels[grep("Canis_lupus",shortLabels)] <- "Canis_sp"
shortLabels[grep("Mustela_strigidorsa",shortLabels)] <- "Mustela_sp"
shortLabels[grep("Spilogale_putorius",shortLabels)] <- "Spilogale_sp"
shortLabels[grep("Felis_nigripes",shortLabels)] <- "Felis_sp"

#Update tip names  
treeAll$tip.label <- shortLabels

#Trees cropped with sample
treeSamp <-  keep.tip(treeAll,dataPrange$Taxon)


#### Gls regression for terrestrial mammals #### 

  #Comparing the fits of white noise, Pagel's lambda, and Brownian motion:
fitGls_uninformed <- gls(log10(SkeletalMass)~log10(BodyMass),data = dataPrange,correlation=corPagel(value=0,treeSamp, fixed=T, form=~Taxon),method="ML")
summary(fitGls_uninformed);rsquared(fitGls_uninformed)

fitGlsLambda <- gls(log10(SkeletalMass)~log10(BodyMass),data = dataPrange,correlation=corPagel(value=1,treeSamp, fixed=F, form=~Taxon),method="REML")
summary(fitGlsLambda);rsquared(fitGlsLambda)
fitGlsLambda$modelStruct # Lambda slighlty below 0

fitGlsBM <- gls(log10(SkeletalMass)~log10(BodyMass),data = dataPrange,correlation=corPagel(value=1,treeSamp, fixed=T, form=~Taxon),method="ML")
summary(fitGlsBM); rsquared(fitGlsBM)

fitGls <- fitGls_uninformed
plot(fitGls);qqnorm(fitGls,abline=c(0,1));shapiro.test(fitGls$residuals)

  #Printing the results
summary(fitGls); rsquared(fitGls)
summary(fitGls)$tTable;round(summary(fitGls)$tTable,digits = 2)

  #Test for isometry
fitGlsIso <- gls(log10(SkeletalMass)-log10(BodyMass)~log10(BodyMass),data = dataPrange,correlation=corPagel(value=0,treeSamp, fixed=T, form=~Taxon),method="ML")

    #Printing the results
summary(fitGlsIso);rsquared(fitGlsIso)
summary(fitGlsIso)$tTable;round(summary(fitGlsIso)$tTable,digits = 2)

  #Making estimations of large, extinct terrestrial mammals based on the regression
DataEstim[DataEstim$Taxon=="Palaeoloxodon_namadicus","SkeletalMass"] <-10^(fitGls$coefficients["(Intercept)"]+log10(DataEstim[DataEstim$Taxon=="Palaeoloxodon_namadicus","BodyMass"])*fitGls$coefficients[2])
DataEstim[DataEstim$Taxon=="Paraceratherium_transouralicum","SkeletalMass"] <-10^(fitGls$coefficients["(Intercept)"]+log10(DataEstim[DataEstim$Taxon=="Paraceratherium_transouralicum","BodyMass"])*fitGls$coefficients[2])


#### Gls regression for cetaceans #### 

  #Subsetting to extant cetaceans (with associated body mass) 
DataEstimCet <- subset(DataEstim,Cet4Reg=="1")
  #Preparing data for phyloPGLS
DataEstimCet_M <- DataEstimCet[,sapply(DataEstimCet, class)=="numeric"];DataEstimCet_M$Taxon <- DataEstimCet$Taxon
DataEstimCet_M<-aggregate(. ~Taxon,data=DataEstimCet_M,mean, na.rm=TRUE, na.action=NULL)
TreeCet <- keep.tip(treeAll,DataEstimCet_M$Taxon)

  #Comparing the fits of white noise, Pagel's lambda, and Brownian motion:
fitGls_uninformed_Cet <- gls(log10(SkeletalMass)~log10(BodyMass),data = DataEstimCet_M,correlation=corPagel(value=0,TreeCet, fixed=T, form=~Taxon),method="ML")
summary(fitGls_uninformed_Cet);rsquared(fitGls_uninformed_Cet)

fitGlsLambda_Cet <- gls(log10(SkeletalMass)~log10(BodyMass),data = DataEstimCet_M,correlation=corPagel(value=1,TreeCet, fixed=F, form=~Taxon),method="ML")
summary(fitGlsLambda_Cet);rsquared(fitGlsLambda_Cet)
fitGlsLambda_Cet$modelStruct # Lambda above 1

fitGlsBM_Cet <- gls(log10(SkeletalMass)~log10(BodyMass),data = DataEstimCet_M,correlation=corPagel(value=1,TreeCet, fixed=T, form=~Taxon),method="ML")
summary(fitGlsBM_Cet);rsquared(fitGlsBM_Cet)

fitGls_Cet <- fitGlsBM_Cet
plot(fitGls_Cet); shapiro.test(fitGls_Cet$residuals); qqnorm(fitGls_Cet,abline=c(0,1))

  #Printing the results
summary(fitGls_Cet); rsquared(fitGls_Cet)
summary(fitGls_Cet)$tTable;round(summary(fitGls_Cet)$tTable,digits = 2)

  #Test for isometry
fitGlsBM_Cet_Iso <- gls(log10(SkeletalMass)-log10(BodyMass)~log10(BodyMass),data = DataEstimCet_M,correlation=corPagel(value=1,TreeCet, fixed=T, form=~Taxon),method="ML")
summary(fitGlsBM_Cet_Iso); rsquared(fitGlsBM_Cet_Iso);plot(fitGlsBM_Cet_Iso)
    #Printing the results
summary(fitGlsBM_Cet_Iso)$tTable;round(summary(fitGlsBM_Cet_Iso)$tTable,digits = 2)


#### Figure #### 

#Combining datasets
dataPrange[,"Prange"] <- "1";DataEstim[,"Prange"] <- "0";
dataAll <- rbind(dataPrange,DataEstim)
viridis10=viridis(10);cols=c(viridis10[9],viridis10[4],viridis10[7],"grey","black")

dataPlot <- subset(dataAll, Data4Plot==1)#sub-setting to have species mean for largest cetaceans 

dataPlot[dataPlot$Cet==1,"Groups"]  <- "Cetacea"
dataPlot[dataPlot$Cet==0 & dataPlot$Lifestyle=="Aq","Groups"] <- "Sirenia"
dataPlot[dataPlot$Prange==1,"Groups"]  <- "TerrExtant"
dataPlot[dataPlot$Prange==0 & dataPlot$Lifestyle=="Te","Groups"] <- "TerrEstim"
dataPlot[dataPlot$Lifestyle=="NEW","Groups"] <- "NEW"

dataPlot$Groups <- as.factor(dataPlot$Groups);dataPlot$Groups <- factor(dataPlot$Groups,levels(dataPlot$Groups)[c(2,1,3:5)])

  #defining range of the Cetacean regression line
Cet <- dataPlot$Cet==1
x_Cet <- min(dataPlot[Cet,"BodyMass"])
x_Cet_end <- max(dataPlot[Cet,"BodyMass"])
y_Cet <- 10^(fitGls_Cet$coef[1]+log10(x_Cet)*fitGls_Cet$coef[2])
y_Cet_end <- 10^(fitGls_Cet$coef[1]+log10(x_Cet_end)*fitGls_Cet$coef[2])


#dev.off()
#svg(file ='RplotRegAllF1.svg', height=90/25.4,width=136*1.3/25.4)
p <- ggplot(dataPlot, aes(BodyMass, SkeletalMass,color=Groups,label=Abrev))
p + scale_x_continuous(trans='log10',breaks=c(0,0.001,0.1,10,1000,100000)) +
  scale_y_continuous(trans='log10',breaks=c(0,0.001,0.1,10,1000,100000)) +
  expand_limits(x=c(min(dataPlot$BodyMass), max(dataPlot$BodyMass)),y=c(min(dataPlot$SkeletalMass), max(dataPlot$SkeletalMass)))+
  geom_abline(aes(intercept=fitGls$coefficients[1], slope=fitGls$coefficients[2]),colour="azure4")+
  geom_segment(aes(x = x_Cet, xend = x_Cet_end, y = y_Cet, yend = y_Cet_end),colour="#8db5d3ff") +
  geom_errorbar(aes(ymin=SkeletalMassMin, ymax=SkeletalMassMax))+
  geom_errorbarh(aes(xmin =BodyMassMin, xmax = BodyMassMax))+
  scale_color_manual(values=cols)+
  scale_fill_manual(values=cols)+
  geom_point(alpha = 0.8)+
  xlab("Body mass (kg)")+ ylab("Skeletal mass (kg)")+
  theme_classic()
#dev.off()

