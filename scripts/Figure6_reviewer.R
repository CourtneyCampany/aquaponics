source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$ITE <- with(gasex, A/(E*1000))

stomata <- read.csv("raw_data/stomata_traits.csv")


alldata <- merge(gasex, stomata, by=c('species', 'treatment', 'week', 'plant'), all=TRUE)
  alldata$species <- as.factor(alldata$species)
  alldata$trial <- as.factor(alldata$trial)
  alldata$treatment <- as.factor(alldata$treatment)

soil <- alldata[alldata$treatment == 'C',]
aqua <- alldata[alldata$treatment == 'A',]

element <- read.csv("raw_data/elemental_leaves.csv")
  element$species <- as.factor(element$species)
  element$trial <- as.factor(element$trial)
  element$treatment <- as.factor(element$treatment)
  element$uniqueid <- paste(element$species, element$treatment, sep="-")
  element$nmass <- element$n_perc/100


#linear models for photosynthesis x nitrogen
photo_agg <- doBy::summaryBy(A ~ treatment + species + plant , 
                               data =gasex, FUN=mean, keep.names=TRUE )
                             
photonitro <- merge(photo_agg, element)
  photonitro$species <- as.factor(photonitro$species)
  photonitro$treatment <- as.factor(photonitro$treatment)

photonitro_lastweek <- merge(photo_agg, element)

##setup for plotting (splitting data and simple models) with An
container <- photonitro[photonitro$treatment == 'C',]
aquaponic <- photonitro[photonitro$treatment == 'A',]

#simple models - big aqua vs soil
aquamod <- lm(A ~ nmass ,data=aquaponic)
soilmod <- lm(A ~ nmass ,data=container)

#nmass by species
broc_mod <- lm(A ~ nmass ,data=photonitro[photonitro$species =="B",])
pac_mod <- lm(A ~ nmass ,data=photonitro[photonitro$species =="P",])
sal_mod <- lm(A ~ nmass ,data=photonitro[photonitro$species =="S",])

aquamod2 <- lm(A ~ gsw ,data=gasex[gasex$treatment == "A",])
soilmod2 <- lm(A ~ gsw ,data=gasex[gasex$treatment == "C",])

aquamod3 <- lm(A ~ sd_mm2 ,data=alldata[alldata$treatment == "A",])
soilmod3 <- lm(A ~ sd_mm2 ,data=alldata[alldata$treatment == "C",])

anmass_mod <- lm(A ~ nmass, data=photonitro)


#GAM - non-linear fits
library(mgcv)

##A vs gsw non-linear with GAM
##match K to visual fit
# agsw_mod_aqua <- gam(A ~ s(gsw, k=3), data=aqua)
# summary(agsw_mod_aqua) #p<0.001
# anova(agsw_mod_aqua)
# 
# agsw_mod_soil <- gam(A ~ s(gsw, k=4), data=soil)
# summary(agsw_mod_soil) #p<0.001
# 
##A vs SD non-linear with GAM
asd_mod_aqua <- gam(A ~ s(sd_mm2, k=4), data=aqua)
summary(asd_mod_aqua) #p<0.001

asd_mod_soil <- gam(A ~ s(sd_mm2, k=4), data=soil)
summary(asd_mod_soil) #p<0.001



jpeg(filename = "output/Figure6reviewer.jpeg",width = 12, height = 6 ,units = "in", res= 500)

# windows(12,6)
par(mgp=c(2.5,.75,0), cex.lab=1.15,cex.axis=1.15,mfrow=c(1,3))

#Panel 1 - A vs Nitro
par(mar=c(5,5,1,0))
plot(A ~ nmass, data=photonitro, type='n', ylab=photolab, xlab=nitrolab, ylim=c(0,30), 
     xlim=c(0,.1))
points(A ~ nmass, data=aquaponic, col=trtcols2[1], pch=pchs[species],cex=1.25)
points(A ~ nmass, data=container, col=trtcols2[2], pch=pchs[species],cex=1.25)


predline(broc_mod, col="black", lwd=2, lty=2)
text(.072, 16.5, "Broccoli")
predline(pac_mod, col="black", lwd=2, lty=2)
text(.085, 20.1, "Pak Choi")
predline(sal_mod, col="black", lwd=2, lty=2)
text(.09, 13.6, "Salanova")

legend("topright", legend = specieslabs, pch=pchs, col="black", bty="n", inset=.02)
legend(.05,31, boxlabs, pch=16, col=trtcols, inset=0.02,  title="", bty='n') 

# text(0.1025, 7, expression(paste(R[cond]^{"2"}," = "," 0.37")), 1.25)
# text(0.1025, 5, expression(paste(R[marg]^{"2"}," = "," 0.90")), 1.25)
text(0, 30, "A", cex=1.5, font=2)

#Panel 2 - A vs GS
par(mar=c(5,0,1,0))

plot(A ~ gsw, data=alldata, type='n', ylab="", yaxt='n', xlab="", ylim=c(0,30), xlim=c(0,2))
axis(2, labels=FALSE)  
par(new=TRUE)
smoothplot(gsw, A, data=aqua, kgam=3, R="plant", ylab="", axes=FALSE,
           xlab=condlab, ylim=c(0,30), xlim=c(0,1.7), linecols=trtcols[1], pch="")
par(new=TRUE)
smoothplot(gsw, A, data=soil, kgam=4, R="plant", ylab="", axes=FALSE,
           xlab="", ylim=c(0,30), xlim=c(0,1.7), linecols=trtcols[2], pch="")

points(A ~ gsw, data=aqua, col=trtcols3[1], pch=pchs[species],cex=1.25)
points(A ~ gsw, data=soil, col=trtcols3[2], pch=pchs[species],cex=1.25)

text(0, 30, "B", cex=1.5, font=2)

#Panel 23 - A vs SD
par(mar=c(5,0,1,1))
plot(A ~ sd_mm2, data=alldata, type='n', yaxt='n', ylab="", xlab=denslab, 
     ylim=c(0,30), xlim=c(0,850))
axis(2, labels=FALSE)  
points(A ~ sd_mm2, data=aqua, col=trtcols2[1], pch=pchs[species],cex=1.25)
points(A ~ sd_mm2, data=soil, col=trtcols2[2], pch=pchs[species],cex=1.25)
predline(aquamod3, col=trtcols[1], lwd=2, lty=2)
predline(soilmod3, col=trtcols[2], lwd=2, lty=2)

# legend("topleft", legend = specieslabs, pch=pchs, col="black", bty="n", inset=.02)
# legend("topright", boxlabs, pch=16, col=trtcols, inset=0.02,  title="", bty='n') 

text(10, 30, "C", cex=1.5, font=2)
# text(850, 7, expression(paste(R[cond]^{"2"}," = "," 0.81")), 1.25)
# text(850, 5, expression(paste(R[marg]^{"2"}," = "," 0.25")), 1.25)

dev.off()


