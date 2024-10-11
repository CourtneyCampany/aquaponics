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


# 2 possible approaches = average of A across weeks, or choose week closest to N harvest
photo_agg <- doBy::summaryBy(A + gsw ~ treatment + species + plant , 
                             data =gasex, FUN=mean, keep.names=TRUE)

#GAM - non-linear fits
library(mgcv)

##A vs gsw non-linear with GAM
##match K to visual fit
agsw_mod_aqua <- gam(A ~ s(gsw, k=3), data=aqua)
  summary(agsw_mod_aqua) #p<0.001
  anova(agsw_mod_aqua)

agsw_mod_soil <- gam(A ~ s(gsw, k=4), data=soil)
  summary(Agsw_mod_soil) #p<0.001

##A vs SD non-linear with GAM
asd_mod_aqua <- gam(A ~ s(sd_mm2, k=4), data=aqua)
  summary(asd_mod_aqua) #p<0.001

asd_mod_soil <- gam(A ~ s(sd_mm2, k=4), data=soil)
  summary(asd_mod_soil) #p<0.001

  
windows(6,6)
par(mgp=c(2.5,.75,0), cex.lab=1.15,cex.axis=1.15)
plot(A ~ gsw, data=alldata, type='n', ylab="", xlab="", ylim=c(0,30), xlim=c(0,2))
# axis(2, labels=FALSE)  
par(new=TRUE)
smoothplot(gsw, A, data=aqua, kgam=3, R="plant", ylab=satlab, 
           xlab=condlab, ylim=c(0,30), xlim=c(0,2), linecols=trtcols[1], pch="")
par(new=TRUE)
smoothplot(gsw, A, data=soil, kgam=4, R="plant", ylab=satlab, 
           xlab=condlab, ylim=c(0,30), xlim=c(0,2), linecols=trtcols[2], pch="")

points(A ~ gsw, data=aqua, col=trtcols3[1], pch=pchs[species],cex=1.25)
points(A ~ gsw, data=soil, col=trtcols3[2], pch=pchs[species],cex=1.25)

# legend("topleft", legend = specieslabs, pch=pchs, col="black", bty="n", inset=.02)
# legend("topright", boxlabs, pch=16, col=trtcols, inset=0.02,  title="", bty='n') 

text(0, 29.5, "B", cex=1.25, font=2)
# text(1.75, 7, expression(paste(R[cond]^{"2"}," = "," 0.78")), 1.25)
# text(1.75, 5, expression(paste(R[marg]^{"2"}," = "," 0.56")), 1.25)
dev.off()

windows(6,6)
par(mgp=c(2.5,.75,0), cex.lab=1.15,cex.axis=1.15)
plot(A ~ sd_mm2, data=alldata, type='n', yaxt='n', ylab="", xlab="", 
     ylim=c(0,30), xlim=c(0,850))
axis(2, labels=FALSE)  
par(new=TRUE)
smoothplot(sd_mm2, A,data=aqua, kgam=4, R="plant", ylab="", 
           xlab=denslab, ylim=c(0,30), xlim=c(0,850), linecols=trtcols[1], pch="")
par(new=TRUE)
smoothplot(sd_mm2, A, data=soil, kgam=4, R="plant", ylab="", 
           xlab=denslab, ylim=c(0,30), xlim=c(0,850), linecols=trtcols[2], pch="")

points(A ~ sd_mm2, data=aqua, col=trtcols3[1], pch=pchs[species],cex=1.25)
points(A ~ sd_mm2, data=soil, col=trtcols3[2], pch=pchs[species],cex=1.25)

# legend("topleft", legend = specieslabs, pch=pchs, col="black", bty="n", inset=.02)
# legend("topright", boxlabs, pch=16, col=trtcols, inset=0.02,  title="", bty='n') 

text(10, 29.5, "C", cex=1.25, font=2)
# text(850, 7, expression(paste(R[cond]^{"2"}," = "," 0.81")), 1.25)
# text(850, 5, expression(paste(R[marg]^{"2"}," = "," 0.25")), 1.25)

dev.off()


