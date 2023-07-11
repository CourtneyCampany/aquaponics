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


photo_lastweek <- gasex[gasex$week == 3, ] ##need to fix with broc week4

element <- read.csv("raw_data/elemental_leaves.csv")
  element$species <- as.factor(element$species)
  element$trial <- as.factor(element$trial)
  element$treatment <- as.factor(element$treatment)
  element$uniqueid <- paste(element$species, element$treatment, sep="-")
  element$nmass <- element$n_perc/100


# 2 possible approaches = average of A across weeks, or choose week closest to N harvest
photo_agg <- doBy::summaryBy(A ~ treatment + species + plant , 
                             data =gasex, FUN=mean, keep.names=TRUE)

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

aquamod2 <- lm(A ~ gsw ,data=gasex[gasex$treatment == "A",])
soilmod2 <- lm(A ~ gsw ,data=gasex[gasex$treatment == "C",])

aquamod3 <- lm(A ~ sd_mm2 ,data=alldata[alldata$treatment == "A",])
soilmod3 <- lm(A ~ sd_mm2 ,data=alldata[alldata$treatment == "C",])

anmass_mod <- lm(A ~ nmass, data=photonitro)

#A vs N
jpeg(filename = "botany2023/photonitro.jpeg",
     width = 5.75, height = 5.75, units = "in", res= 500)
par(mgp=c(2.5,.75,0), cex.lab=1,cex.axis=1)

par(mar=c(4,4,1,1))

plot(A ~ nmass, data=photonitro, type='n', ylab=photolab, xlab=nitrolab, ylim=c(0,30), 
     xlim=c(0,.1))
points(A ~ nmass, data=aquaponic, col=trtcols2[1], pch=pchs[species],cex=1.5)
points(A ~ nmass, data=container, col=trtcols2[2], pch=pchs[species],cex=1.5)
# predline(aquamod, col=trtcols[1], lwd=2, lty=2)
# predline(soilmod, col=trtcols[2], lwd=2, lty=2)
predline(anmass_mod, col="black", lwd=2, lty=2)

legend("topright", legend = specieslabs, pch=pchs, col="black", bty="n", inset=.02)
legend(.05,31, boxlabs, pch=16, col=trtcols, inset=0.02,  title="", bty='n') 

# text(0.1025, 7, expression(paste(R[cond]^{"2"}," = "," 0.37")), 1.25)
# text(0.1025, 5, expression(paste(R[marg]^{"2"}," = "," 0.90")), 1.25)
dev.off()

#A vs GS
jpeg(filename = "botany2023/photogsw.jpeg",
     width = 5.75, height = 5.75, units = "in", res= 500)
par(mgp=c(2.5,.75,0), cex.lab=1,cex.axis=1)

par(mar=c(4,4,1,1))

plot(A ~ gsw, data=alldata, type='n', yaxt='n', xlab=condlab, ylim=c(0,30), ylab=photolab, xlim=c(0,1.7))

predline(aquamod2, col=trtcols[1], lwd=2, lty=2)
predline(soilmod2, col=trtcols[2], lwd=2, lty=2)
points(A ~ gsw, data=aqua, col=trtcols2[1], pch=pchs[species],cex=1.5)
points(A ~ gsw, data=soil, col=trtcols2[2], pch=pchs[species],cex=1.5)

legend("topleft", legend = specieslabs, pch=pchs, col="black", bty="n", inset=.02)
legend("topright", boxlabs, pch=16, col=trtcols, inset=0.02,  title="", bty='n')

# text(1.75, 7, expression(paste(R[cond]^{"2"}," = "," 0.78")), 1.25)
# text(1.75, 5, expression(paste(R[marg]^{"2"}," = "," 0.56")), 1.25)
dev.off()

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

text(10, 29.5, "C", cex=1.25, font=2)
text(850, 7, expression(paste(R[cond]^{"2"}," = "," 0.81")), 1.25)
text(850, 5, expression(paste(R[marg]^{"2"}," = "," 0.25")), 1.25)

dev.off()


