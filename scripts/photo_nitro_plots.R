source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$species <- as.factor(gasex$species)
  gasex$trial <- as.factor(gasex$trial)
  gasex$treatment <- as.factor(gasex$treatment)
  
# 2 possible approaches = average of A across weeks, or choose week closest to N harvest
photo_agg <- doBy::summaryBy(A ~ treatment + species + plant , 
                                         data =gasex, FUN=mean, keep.names=TRUE)

photo_lastweek <- gasex[gasex$week == 3, ] ##need to fix with broc week4
  
element <- read.csv("raw_data/elemental_leaves.csv")
  element$species <- as.factor(element$species)
  element$trial <- as.factor(element$trial)
  element$treatment <- as.factor(element$treatment)
  element$uniqueid <- paste(element$species, element$treatment, sep="-")
  element$nmass <- element$n_perc/100
  
photonitro <- merge(photo_agg, element)
photonitro_lastweek <- merge(photo_agg, element)


##setup for plotting (splitting data and simple models)

container <- photonitro[photonitro$treatment == 'C',]
aquaponic <- photonitro[photonitro$treatment == 'A',]

#simple model - big aqua vs soil
aquamod <- lm(A ~ nmass ,data=aquaponic)
soilmod <- lm(A ~ nmass ,data=container)

anmass_mod <- lm(A ~ nmass, data=photonitro)

###simple models for graphs (a by nitro)----------------------
# broc_cont <- lm(A ~ nmass ,data=container[container$species=='B',])
# pac_cont <- lm(A ~ nmass ,data=container[container$species=='P',])
# lettuce_cont <- lm(A ~ nmass ,data = container[container$species=='S',])
# 
# broc_aqua <- lm(A ~ nmass ,data=aquaponic[aquaponic$species=='B',])
# pac_aqua <- lm(A ~ nmass ,data=aquaponic[aquaponic$species=='P',])
# lettuce_aqua <- lm(A ~ nmass ,data = aquaponic[aquaponic$species=='S',])

###nitro in soil by species?
jpeg(filename = "output/photonitro.jpeg",width = 6, height = 6, units = "in", res= 400)

# windows(8,8)
par(mgp=c(2.5,.75,0), cex.lab=1.15,cex.axis=1.15,mar=c(5,5,1,1))

plot(A ~ nmass, data=photonitro, type='n', ylab=photolab, xlab=nitrolab, ylim=c(0,30), 
     xlim=c(0,.1))

points(A ~ nmass, data=aquaponic, col=trtcols2[1], pch=pchs[species],cex=1.25)
points(A ~ nmass, data=container, col=trtcols2[2], pch=pchs[species],cex=1.25)
# predline(aquamod, col=trtcols[1], lwd=2, lty=2)
# predline(soilmod, col=trtcols[2], lwd=2, lty=2)
predline(anmass_mod, col="black", lwd=2, lty=2)

legend("topleft", legend = specieslabs, pch=pchs, col="black", bty="n", inset=.02)
legend("bottomright", boxlabs, pch=16, col=trtcols, inset=0.02,  title="", bty='n') 
# text(1.65, 29.5, "A", cex=1.25)
# text(0.25,1, "Aquaponics", cex=1.25)
text(0.1025, 7, expression(paste(R[cond]^{"2"}," = "," 0.37")), 1.25)
text(0.1025, 5, expression(paste(R[marg]^{"2"}," = "," 0.90")), 1.25)

dev.off()
