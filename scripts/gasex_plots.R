source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$species <- as.factor(gasex$species)
  gasex$trial <- as.factor(gasex$trial)
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$ITE <- with(gasex, A/(E*1000))

  
stomcond_modA <- lm(A ~ gsw ,data=gasex[gasex$treatment=='a',])
stomphoto_modC <- lm(A ~ gsw ,data=gasex[gasex$treatment=='c',])
  
windows(7,7)
par(mar=c(5,5,1,1),las=1)
plot(A ~ gsw, col=trtcols2[treatment], data=gasex, ylim=c(0,26), xlim=c(0,1.7),
     ylab=photolab, xlab=condlab, pch=pchs[species])

predline(stomcond_modA, col=trtcols[1], lwd=2, lty=2)
predline(stomphoto_modC, col=trtcols[2],lwd=2, lty=2)

legend("bottomright", specieslabs, pch=pchs, inset=0.02,  title="", bty='n') 
legend("topleft", boxlabs, pch=16, col=trtcols, inset=0.02,  title="", bty='n') 

dev.off()


##by treatment and species-----

lettuceaqua <- gasex[gasex$species == "S" & gasex$treatment == "a",]
lettucesoil <- gasex[gasex$species == "S" & gasex$treatment == "c",]

brocaqua <- gasex[gasex$species == "B" & gasex$treatment == "a",]
brocsoil <- gasex[gasex$species == "B" & gasex$treatment == "c",]

pacaqua <- gasex[gasex$species == "P" & gasex$treatment == "a",]
pacsoil <- gasex[gasex$species == "P" & gasex$treatment == "c",]

##simple models
lettuce_modA <- lm(A ~ gsw ,data=lettuceaqua)
lettuce_modC <- lm(A ~ gsw ,data=lettucesoil)

broc_modA <- lm(A ~ gsw ,data=brocaqua)
broc_modC <- lm(A ~ gsw ,data=brocsoil)

pac_modA <- lm(A ~ gsw ,data=pacaqua)
pac_modC <- lm(A ~ gsw ,data=pacsoil)

##graphing by treatment
aqua <- gasex[gasex$treatment == "a",]
soil <- gasex[gasex$treatment == "c",]

# windows(7,7)

jpeg(filename = "output/photoconductance.jpeg",
     width = 8.4, height = 8.4, units = "in", res= 300)

par(mgp=c(2.5,.75,0), cex.lab=1.15,cex.axis=1.15, mfrow=c(2,1))
#aqua
par(mar=c(0,5,1,1))
plot(A ~ gsw, col=trtcols2[1], data=aqua, ylim=c(0,26), xlim=c(0,1.6),
     ylab=photolab, xlab="", pch=pchs[species], xaxt="n", type='n')

predline(lettuce_modA, col="black", lwd=2, lty=2)
predline(broc_modA, col="black",lwd=2, lty=2)
predline(pac_modA, col="black",lwd=2, lty=2)

points(A ~ gsw, col=trtcols2[1], data=aqua,  pch=pchs[species])

legend("bottomright", specieslabs, pch=pchs, inset=0.02,  title="", bty='n') 
legend("topleft", "Aquaponics", inset=0.01,  title="", bty='n') 

#soil
par(mar=c(5,5,0,1))
plot(A ~ gsw, col=trtcols2[2], data=soil, ylim=c(0,26), xlim=c(0,1.6),
     ylab=photolab, xlab=condlab, pch=pchs[species], type='n')

predline(lettuce_modC, col="black", lwd=2, lty=2)
predline(broc_modC, col="black",lwd=2, lty=2)
predline(pac_modC, col="black",lwd=2, lty=2)

points(A ~ gsw, col=trtcols2[2], data=soil,  pch=pchs[species])

legend("topleft", "Soil",  title="", bty='n') 

dev.off()
