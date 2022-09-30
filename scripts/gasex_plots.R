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

