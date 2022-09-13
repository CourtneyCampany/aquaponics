## script to make 4 panel figure for brcbio grant

source("scripts/functions.R")
source("scripts/plot_objects.R")

#gas exchange data-------
gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$species <- as.factor(gasex$species)
  gasex$trial <- as.factor(gasex$trial)
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$ITE <- with(gasex, A/(E*1000))
  
 #bivariateplpots
  lettuceaqua <- gasex[gasex$species == "S" & gasex$treatment == "a",]
  lettucesoil <- gasex[gasex$species == "S" & gasex$treatment == "c",]
  ags_mod_simp_aqua <- lm(A ~ gsw, data=lettuceaqua)
  ags_mod_simp_soil <- lm(A ~ gsw, data=lettucesoil)
  
  lettuce_gas <- gasex[gasex$species == "S",]
  
#biomass data------
biomass <- read.csv("raw_data/leafmassfinal.csv")
  biomass$aboveground <- with(biomass, shoots + leafexcess_week1+ leafexcess_week2 + 
                                leafexcess_week3 + leafexcess_week4)
  biomass$totalbiomass <- with(biomass, aboveground + roots)
  biomass$rs_ratio <- with(biomass, roots/aboveground)
  
lettuce <- biomass[biomass$species == "S",]
  mass_perc <- data.frame(treatment=as.factor(lettuce$treatment), 
                          shoots_perc = with(lettuce, aboveground/totalbiomass),
                          roots_perc = with(lettuce, roots/totalbiomass))
  mass_perc_agg <- doBy::summaryBy(.~treatment, data=mass_perc, FUN = mean)
  mass_perc2 <- mass_perc_agg[,2:3]
  i=c(2,1)
  
#4 panel graph-------
  
windows(12,12)
  
jpeg(filename = "output/brcbio.jpeg",
       width = 6, height = 6, units = "in", res= 500) 
  
par(mfrow=c(2,2))
  
#totalbiomass
  par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
  boxplot(biomass$totalbiomass ~ treatment, data=biomass, xaxt='n',varwidth=TRUE,
          ylab=biomasslab,border=trtcols,ylim=c(0,15),xlab="",outline=FALSE,
          boxlwd=2, whisklwd=2,staplelwd=2)
  axis(1, boxlabs, at=1:2, cex=1.1)
  stripchart(biomass$totalbiomass ~ treatment, data = biomass,
             vertical = TRUE, method = "jitter",cex=1.25,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
  text(2.25, 14.5, expression(paste(italic(P)," < "," 0.001")))
  text(.5, 14.5, "A",cex=1.51)

  #allocation
  par(mar = c(4, 4, 1, 3), xpd = TRUE,cex.lab=1.1)
  barplot(t(as.matrix(mass_perc2))[i,], names.arg=boxlabs, col=plantcols, width=2, xlab= "", 
          ylab="Biomass Partitioning (%)", ylim=c(0, 1), xaxt = 'n')
  box()
  axis(1, boxlabs, at=c(1.35, 3.85))
  legend("topright", inset = c(-0.25, 0), fill = c(plantcols[2],plantcols[1]),
         legend=treelab, cex=.5)
  text(.6 ,.94, "B", cex=1.51)

#A - gs  
  par(mgp=c(2.25,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
  plot(A ~ gsw, data=lettuce_gas, col=treatment, ylab=satlab, xlab=condlab, ylim=c(0,20),
       xlim=c(0,1), type='n')
  points(A ~ gsw, data=lettuce_gas, bg=trtcols2[treatment], pch=21, cex=1.5)
  predline(ags_mod_simp_aqua, col=trtcols[1],lwd=2, lty=2)
  predline(ags_mod_simp_soil, col=trtcols[2],lwd=2, lty=2)
  text(0 ,19, "C", cex=1.51)
  text(.75, 7, expression(paste(R^{"2"}," = "," 0.63","  ", italic(P)," < "," 0.001")),
       col=trtcols[1], cex=.8)
  text(.75, 5, expression(paste(R^{"2"}," = "," 0.70","  ", italic(P)," < "," 0.001")),
       col=trtcols[2], cex=.8)
  legend("bottomright", boxlabs, pch=21, pt.bg = trtcols, inset=0.02, 
         title="", bty='n', cex=.8) 
  
#WUE  
  par(mgp=c(2.25,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
  boxplot(ITE ~ treatment, data=lettuce_gas, xaxt='n',varwidth=TRUE,
          ylab=itelab,border=trtcols,ylim=c(0,14),xlab="",outline=FALSE,
          boxlwd=2, whisklwd=2,staplelwd=2)
  axis(1, boxlabs, at=1:2, cex=1.25)
  stripchart(ITE ~ treatment, data=lettuce_gas,
             vertical = TRUE, method = "jitter",cex=1.25,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
  text(0.5 ,13.5, "D", cex=1.51)
dev.off()
