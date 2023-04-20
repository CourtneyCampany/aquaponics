source("scripts/functions.R")
source("scripts/plot_objects.R")
       
biomass <- read.csv("raw_data/leafmassfinal.csv")

  biomass$aboveground <- with(biomass, shoots + leafexcess_week1+ leafexcess_week2 + 
                                leafexcess_week3 + leafexcess_week4)
  
  biomass$totalbiomass <- with(biomass, aboveground + roots)
  
  biomass$rs_ratio <- with(biomass, roots/aboveground)
  biomass$uniqueid <- paste(biomass$species, biomass$treatment, sep="-")
  biomass$treatment <- as.factor(biomass$treatment)
  biomass$species <- as.factor(biomass$species)
  
#biomass partitioning
mass_perc <- data.frame(treatment=as.factor(biomass$treatment), 
                         species=as.factor(biomass$species),
                          shoots_perc = with(biomass, aboveground/totalbiomass),
                          roots_perc = with(biomass, roots/totalbiomass))
  mass_perc_agg <- doBy::summaryBy(.~treatment+species, data=mass_perc, FUN = mean)
  mass_perc2 <- mass_perc_agg[,3:4]
  i=c(2,1)


#for paper, combined into 2 panel graph
jpeg(filename = "output/Figure2.jpeg",
       width = 12, height = 6, units = "in", res= 500)
# windows(12,6)
par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25, mfrow=c(1,2),  
      omi=c(.5,0,0.1,0.1))

#totalbiomass by species------
# jpeg(filename = "output/totalbiomass.jpeg",
#        width = 8, height = 6, units = "in", res= 500)
par(mar=c(5,5,1,0))
boxplot(biomass$totalbiomass ~ uniqueid, data=biomass, xaxt='n',varwidth=TRUE,
        ylab="",border=trtcols,ylim=c(0,15),xlab="",outline=FALSE, 
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, boxlabs2, at=1:6)
axis(2, biomasslab , at=7.5, tick=FALSE, line=2)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
stripchart(biomass$totalbiomass ~ uniqueid, data = biomass,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

text(1.5, 10, "*",cex=2.5, font=2)
text(3.5, 14.5, "*",cex=2.5, font=2)
text(5.5, 8.5, "*",cex=2.5, font=2)
text(.55, 14.6, "(A)", cex=1.51)
# dev.off()

#biomasspartioning
# jpeg(filename = "output/shootroot.jpeg",
#      width = 8, height = 6, units = "in", res= 500)
par(mar = c(5, 5, 1, 7.3), xpd = TRUE)
barplot(t(as.matrix(mass_perc2))[i,], names.arg=boxlabs2, col=c("chocolate3", "forestgreen"),
        width=2, xlab= "", 
        ylab="Biomass Partitioning (%)", ylim=c(0, 1), xaxt = 'n')
box()
axis(1, boxlabs4, at=c(1.4, 3.8, 6.2 ,8.6, 11, 13.4))
mtext(specieslabs, 1, line=2.25, at=c(2.6, 7.4, 12.2),  cex=1.25)
legend("topright", inset = c(-0.315, 0), fill = c("forestgreen", "chocolate3"), 
       legend=partlab, cex=1)
text(x=1.3, .94, "(B)", cex=1.51)
dev.off()

## panel 3 that accound for plant size. 
# library(smatr)
# 
# RS_mod <- sma(aboveground ~ roots * treatment, log="xy", data=biomass)
# summary(RS_mod)
# 
# RS_broc <- sma(aboveground ~ roots * treatment, log="xy", data=biomass[biomass$species =="B",])
# summary(RS_broc)  #treatment slopes differ
# 
# RS_pak <- sma(aboveground ~ roots * treatment, log="xy", data=biomass[biomass$species =="P",])
# summary(RS_pak)  #treatment slopes differ
# 
# RS_sal <- sma(aboveground ~ roots * treatment, log="xy", data=biomass[biomass$species =="S",])
# summary(RS_sal) #treatment slopes differ



#variables correlated and slopes differe by treatment

# par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25,mar=c(5,5,1,1))
# plot(RS_mod, xlab="Roots  (g)" , ylab="Shoots  (g)", col=trtcols, pch=pchs,cex=1.25, lwd=2)
   
# legend("topleft", legend = specieslabs, pch=pchs, col="black", bty="n", inset=.02)
# legend("bottomright", boxlabs, pch=16, col=trtcols, inset=0.02,  title="", bty='n')
# text(x=7.1, 62.5, "(b)", cex=1.51)
# 
# library(magicaxis)
# library(plotrix)
# 
# lettuceA <- biomass[biomass$species == "S"& biomass$treatment =="A",]
# brocA <- biomass[biomass$species == "B" & biomass$treatment =="A",]
# pacA <- biomass[biomass$species == "P"& biomass$treatment =="A",]
# 
# lettuceS <- biomass[biomass$species == "S" & biomass$treatment =="C",]
# brocS <- biomass[biomass$species == "B" & biomass$treatment =="C",]
# pacS <- biomass[biomass$species == "P" & biomass$treatment =="C",]
# 
# ratio_broc_aqua <- lm(log10(aboveground) ~ log10(roots), data=brocA)
# ratio_broc_soil <- lm(log10(aboveground) ~ log10(roots), data=brocS)
# 
# ratio_pak_aqua <- lm(log10(aboveground) ~ log10(roots), data=pacA)
# ratio_pak_soil <- lm(log10(aboveground) ~ log10(roots), data=pacS)
# 
# ratio_sal_aqua <- lm(log10(aboveground) ~ log10(roots), data=lettuceA)
# ratio_sal_soil <- lm(log10(aboveground) ~ log10(roots), data=lettuceS)
# 
# 
# par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25,mar=c(5,5,1,1))
# with(biomass, plot(log10(roots), log10(aboveground),col=trtcols[treatment],
#                      xlab = "Root Mass  (g)",
#                       ylim=c(-1.5,1.5), xlim=c(-1.5,1.5),
#                      ylab = "Shoot Mass  (g)",axes=FALSE, type='n'))
# abline(0,1, lwd=2, lty=2, col="grey35")
# ablineclip(ratio_broc_aqua , lwd=2, col=trtcols[1], x1=min(log10(brocA$root)), 
#                                           x2=max(log10(brocA$root)))
# ablineclip(ratio_broc_soil , lwd=2, col=trtcols[2], x1=min(log10(brocS$root)), 
#            x2=max(log10(brocS$root)))
# 
# ablineclip(ratio_pak_aqua, lwd=2, col=trtcols[1], x1=min(log10(pacA$root)), 
#            x2=max(log10(pacA$root)))
# ablineclip(ratio_pak_soil, lwd=2, col=trtcols[2], x1=min(log10(pacS$root)), 
#            x2=max(log10(pacS$root)))
# 
# ablineclip(ratio_sal_aqua, lwd=2, col=trtcols[1], x1=min(log10(lettuceA$root)), 
#            x2=max(log10(lettuceA$root)))
# ablineclip(ratio_sal_soil, lwd=2, col=trtcols[2], x1=min(log10(lettuceS$root)), 
#            x2=max(log10(lettuceS$root)))
# 
# 
# with(biomass, points(log10(roots), log10(aboveground), cex=1.25, 
#                      pch=pchs[treatment],col=trtcols2[species]))
# 
# magaxis(side=c(1,2), unlog='xy', frame.plot=TRUE)


