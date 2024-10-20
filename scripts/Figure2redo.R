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
# mass_perc <- data.frame(treatment=as.factor(biomass$treatment), 
#                          species=as.factor(biomass$species),
#                           shoots_perc = with(biomass, aboveground/totalbiomass),
#                           roots_perc = with(biomass, roots/totalbiomass))
#   mass_perc_agg <- doBy::summaryBy(.~treatment+species, data=mass_perc, FUN = mean)
#   mass_perc2 <- mass_perc_agg[,3:4]
#   i=c(2,1)


#for paper, combined into 2 panel graph
jpeg(filename = "output/Figure2redo.jpeg",
       width = 8, height = 10, units = "in", res= 500)

par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25, mfrow=c(2,1),  
      omi=c(.5,0,0.1,0.1))

#totalbiomass by species------
par(mar=c(0,5,1,1))
boxplot(biomass$totalbiomass ~ uniqueid, data=biomass, xaxt='n',varwidth=TRUE,
        ylab="",border=trtcols,ylim=c(0,15.5),xlab="", outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
# axis(1, boxlabs2, at=1:6)
axis(2, "Total Biomass  (g)" , at=7.5, tick=FALSE, line=2)
# mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
stripchart(biomass$totalbiomass ~ uniqueid, data = biomass,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

text(1.5, 10, "*",cex=2.5, font=2)
text(3.5, 14.5, "*",cex=2.5, font=2)
text(5.5, 8.5, "*",cex=2.5, font=2)
text(.55, 15, "A", cex=1.51, font=2)
# RS ratio
par(mar=c(5,5,0,1))
boxplot(biomass$rs_ratio ~ uniqueid, data=biomass, xaxt='n',varwidth=TRUE,
        ylab="",border=trtcols,ylim=c(0,.65),xlab="", outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, boxlabs2, at=1:6)
axis(2, "Root:Shoot Ratio  (%)" , at=.325, tick=FALSE, line=2)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
stripchart(biomass$rs_ratio ~ uniqueid, data = biomass,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

text(1.5, .4, "*",cex=2.5, font=2)
text(5.5, .6, "*",cex=2.5, font=2)
text(.55, .625, "B", cex=1.51, font=2)

dev.off()

#biomasspartioning
# jpeg(filename = "output/shootroot.jpeg",
#      width = 8, height = 6, units = "in", res= 500)
# par(mar = c(5, 5, 1, 6), xpd = TRUE)
# barplot(t(as.matrix(mass_perc2))[i,], names.arg=boxlabs2, col=c("chocolate3", "forestgreen"),
#         width=2, xlab= "", 
#         ylab="Biomass Partitioning (%)", ylim=c(0, 1), xaxt = 'n')
# box()
# axis(1, boxlabs2, at=c(1.4, 3.8, 6.2 ,8.6, 11, 13.4))
# mtext(specieslabs, 1, line=2.25, at=c(2.6, 7.4, 12.2),  cex=1.25)
# legend("topright", inset = c(-0.2, 0), fill = c("forestgreen", "chocolate3"), 
#        legend=partlab, cex=1)
# text(x=1.3, .94, "(B)", cex=1.51)
# dev.off()


