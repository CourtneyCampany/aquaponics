biomass <- read.csv("raw_data/leafmassfinal.csv")

  biomass$aboveground <- with(biomass, shoots + leafexcess_week1+ leafexcess_week2 + 
                                leafexcess_week3 + leafexcess_week4)
  
  biomass$totalbiomass <- with(biomass, aboveground + roots)
  
  biomass$rs_ratio <- with(biomass, roots/aboveground)
  biomass$uniqueid <- paste(biomass$species, biomass$treatment, sep="-")
  
#plot objects ---------
wp1 <- wesanderson::wes_palette("Darjeeling2") 
wp2 <- wesanderson::wes_palette("Cavalcanti1")
  
trtcols <- c(wp1[2], wp2[5])
# trtcols <- c("navy", "olivedrab")
  
trtcols2 <- scales::alpha(trtcols, .6)
ratio_lab <- "Root to Shoot ratio"
biomasslab <- "Total Biomass (g)"
boxlabs <- c("Aquaponics", "Soil")
boxlabs2 <- rep(boxlabs, 3)
boxlabs3 <- c("Aqua", "Soil")
boxlabs4 <- rep(boxlabs3, 3)
specieslabs <- c("Salanova", "PacChoy", "Broccoli")
partlab <- c("shoots", "roots")

jpeg(filename = "output/totalbiomass.jpeg",
     width = 8, height = 6, units = "in", res= 500)

#totalbiomass by species------
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1, font.lab = 2)
boxplot(biomass$totalbiomass ~ uniqueid, data=biomass, xaxt='n',varwidth=TRUE,
        ylab=biomasslab,border=trtcols,ylim=c(0,15),xlab="",outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, boxlabs2, at=1:6, cex=1.1, font=2)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), font=2, cex=1.1)
stripchart(biomass$totalbiomass ~ uniqueid, data = biomass,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

text(1.5, 10, "*",cex=2, font=2)
text(3.5, 14.5, "*",cex=2, font=2)
text(5.5, 8.5, "*",cex=2, font=2)
# text(1, 14.5, expression(paste('* = ', italic(P)," < "," 0.001")))

dev.off()

#biomasspartioning

mass_perc <- data.frame(treatment=as.factor(biomass$treatment), 
                        species=as.factor(biomass$species),
                        shoots_perc = with(biomass, aboveground/totalbiomass),
                        roots_perc = with(biomass, roots/totalbiomass))
mass_perc_agg <- doBy::summaryBy(.~treatment+species, data=mass_perc, FUN = mean)
mass_perc2 <- mass_perc_agg[,3:4]
i=c(2,1)


jpeg(filename = "output/shootroot.jpeg",
     width = 6, height = 6, units = "in", res= 500)
par(mar = c(4, 4, 1, 7.3), xpd = TRUE,cex.lab=1.1, font.lab = 2)
barplot(t(as.matrix(mass_perc2))[i,], names.arg=boxlabs2, col=c("forestgreen","brown"), width=2, xlab= "", 
        ylab="Biomass Partitioning (%)", ylim=c(0, 1), xaxt = 'n')
box()
axis(1, boxlabs4, at=c(1.4, 3.8, 6.2 ,8.6, 11, 13.4), font=2)
mtext(specieslabs, 1, line=2.25, at=c(2.6, 7.4, 12.2), font=2, cex=1.1)
legend("topright", inset = c(-0.305, 0), fill = c("forestgreen","brown"), legend=partlab, cex=1)

dev.off()
