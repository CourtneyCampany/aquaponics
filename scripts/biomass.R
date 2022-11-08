source("scripts/functions.R")

biomass <- read.csv("raw_data/leafmassfinal.csv")
        biomass$aboveground <- with(biomass, shoots + leafexcess_week1+ leafexcess_week2 + 
                                      leafexcess_week3 + leafexcess_week4)
        
        biomass$totalbiomass <- with(biomass, aboveground + roots)
        biomass$rs_ratio <- with(biomass, roots/aboveground)

biomass_agg <- doBy::summaryBy(aboveground + roots + rs_ratio ~ treatment + species , 
                            data =biomass, FUN=c(mean, sd, se), keep.names=TRUE)


#plot objects ---------
wp1 <- wesanderson::wes_palette("Darjeeling2") 
wp2 <- wesanderson::wes_palette("Cavalcanti1")

trtcols <- c(wp1[2], wp2[5])
# trtcols <- c("navy", "olivedrab")

trtcols2 <- scales::alpha(trtcols, .6)
ratio_lab <- "Root to Shoot ratio"
biomasslab <- "Total Biomass (g)"
boxlabs <- c("Aquaponics", "Containerized")


#lettuce stats and graphs ----------
lettuceaqua <- biomass[biomass$species == "S" & biomass$treatment == "A",]
lettucesoil <- biomass[biomass$species == "S" & biomass$treatment == "C",]

lettuce_ttest <- t.test(lettuceaqua$totalbiomass, lettucesoil$totalbiomass, 
                        alternative = "two.sided")

lettuce_ttest_roots <- t.test(lettuceaqua$roots, lettucesoil$roots, 
                        alternative = "two.sided")

lettuce_ttest_above <- t.test(lettuceaqua$aboveground, lettucesoil$aboveground, 
                              alternative = "two.sided")

lettuce_ttest_ratio <- t.test(lettuceaqua$rs_ratio, lettucesoil$rs_ratio, 
                              alternative = "two.sided")

##all variables are different, including root shoot ratio

lettuce <- biomass[biomass$species == "S",]
mean(lettuceaqua$rs_ratio) #.178
mean(lettucesoil$rs_ratio) #.427
mean(lettuceaqua$totalbiomass) #6.32
mean(lettucesoil$totalbiomass) #1.67

mass_perc <- data.frame(treatment=as.factor(lettuce$treatment), 
                        shoots_perc = with(lettuce, aboveground/totalbiomass),
                        roots_perc = with(lettuce, roots/totalbiomass))
mass_perc_agg <- doBy::summaryBy(.~treatment, data=mass_perc, FUN = mean)
mass_perc2 <- mass_perc_agg[,2:3]
i=c(2,1)


windows(12,6)

jpeg(filename = "output/lettuce_biomass.jpeg",
     width = 12, height = 6, units = "in", res= 500)

par(mfrow=c(1,2))

#lettuce totalbiomass
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1, font.lab = 2)
boxplot(biomass$totalbiomass ~ treatment, data=biomass, xaxt='n',varwidth=TRUE,
        ylab=biomasslab,border=trtcols,ylim=c(0,15),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:2, cex=1.1, font=2)
stripchart(biomass$totalbiomass ~ treatment, data = biomass,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(2.25, 14.5, expression(paste(italic(P)," < "," 0.001")))
text(.5, 14.5, "A",cex=1.51)

par(mar = c(4, 4, 1, 7.3), xpd = TRUE,cex.lab=1.1, font.lab = 2)
barplot(t(as.matrix(mass_perc2))[i,], names.arg=boxlabs, col=plantcols, width=2, xlab= "", 
        ylab="Biomass Partitioning (%)", ylim=c(0, 1), xaxt = 'n')
box()
axis(1, boxlabs, at=c(1.35, 3.85), font=2)
legend("topright", inset = c(-0.305, 0), fill = c(plantcols[2],plantcols[1]), legend=treelab, cex=1)
text(.75 ,.94, "B", cex=1.51)

dev.off()


# #lettuce root to shoot
# par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
# boxplot(lettuce$rs_ratio ~ treatment, data=lettuce, xaxt='n',varwidth=TRUE,
#         ylab=ratio_lab,border=trtcols,ylim=c(0,.8),xlab="",outline=FALSE,
#         boxlwd=2, whisklwd=2,staplelwd=2)
# axis(1, boxlabs, at=1:2, cex=1.1)
# stripchart(lettuce$rs_ratio ~ treatment, data = lettuce,
#            vertical = TRUE, method = "jitter",cex=1.25,
#            pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
# text(.65, .7, "Salanova")


#pac choy stats and graphs ----------
pacaqua <- biomass[biomass$species == "P" & biomass$treatment == "A",]
pacsoil <- biomass[biomass$species == "P" & biomass$treatment == "C",]

pac_ttest <- t.test(pacaqua$totalbiomass, pacsoil$totalbiomass, 
                        alternative = "two.sided")

pac_ttest_roots <- t.test(pacaqua$roots, pacsoil$roots, 
                              alternative = "two.sided")

pac_ttest_above <- t.test(pacaqua$aboveground, pacsoil$aboveground, 
                              alternative = "two.sided")

pac_ttest_ratio <- t.test(pacaqua$rs_ratio, pacsoil$rs_ratio, 
                              alternative = "two.sided")

##root to shoot not different

mean(pacaqua$rs_ratio)
mean(pacsoil$rs_ratio)

pacchoy <- biomass[biomass$species == "P",]
#lettuce root to shoot
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(pacchoy$rs_ratio ~ treatment, data=pacchoy, xaxt='n',varwidth=TRUE,
        ylab=ratio_lab,border=trtcols,ylim=c(0,.4),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:2, cex=1.1)
stripchart(pacchoy$rs_ratio ~ treatment, data = pacchoy,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(.65, .375, "Pac Choy")

#pachoy totalbiomass
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(pacchoy$totalbiomass ~ treatment, data=pacchoy, xaxt='n',varwidth=TRUE,
        ylab=biomasslab,border=trtcols,ylim=c(0,15),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:2, cex=1.1)
stripchart(pacchoy$totalbiomass ~ treatment, data = pacchoy,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(2.25, 14, "p<0.0001")
text(.65, 14, "Pac Choy")

#broccoli stats and graphs ----------
brocaqua <- biomass[biomass$species == "B" & biomass$treatment == "A",]
brocsoil <- biomass[biomass$species == "B" & biomass$treatment == "C",]

broc_ttest <- t.test(brocaqua$totalbiomass, brocsoil$totalbiomass, 
                    alternative = "two.sided")

broc_ttest_roots <- t.test(brocaqua$roots, brocsoil$roots, 
                          alternative = "two.sided")

broc_ttest_above <- t.test(brocaqua$aboveground, brocsoil$aboveground, 
                          alternative = "two.sided")

broc_ttest_ratio <- t.test(brocaqua$rs_ratio, brocsoil$rs_ratio, 
                          alternative = "two.sided")

##root to shoot not different

mean(brocaqua$rs_ratio)
mean(brocsoil$rs_ratio)

broccoli <- biomass[biomass$species == "B",]
#lettuce root to shoot
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(broccoli$rs_ratio ~ treatment, data=broccoli, xaxt='n',varwidth=TRUE,
        ylab=ratio_lab,border=trtcols,ylim=c(0,.4),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:2, cex=1.1)
stripchart(broccoli$rs_ratio ~ treatment, data = broccoli,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(.65, .375, "Broccoli")

#lettuce totalbiomass
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(broccoli$totalbiomass ~ treatment, data=broccoli, xaxt='n',varwidth=TRUE,
        ylab=biomasslab,border=trtcols,ylim=c(0,12),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:2, cex=1.1)
stripchart(broccoli$totalbiomass ~ treatment, data = broccoli,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(.65, 11, "Broccoli")


##combined boxplot

