biomass <- read.csv("raw_data/leafmassfinal.csv")

biomass$aboveground <- with(biomass, shoots + leafexcess_week1+ leafexcess_week2 + 
                              leafexcess_week3 + leafexcess_week4)

biomass$totalbiomass <- with(biomass, aboveground + roots)

biomass$rs_ratio <- with(biomass, roots/aboveground)

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

lettuce_ttest_above <- t.test(lettuceaqua$aboveground, lettucesoil$aoveground, 
                              alternative = "two.sided")

lettuce_ttest_ratio <- t.test(lettuceaqua$rs_ratio, lettucesoil$rs_ratio, 
                              alternative = "two.sided")

##all variables are different, including root shoot ratio

lettuce <- biomass[biomass$species == "S",]
mean(lettuceaqua$rs_ratio)
mean(lettucesoil$rs_ratio)

#lettuce root to shoot
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(lettuce$rs_ratio ~ treatment, data=lettuce, xaxt='n',varwidth=TRUE,
        ylab=ratio_lab,border=trtcols,ylim=c(0,.8),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:2, cex=1.1)
stripchart(lettuce$rs_ratio ~ treatment, data = lettuce,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(.65, .7, "Salanova")

#lettuce totalbiomass
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(biomass$totalbiomass ~ treatment, data=biomass, xaxt='n',varwidth=TRUE,
        ylab=biomasslab,border=trtcols,ylim=c(0,15),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:2, cex=1.1)
stripchart(biomass$totalbiomass ~ treatment, data = biomass,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(2.25, 14, "p<0.0001")
text(.65, 14, "Salanova")

#pac choy stats and graphs ----------
pacaqua <- biomass[biomass$species == "P" & biomass$treatment == "A",]
pacsoil <- biomass[biomass$species == "P" & biomass$treatment == "C",]

pac_ttest <- t.test(pacaqua$totalbiomass, pacsoil$totalbiomass, 
                        alternative = "two.sided")

pac_ttest_roots <- t.test(pacaqua$roots, pacsoil$roots, 
                              alternative = "two.sided")

pac_ttest_above <- t.test(pacaqua$aboveground, pacsoil$aoveground, 
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

broc_ttest_above <- t.test(brocaqua$aboveground, brocsoil$aoveground, 
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