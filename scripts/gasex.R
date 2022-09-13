source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
        gasex$species <- as.factor(gasex$species)
        gasex$trial <- as.factor(gasex$trial)
        gasex$treatment <- as.factor(gasex$treatment)
        gasex$ITE <- with(gasex, A/(E*1000))

#extract lettuce ---------
lettuceaqua <- gasex[gasex$species == "S" & gasex$treatment == "a",]
lettucesoil <- gasex[gasex$species == "S" & gasex$treatment == "c",]

lettuce <- gasex[gasex$species == "S",]

mean(lettuceaqua$ITE) #3.917
mean(lettucesoil$ITE) #5.951

mean(lettuceaqua$A)#10.85
mean(lettucesoil$A)#4.33

mean(lettuceaqua$gsw) #.3867
mean(lettucesoil$gsw) #0.0847
threadr::percentage_difference (5.91, 3.91)

#lettuce graphs ----------
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(A ~ treatment, data=lettuce, xaxt='n',varwidth=TRUE,
        ylab=satlab,border=trtcols,ylim=c(0,20),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:2, cex=1.1)
stripchart(A ~ treatment, data=lettuce,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(.65, 19, "Salanova")

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(A ~ treatment, data=lettuce, xaxt='n',varwidth=TRUE,
        ylab=satlab,border=trtcols,ylim=c(0,20),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:2, cex=1.1)
stripchart(A ~ treatment, data=lettuce,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(.65, 19, "Salanova")

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(A ~ treatment, data=lettuce, xaxt='n',varwidth=TRUE,
        ylab=satlab,border=trtcols,ylim=c(0,20),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:2, cex=1.1)
stripchart(A ~ treatment, data=lettuce,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(.65, 19, "Salanova")

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(gsw ~ treatment, data=lettuce, xaxt='n',varwidth=TRUE,
        ylab=satlab,border=trtcols,ylim=c(0,1),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:2, cex=1.1)
stripchart(gsw ~ treatment, data=lettuce,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(.65, .9, "Salanova")

#bivariateplpots
ags_mod_simp_aqua <- lm(A ~ gsw, data=lettuceaqua)
ags_mod_simp_soil <- lm(A ~ gsw, data=lettucesoil)

windows(12,6)
par(mfrow=c(1,2))

par(mgp=c(2.25,.75,0), mar=c(4,4,1,1), cex.lab=1.1, font.lab = 2)
plot(A ~ gsw, data=lettuce, col=treatment, ylab=satlab, xlab=condlab, ylim=c(0,20),
     xlim=c(0,1), type='n')
        points(A ~ gsw, data=lettuce, bg=trtcols2[treatment], pch=21, cex=1.5)
        predline(ags_mod_simp_aqua, col=trtcols[1],lwd=2, lty=2)
        predline(ags_mod_simp_soil, col=trtcols[2],lwd=2, lty=2)
        
text(.25, 19.5, expression(paste(R^{"2"}," = "," 0.63","  ", italic(P)," < "," 0.001")),
     col=trtcols[1])
text(.25, 17.5, expression(paste(R^{"2"}," = "," 0.70","  ", italic(P)," < "," 0.001")),
     col=trtcols[2])
legend("bottomright", boxlabs, pch=21, pt.bg = trtcols, inset=0.02, 
       title="", bty='n') 

par(mgp=c(2.25,.75,0), mar=c(4,4,1,1), cex.lab=1.1, font.lab = 2)
boxplot(ITE ~ treatment, data=lettuce, xaxt='n',varwidth=TRUE,
        ylab=itelab,border=trtcols,ylim=c(0,14),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:2, cex=1.25, font=2)
stripchart(ITE ~ treatment, data=lettuce,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

#t-tests-------------------

#photosynthesis = not different
t.test(pretreat_agg[pretreat_agg$treatment == "control", "A"],
       pretreat_agg[pretreat_agg$treatment == "herb25perc", "A"],
       alternative = "two.sided", var.equal = FALSE)

#stomatal conductance = not different
t.test(pretreat_agg[pretreat_agg$treatment == "control", "gsw"],
       pretreat_agg[pretreat_agg$treatment == "herb25perc", "gsw"],
       alternative = "two.sided", var.equal = FALSE)

#transpiration = not different = not different
t.test(pretreat_agg[pretreat_agg$treatment == "control", "E"],
       pretreat_agg[pretreat_agg$treatment == "herb25perc", "E"],
       alternative = "two.sided", var.equal = FALSE)

#Ci = not different = not different
t.test(pretreat_agg[pretreat_agg$treatment == "control", "Ci"],
       pretreat_agg[pretreat_agg$treatment == "herb25perc", "Ci"],
       alternative = "two.sided", var.equal = FALSE)
