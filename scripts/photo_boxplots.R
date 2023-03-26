source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
gasex$species <- as.factor(gasex$species)
gasex$trial <- as.factor(gasex$trial)
gasex$treatment <- as.factor(gasex$treatment)
gasex$ITE <- with(gasex, A/(E*1000))
gasex$uniqueid <- paste(gasex$species, gasex$treatment, sep="-")

treatmentlab <- c("Aqua", "Soil", "Aqua", "Soil", "Aqua", "Soil")

### 3 panel physiology graph --------
jpeg(filename = "output/photosynthesis.jpeg",
     width = 8, height = 6, units = "in", res= 500)
par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25,mar=c(5,5,1,1))

#photosynthesis
boxplot(gasex$A ~ uniqueid, data=gasex, xaxt='n',varwidth=TRUE,
        ylab=photolab,border=trtcols,ylim=c(0,27),xlab="",outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
stripchart(gasex$A ~ uniqueid, data = gasex,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
axis(1, boxlabs2, at=1:6)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
text(1.5, 21, "*",cex=2.5, font=2)
text(3.5, 26, "*",cex=2.5, font=2)
text(5.5, 17.5, "*",cex=2.5, font=2)

dev.off()
