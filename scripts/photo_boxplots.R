#read in functions that we need and all plot labels and colors
source("scripts/functions.R")
source("scripts/plot_objects.R")

# read in gas exchange data
gasex <- read.csv("raw_data/gasexchange_master.csv")
# make variables into factors
  gasex$species <- as.factor(gasex$species)
  gasex$trial <- as.factor(gasex$trial)
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$uniqueid <- paste(gasex$species, gasex$treatment, sep="-")

### Photosynthesis graph --------
jpeg(filename = "output/photosynthesis.jpeg",
     width = 8, height = 6, units = "in", res= 500)
par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25,mar=c(5,5,1,1))

#make photosynthesis barplot
boxplot(gasex$A ~ uniqueid, data=gasex, xaxt='n',varwidth=TRUE,
        ylab=photolab,border=trtcols,ylim=c(0,27),xlab="",outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
#add data points on top of box plots
stripchart(gasex$A ~ uniqueid, data = gasex,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
#jitter add scatter to the points
#add custom x labels and significance asterisk
axis(1, boxlabs2, at=1:6)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
text(1.5, 21, "*",cex=2.5, font=2)
text(3.5, 26, "*",cex=2.5, font=2)
text(5.5, 17.5, "*",cex=2.5, font=2)

dev.off()
