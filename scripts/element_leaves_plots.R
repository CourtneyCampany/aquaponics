source("scripts/functions.R")
source("scripts/plot_objects.R")

element <- read.csv("raw_data/elemental_leaves.csv")
  element$species <- as.factor(element$species)
  element$trial <- as.factor(element$trial)
  element$treatment <- as.factor(element$treatment)
  element$cn_ratio <- with(element, c_perc/n_perc)
  element$uniqueid <- paste(element$species, element$treatment, sep="-")
  element$nmass <- element$n_perc/100
  
#boxplot leaf nitro --------

jpeg(filename = "output/foliar_nitrogen.jpeg",
       width = 8, height = 6, units = "in", res= 500)
par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25,mar=c(5,5,1,1))
  
boxplot(nmass ~ uniqueid, data=element, xaxt='n',varwidth=TRUE,
          ylab=nitrolab,border=trtcols,ylim=c(0,.1),xlab="",outline=FALSE,
          boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)

stripchart(nmass ~ uniqueid, data = element,
             vertical = TRUE, method = "jitter",cex=1.5,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
axis(1, boxlabs2, at=1:6)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
text(1.5, .075, "*",cex=2.5, font=2)
text(3.5, .09, "*",cex=2.5, font=2)
text(5.5, .0925, "*",cex=2.5, font=2)
  
dev.off()

#boxplot 13c --------

jpeg(filename = "output/carbon13.jpeg",
     width = 8, height = 6, units = "in", res= 500)
par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25,mar=c(5,5,1,1))

boxplot(c13 ~ uniqueid, data=element, xaxt='n',varwidth=TRUE,
        ylab=c13lab,border=trtcols,ylim=c(-40,-28),xlab="",outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)

stripchart(c13 ~ uniqueid, data = element,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

axis(1, boxlabs2, at=1:6)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
text(1.5, -36, "*",cex=2.5, font=2)
text(3.5, -31, "*",cex=2.5, font=2)

dev.off()
