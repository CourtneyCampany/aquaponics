source("scripts/functions.R")
source("scripts/plot_objects.R")

element <- read.csv("raw_data/elemental_roots.csv")
  element$species <- as.factor(element$species)
  element$trial <- as.factor(element$trial)
  element$treatment <- as.factor(element$treatment)
  element$uniqueid <- paste(element$species, element$treatment, sep="-")
  element$nmass <- element$n_perc/100

  
#boxplot leaf nitro --------
  
jpeg(filename = "output/root_nitrogen.jpeg",
       width = 8, height = 6, units = "in", res= 500)
par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25,mar=c(5,5,1,1))
  
boxplot(nmass ~ uniqueid, data=element, xaxt='n',varwidth=TRUE,
          ylab=rootnitrolab,border=trtcols,ylim=c(0,.08),xlab="",outline=FALSE,
          boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
  
stripchart(nmass ~ uniqueid, data = element,
             vertical = TRUE, method = "jitter",cex=1.5,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
axis(1, boxlabs2, at=1:6)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
text(1.5, .06, "*",cex=2.5, font=2)
text(3.5, .06, "*",cex=2.5, font=2)
text(5.5, .06, "*",cex=2.5, font=2)
  
dev.off()
  
#boxplot root cn --------
  
jpeg(filename = "output/root_cn_ratio.jpeg",
       width = 8, height = 6, units = "in", res= 500)
par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25,mar=c(5,5,1,1))
  
boxplot(cn_ratio ~ uniqueid, data=element, xaxt='n',varwidth=TRUE,
          ylab="Root C:N",border=trtcols,ylim=c(0,70),xlab="",outline=FALSE,
          boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
  
stripchart(cn_ratio ~ uniqueid, data = element,
             vertical = TRUE, method = "jitter",cex=1.5,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
  
axis(1, boxlabs2, at=1:6)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
text(1.5, 37.5, "*",cex=2.5, font=2)
text(3.5, 34, "*",cex=2.5, font=2)
text(5.5, 68, "*",cex=2.5, font=2)
  
dev.off()
  