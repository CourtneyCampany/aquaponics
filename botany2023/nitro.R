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

jpeg(filename = "botany2023/rootnitro.jpeg",
       width = 5.75, height = 5.75, units = "in", res= 500)
par(mgp=c(2.5,.75,0), cex.lab=1,cex.axis=1)
  
par(mar=c(4,4,1,1))
boxplot(nmass ~ uniqueid, data=element, xaxt='n',varwidth=TRUE,
        ylab=nitrolab,border=trtcols,ylim=c(0,.1),xlab="",outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, boxlabs2, at=1:6)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1)
stripchart(nmass ~ uniqueid, data = element,
            vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)


text(1.5, .075, "*",cex=2.5, font=2)
text(3.5, .09, "*",cex=2.5, font=2)
text(5.5, .0925, "*",cex=2.5, font=2)

dev.off()

#shoot N
par(mar=c(5,5,0,1))
boxplot(cn_ratio ~ uniqueid, data=element, xaxt='n',varwidth=TRUE,
        ylab="Shoot C:N ratio",border=trtcols,ylim=c(0,65),xlab="",outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)

# stripchart(cn_ratio ~ uniqueid, data = element,
#            vertical = TRUE, method = "jitter",cex=1.5,
#            pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
axis(1, boxlabs2, at=1:6)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
text(1.5, 50, "*",cex=2.5, font=2)
text(3.5, 55, "*",cex=2.5, font=2)
text(5.5, 55, "*",cex=2.5, font=2)
text(.5, 63, "B",cex=1.5, font=2)
dev.off()
