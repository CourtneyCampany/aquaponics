source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$species <- as.factor(gasex$species)
  gasex$trial <- as.factor(gasex$trial)
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$ITE <- with(gasex, A/(E*1000))
  gasex$uniqueid <- paste(gasex$species, gasex$treatment, sep="-")

element <- read.csv("raw_data/elemental_leaves.csv")
  element$species <- as.factor(element$species)
  element$trial <- as.factor(element$trial)
  element$treatment <- as.factor(element$treatment)
  element$uniqueid <- paste(element$species, element$treatment, sep="-")


#stomatal density data
sd <- read.csv("raw_data/stomatadensity.csv")
  sd$date <- as.Date(sd$date, format = "%m/%d/%Y")
  #calculate density from FOV diameter
  sd$sd_mm2 <- with(sd, (stomata_count/(3.14 * (fov_mm/2)^2)))

sd_plant <- doBy::summaryBy(sd_mm2 ~ treatment + species + plant + week, 
                            data =sd, FUN=mean, keep.names=TRUE)
sd_plant$uniqueid <- paste(sd_plant$species, sd_plant$treatment, sep="-")

#3 panel graph of WUE traits
jpeg(filename = "output/Figure6.jpeg", width = 6, height = 12, units = "in", res= 500)
par(mfrow=c(3,1),mgp=c(2.5,.75,0), cex.lab=1.25,  cex.axis = 1.25)

#ITE
par(mar=c(0,5,1,1))
boxplot(gasex$ITE ~ uniqueid, data=gasex, xaxt='n',varwidth=TRUE,
        ylab=itelab,border=trtcols,ylim=c(0,16.5),xlab="",outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
stripchart(gasex$ITE ~ uniqueid, data = gasex,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
axis(1, labels=FALSE)
text(1.5, 7.5, "*",cex=2, font=2)
text(3.5, 16, "*",cex=2, font=2)
text(5.5, 13, "*",cex=2, font=2)
text(.5, 16.5, "A",cex=1.5, font=2)

###13C
par(mar=c(0,5,0,1))

boxplot(c13 ~ uniqueid, data=element, xaxt='n',varwidth=TRUE,
        ylab=c13lab,border=trtcols,ylim=c(-40,-28),xlab="",outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)

stripchart(c13 ~ uniqueid, data = element,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(1.5, -36, "*",cex=2.5, font=2)
text(3.5, -31, "*",cex=2.5, font=2)
text(.5, -28, "B",cex=1.5, font=2)
axis(1, labels=FALSE)

##SD
par(mar=c(5,5,0,1))
boxplot(sd_plant$sd_mm2 ~ uniqueid, data=sd_plant, xaxt='n',varwidth=TRUE,
        ylab=denslab,border=trtcols,ylim=c(0,800),xlab="",outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, boxlabs2, at=1:6)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=.8)
stripchart(sd_plant$sd_mm2 ~ uniqueid, data = sd_plant,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(1.5, 525, "*",cex=2.5, font=2)
text(5.5, 175, "*",cex=2.5, font=2)
text(.5, 800, "C",cex=1.5, font=2)

dev.off()
