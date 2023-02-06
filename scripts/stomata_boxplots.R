source("scripts/functions.R")
source("scripts/plot_objects.R")

#stomatal density data
sd <- read.csv("raw_data/stomatadensity.csv")
  sd$date <- as.Date(sd$date, format = "%m/%d/%Y")
  #calculate density from FOV diameter
  sd$sd_mm2 <- with(sd, (stomata_count/(3.14 * (fov_mm/2)^2)))
  
sd_plant <- doBy::summaryBy(sd_mm2 ~ treatment + species + plant + week, 
                            data =sd, FUN=mean, keep.names=TRUE)
    sd_plant$uniqueid <- paste(sd_plant$species, sd_plant$treatment, sep="-")

#stomatal size data
ss <- read.csv("raw_data/stomatal_size.csv")
  ss$date <- as.Date(ss$date, format = "%m/%d/%Y")
  #calculate stomatal size
  ss$guardcell_width <- with(ss, (gc_width_1 + gc_width_2))
  ss$stomatasize_mm2 <- with(ss, guardcell_width*guardcell_length)
    
    
ss_plant <- doBy::summaryBy(stomatasize_mm2 +guardcell_width + guardcell_length ~ 
                            treatment + species + plant + week, 
                            data =ss, FUN=mean2, keep.names=TRUE)
  ss_plant$uniqueid <- paste(ss_plant$species, ss_plant$treatment, sep="-")
                           
# stomatal density
jpeg(filename = "output/stomatadensity.jpeg",
         width = 8, height = 6, units = "in", res= 500)
par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25,mar=c(5,5,1,1))
  boxplot(sd_plant$sd_mm2 ~ uniqueid, data=sd_plant, xaxt='n',varwidth=TRUE,
          ylab=denslab,border=trtcols,ylim=c(0,800),xlab="",outline=FALSE,
          boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
  axis(1, boxlabs2, at=1:6)
  mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
  stripchart(sd_plant$sd_mm2 ~ uniqueid, data = sd_plant,
             vertical = TRUE, method = "jitter",cex=1.5,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
  
  text(1.5, 525, "*",cex=2.5, font=2)
  text(5.5, 175, "*",cex=2.5, font=2)
  
  dev.off()
  
# stomatal size
jpeg(filename = "output/stomatasize.jpeg", width = 8, height = 6, units = "in", res= 500)
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1, font.lab = 2)
  boxplot(ss_plant$stomatasize_mm2 ~ uniqueid, data=ss_plant, xaxt='n',varwidth=TRUE,
          ylab=sizelab,border=trtcols,ylim=c(0,5.5),xlab="",outline=FALSE,
          boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
  axis(1, boxlabs2, at=1:6, cex=1.1, font=2)
  mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), font=2, cex=1.1)
  stripchart(ss_plant$stomatasize_mm2 ~ uniqueid, data = ss_plant,
             vertical = TRUE, method = "jitter",cex=1.5,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
  
  text(1.5, 2.25, "*",cex=2, font=2)
  dev.off()