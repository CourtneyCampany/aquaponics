source("scripts/functions.R")
source("scripts/plot_objects.R")

sd <- read.csv("raw_data/stomata_density.csv")
  sd$date <- as.Date(sd$date, format = "%m/%d/%Y")
  #calculate density from FOV diameter
  sd$sd_mm2 <- with(sd, (stomata_count/(3.14 * (fov_diam_mm/2)^2)))
                    
sd_plant <- doBy::summaryBy(sd_mm2 ~ treatment + plant + date + week, data =sd, FUN=mean, 
                           keep.names=TRUE)

sd_agg <- doBy::summaryBy(sd_mm2 ~ treatment +  week, data =sd, FUN=c(mean,se)) 
    sd_agg$treatment <- as.factor(sd_agg$treatment)

aqua <- sd_agg[sd_agg$treatment == "a",]
soil <- sd_agg[sd_agg$treatment == "c",]


jpeg(filename = "output/lettuce_stodens.jpeg",
     width = 6, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
plot(sd_mm2.mean ~ week, data=sd_agg, xlab="", ylab=denslab, xaxt='n', type='n')
  axis(1, at=c(1,2,3), labels=c("Week 1", "Week 2", "Week3"))
with(aqua, points(week, sd_mm2.mean,  pch=21,  cex=1.6,bg=trtcols[1]))
with(soil, points(week, sd_mm2.mean,  pch=21,  cex=1.6,bg=trtcols[2]))  

with(aqua, arrows(week, sd_mm2.mean, week, sd_mm2.mean+sd_mm2.se, angle=90, col=trtcols[1],
                    length=0.05))
with(aqua, arrows(week, sd_mm2.mean, week, sd_mm2.mean-sd_mm2.se, angle=90, col=trtcols[1],
                    length=0.05))

with(soil, arrows(week, sd_mm2.mean, week, sd_mm2.mean+sd_mm2.se, angle=90, col=trtcols[2],
                    length=0.05))
with(soil, arrows(week, sd_mm2.mean, week, sd_mm2.mean-sd_mm2.se, angle=90, col=trtcols[2],
                    length=0.05))
legend("topleft", boxlabs, pch=21, pt.bg = trtcols, inset=0.02, 
       title="", bty='n') 

dev.off()
