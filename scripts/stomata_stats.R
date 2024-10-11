source("scripts/functions.R")
source("scripts/plot_objects.R")

sd <- read.csv("raw_data/stomatadensity.csv")
  sd$date <- as.Date(sd$date, format = "%m/%d/%Y")
  #calculate density from FOV diameter
  sd$sd_mm2 <- with(sd, (stomata_count/(3.14 * (fov_mm/2)^2)))
                    
sd_plant <- doBy::summaryBy(sd_mm2 ~ treatment + species + plant + week, 
                          data =sd, FUN=mean, keep.names=TRUE)

sd_agg <- doBy::summaryBy(sd_mm2 ~ treatment + species, data =sd_plant, FUN=c(mean,se)) 
    sd_agg$treatment <- as.factor(sd_agg$treatment)
    
    
##data for stats----
lettuceaqua <- sd_plant[sd_plant$species == "S" & sd_plant$treatment == "A",]
lettucesoil <- sd_plant[sd_plant$species == "S" & sd_plant$treatment == "C",]
    
brocaqua <- sd_plant[sd_plant$species == "B" & sd_plant$treatment == "A",]
brocsoil <- sd_plant[sd_plant$species == "B" & sd_plant$treatment == "C",]
    
pacaqua <- sd_plant[sd_plant$species == "P" & sd_plant$treatment == "A",]
pacsoil <- sd_plant[sd_plant$species == "P" & sd_plant$treatment == "C",]

    
##t-test lettuce
lettuce_ttest <- t.test(lettuceaqua$sd_mm2, lettucesoil$sd_mm2, 
                                  alternative = "two.sided") 

broc_ttest <- t.test(brocaqua$sd_mm2, brocsoil$sd_mm2, 
                              alternative = "two.sided")

pac_ttest <- t.test(pacaqua$sd_mm2, pacsoil$sd_mm2, 
                              alternative = "two.sided")
    
mean2(lettuceaqua$sd_mm2) #65.6
mean2(lettucesoil$sd_mm2) #91.5
se(lettuceaqua$sd_mm2) #1.94
se(lettucesoil$sd_mm2) #3.77

mean2(brocaqua$sd_mm2) #269.5
mean2(brocsoil$sd_mm2) #392.0
se(brocaqua$sd_mm2) #2.84
se(brocsoil$sd_mm2) #7.20

mean2(pacaqua$sd_mm2) #451.0
mean2(pacsoil$sd_mm2) #436.4
se(pacaqua$sd_mm2) #15.20
se(pacsoil$sd_mm2) #17.98

aqua <- sd_agg[sd_agg$treatment == "a",]
soil <- sd_agg[sd_agg$treatment == "c",]


# jpeg(filename = "output/lettuce_stodens.jpeg",
#      width = 6, height = 6, units = "in", res= 500)
# 
# par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
# plot(sd_mm2.mean ~ week, data=sd_agg, xlab="", ylab=denslab, xaxt='n', type='n')
#   axis(1, at=c(1,2,3), labels=c("Week 1", "Week 2", "Week3"))
# with(aqua, points(week, sd_mm2.mean,  pch=21,  cex=1.6,bg=trtcols[1]))
# with(soil, points(week, sd_mm2.mean,  pch=21,  cex=1.6,bg=trtcols[2]))  
# 
# with(aqua, arrows(week, sd_mm2.mean, week, sd_mm2.mean+sd_mm2.se, angle=90, col=trtcols[1],
#                     length=0.05))
# with(aqua, arrows(week, sd_mm2.mean, week, sd_mm2.mean-sd_mm2.se, angle=90, col=trtcols[1],
#                     length=0.05))
# 
# with(soil, arrows(week, sd_mm2.mean, week, sd_mm2.mean+sd_mm2.se, angle=90, col=trtcols[2],
#                     length=0.05))
# with(soil, arrows(week, sd_mm2.mean, week, sd_mm2.mean-sd_mm2.se, angle=90, col=trtcols[2],
#                     length=0.05))
# legend("topleft", boxlabs, pch=21, pt.bg = trtcols, inset=0.02, 
#        title="", bty='n') 
# 
# dev.off()
