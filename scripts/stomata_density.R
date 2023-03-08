source("scripts/functions.R")
source("scripts/plot_objects.R")

sd <- read.csv("raw_data/stomatadensity.csv")
  sd$date <- as.Date(sd$date, format = "%m/%d/%Y")
  #calculate density from FOV diameter
  sd$sd_mm2 <- with(sd, (stomata_count/(3.14 * (fov_mm/2)^2)))
  
sd_noweek4 <- sd[sd$week==1:3,]
                    
# sd_plant <- doBy::summaryBy(sd_mm2 ~ treatment + species + plant + week, 
#                           data =sd, FUN=mean, keep.names=TRUE)

sd_week <- doBy::summaryBy(sd_mm2 ~ treatment + species + week, data =sd_noweek4, FUN=c(mean,se)) 
  sd_week$treatment <- as.factor(sd_week$treatment)    
   
    
##data by species and treatment
lettuceaqua <- sd_week[sd_week$species == "S" & sd_week$treatment == "A",]
lettucesoil <- sd_week[sd_week$species == "S" & sd_week$treatment == "C",]
    
brocaqua <- sd_week[sd_week$species == "B" & sd_week$treatment == "A",]
brocsoil <- sd_week[sd_week$species == "B" & sd_week$treatment == "C",]
    
pacaqua <- sd_week[sd_week$species == "P" & sd_week$treatment == "A",]
pacsoil <- sd_week[sd_week$species == "P" & sd_week$treatment == "C",]
    
# ##t-test lettuce
# lettuce_ttest <- t.test(lettuceaqua$sd_mm2, lettucesoil$sd_mm2, 
#                                   alternative = "two.sided") 
# 
# broc_ttest <- t.test(brocaqua$sd_mm2, brocsoil$sd_mm2, 
#                               alternative = "two.sided")
# 
# pac_ttest <- t.test(pacaqua$sd_mm2, pacsoil$sd_mm2, 
#                               alternative = "two.sided")
    

# aqua <- sd_agg[sd_agg$treatment == "a",]
# soil <- sd_agg[sd_agg$treatment == "c",]


jpeg(filename = "output/stodens_week.jpeg",
     width = 6, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
plot(sd_mm2.mean ~ week, data=sd_week, xlab="", ylab=denslab, xaxt='n', type='n', ylim=c(0,610))
  axis(1, at=c(1,2,3), labels=c("Week 1", "Week 2", "Week3"))

#broc mean and se
with(brocaqua, points(week, sd_mm2.mean,  pch=22,  cex=1.6,bg=trtcols[1], col=trtcols[1],type='b'))
with(brocsoil, points(week, sd_mm2.mean,  pch=22,  cex=1.6,bg=trtcols[2], col=trtcols[2],type='b'))  

with(brocaqua, arrows(week, sd_mm2.mean, week, sd_mm2.mean+sd_mm2.se, angle=90, col=trtcols[1],
                         length=0.05))
with(brocaqua, arrows(week, sd_mm2.mean, week, sd_mm2.mean-sd_mm2.se, angle=90, col=trtcols[1],
                         length=0.05))

with(brocsoil, arrows(week, sd_mm2.mean, week, sd_mm2.mean+sd_mm2.se, angle=90, col=trtcols[2],
                         length=0.05))
with(brocsoil, arrows(week, sd_mm2.mean, week, sd_mm2.mean-sd_mm2.se, angle=90, col=trtcols[2],
                         length=0.05))

#pachoi mean and se
with(pacaqua, points(week, sd_mm2.mean,  pch=21,  cex=1.6,bg=trtcols[1], col=trtcols[1],type='b'))
with(pacsoil, points(week, sd_mm2.mean,  pch=21,  cex=1.6,bg=trtcols[2],col=trtcols[2],type='b'))

with(pacaqua, arrows(week, sd_mm2.mean, week, sd_mm2.mean+sd_mm2.se, angle=90, col=trtcols[1],
                      length=0.05))
with(pacaqua, arrows(week, sd_mm2.mean, week, sd_mm2.mean-sd_mm2.se, angle=90, col=trtcols[1],
                      length=0.05))

with(pacsoil, arrows(week, sd_mm2.mean, week, sd_mm2.mean+sd_mm2.se, angle=90, col=trtcols[2],
                      length=0.05))
with(pacsoil, arrows(week, sd_mm2.mean, week, sd_mm2.mean-sd_mm2.se, angle=90, col=trtcols[2],
                      length=0.05))

#lettuce - means and se
with(lettuceaqua, points(week, sd_mm2.mean,  pch=24,  cex=1.6,bg=trtcols[1],col=trtcols[1],type='b'))
with(lettucesoil, points(week, sd_mm2.mean,  pch=24,  cex=1.6,bg=trtcols[2],col=trtcols[2],type='b'))

with(lettuceaqua, arrows(week, sd_mm2.mean, week, sd_mm2.mean+sd_mm2.se, angle=90, col=trtcols[1],
                         length=0.05))
with(lettuceaqua, arrows(week, sd_mm2.mean, week, sd_mm2.mean-sd_mm2.se, angle=90, col=trtcols[1],
                         length=0.05))

with(lettucesoil, arrows(week, sd_mm2.mean, week, sd_mm2.mean+sd_mm2.se, angle=90, col=trtcols[2],
                         length=0.05))
with(lettucesoil, arrows(week, sd_mm2.mean, week, sd_mm2.mean-sd_mm2.se, angle=90, col=trtcols[2],
                         length=0.05))

legend(1,620, boxlabs, pch=21, pt.bg = trtcols, inset=0.02, 
       title="", bty='n') 
legend(1,555, specieslabs, pch=pchs, inset=0.02,  title="", bty='n') 

dev.off()
