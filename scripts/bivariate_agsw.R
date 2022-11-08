source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$ITE <- with(gasex, A/(E*1000))

stomata <- read.csv("raw_data/stomata_traits.csv")


alldata <- merge(gasex, stomata, by=c('species', 'treatment', 'week', 'plant'), all=TRUE)
  alldata$species <- as.factor(alldata$species)
  alldata$trial <- as.factor(alldata$trial)
  alldata$treatment <- as.factor(alldata$treatment)
  
  
container <- alldata[alldata$treatment == 'C',]
aquaponic <- alldata[alldata$treatment == 'A',]


###simple models for graphs (a by gsw)
stomcond_broc_cont <- lm(A ~ gsw ,data=container[container$species=='B',])
stomcond_pac_cont <- lm(A ~ gsw ,data=container[container$species=='P',])
stomcond_lettuce_cont <- lm(A ~ gsw ,data = container[container$species=='S',])

stomcond_broc_aqua <- lm(A ~ gsw ,data=aquaponic[aquaponic$species=='B',])
stomcond_pac_aqua <- lm(A ~ gsw ,data=aquaponic[aquaponic$species=='P',])
stomcond_lettuce_aqua <- lm(A ~ gsw ,data = aquaponic[aquaponic$species=='S',])

###plot #1 A by gsw - two panel aquaponics vs constainer
# jpeg(filename = "master_scripts/figure2.jpeg",
  #      width = 12, height = 6, units = "in", res= 400)
  
windows(12,5)
par(mfrow=c(1,2),mgp=c(2.5,.75,0), cex.lab=1.15,cex.axis=1.15)
  
#photo gsw
par(mar=c(5,5,1,0))
plot(A ~ gsw, data=aquaponic, type='n', ylab=photolab, xlab="", ylim=c(0,30), xlim=c(0,1.7))
  predline(stomcond_broc_aqua, col="black", lwd=2, lty=2)
  predline(stomcond_pac_aqua, col="black", lwd=2, lty=2)
  predline(stomcond_lettuce_aqua, col="black", lwd=2, lty=2)
  points(A ~ gsw, data=aquaponic, col=trtcols2[1], pch=pchs[species],cex=1.25)
  legend("topleft", legend = specieslabs, pch=pchs, col="black", bty="n", inset=.01,1.15)
  text(1.65, 29.5, "A", cex=1.25)
  text(0.25,1, "Aquaponics", cex=1.25)
  text(1.75, 10, expression(paste(R[cond]^{"2"}," = "," 0.33")), 1.25)
  text(1.75, 5, expression(paste(R[marg]^{"2"}," = "," 0.88")), 1.25)

par(mar=c(5,0,1,1))
plot(A ~ gsw, data=container, type='n',yaxt='n',ylab="", xlab="", ylim=c(0,30), xlim=c(0,1.7))
  axis(2, labels=FALSE, tcl=.25)
  predline(stomcond_broc_cont, col="black", lwd=2, lty=2)
  predline(stomcond_pac_cont, col="black", lwd=2, lty=2)
  predline(stomcond_lettuce_cont, col="black", lwd=2, lty=2)
  points(A ~ gsw, data=container, col=trtcols2[2], pch=pchs[species],cex=1.25)
  # legend("topleft", legend = specieslabs, pch=pchs, col="black", bty="n", inset=.01,1.15)
  text(1.65, 29.5, "B", cex=1.25)
  text(0.25,1, "Container", cex=1.25)
  text(1.7, 10, expression(paste(R[cond]^{"2"}," = "," 0.33")), 1.25)
  text(1.7, 5, expression(paste(R[marg]^{"2"}," = "," 0.88")), 1.25)
  
  mtext(side=1, text=condlab, line=2.5,at=0, cex=1.15)
  