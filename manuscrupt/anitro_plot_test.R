source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
gasex$ITE <- with(gasex, A/(E*1000))

stomata <- read.csv("raw_data/stomata_traits.csv")

alldata <- merge(gasex, stomata, by=c('species', 'treatment', 'week', 'plant'), all=TRUE)
  alldata$species <- as.factor(alldata$species)
  alldata$trial <- as.factor(alldata$trial)
  alldata$treatment <- as.factor(alldata$treatment)

soil <- alldata[alldata$treatment == 'C',]
aqua <- alldata[alldata$treatment == 'A',]

photo_lastweek <- gasex[gasex$week == 3, ] ##need to fix with broc week4

element <- read.csv("raw_data/elemental_leaves.csv")
  element$species <- as.factor(element$species)
  element$trial <- as.factor(element$trial)
  element$treatment <- as.factor(element$treatment)
  element$uniqueid <- paste(element$species, element$treatment, sep="-")
  element$nmass <- element$n_perc/100

photo_agg <- doBy::summaryBy(A ~ treatment + species + plant , 
                             data =gasex, FUN=mean, keep.names=TRUE)

photonitro <- merge(photo_agg, element)
  photonitro$species <- as.factor(photonitro$species)
  photonitro$treatment <- as.factor(photonitro$treatment)
  photonitro_lastweek <- merge(photo_agg, element)

##setup for plotting (splitting data and simple models) with An
container <- photonitro[photonitro$treatment == 'C',]
aquaponic <- photonitro[photonitro$treatment == 'A',]


#simple models - various combo
broc_mod <- lm(A ~ nmass ,data=photonitro[photonitro$species =="B",])
pac_mod <- lm(A ~ nmass ,data=photonitro[photonitro$species =="P",])
sal_mod <- lm(A ~ nmass ,data=photonitro[photonitro$species =="S",])

# anitro_mod_soil <- lmer(A ~ nmass * species + (1|plant), data=container)
# anitro_mod_aqua <- lmer(A ~ nmass * species + (1|plant), data=aquaponic)

##lines by species
windows()
plot(A ~ nmass, data=photonitro, type='n', ylab=photolab, xlab=nitrolab, ylim=c(0,30), 
     xlim=c(0,.1))
points(A ~ nmass, data=aquaponic, col=trtcols2[1], pch=pchs[species],cex=1.25)
points(A ~ nmass, data=container, col=trtcols2[2], pch=pchs[species],cex=1.25)
predline(broc_mod, col="black", lwd=2, lty=2)
text(.072, 16.5, "Broccoli")
predline(pac_mod, col="black", lwd=2, lty=2)
text(.085, 20.1, "Pak Choi")
predline(sal_mod, col="black", lwd=2, lty=2)
text(.09, 13.6, "Salanova")

legend("topright", legend = specieslabs, pch=pchs, col="black", bty="n", inset=.02)
legend(.05,31, boxlabs, pch=16, col=trtcols, inset=0.02,  title="", bty='n') 

text(0.1025, 7, expression(paste(R[cond]^{"2"}," = "," 0.37")), 1.25)
text(0.1025, 5, expression(paste(R[marg]^{"2"}," = "," 0.90")), 1.25)
text(0, 29.5, "A", cex=1.25, font=2)
