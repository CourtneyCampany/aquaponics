source("scripts/functions.R")
source("scripts/plot_objects.R")

stomata <- read.csv("raw_data/stomata_traits.csv")
  stomata$treatment <- as.factor(stomata$treatment)
  stomata$week <- as.factor(stomata$week)
  stomata$species <- as.factor(stomata$species)

sd_plant <- doBy::summaryBy(sd_mm2 ~ treatment + species + plant + week, 
                              data =stomata, FUN=mean2, keep.names=TRUE)

sd_agg <- doBy::summaryBy(sd_mm2 ~ treatment + species + week, 
                            data =sd_plant, FUN=mean2, keep.names=TRUE)
  
#species
lettuce <- stomata[stomata$species == "S",]
broc <- stomata[stomata$species == "B" ,]
pac <- stomata[stomata$species == "P",]

##species by treatment
lettuceaqua <- sd_plant[sd_plant$species == "S" & sd_plant$treatment == "A",]
lettucesoil <- sd_plant[sd_plant$species == "S" & sd_plant$treatment == "C",]

brocaqua <- sd_plant[sd_plant$species == "B" & sd_plant$treatment == "A",]
brocsoil <- sd_plant[sd_plant$species == "B" & sd_plant$treatment == "C",]

pacaqua <- sd_plant[sd_plant$species == "P" & sd_plant$treatment == "A",]
pacsoil <- sd_plant[sd_plant$species == "P" & sd_plant$treatment == "C",]


#plot lettuce
with(lettuce, interaction.plot(week, treatment, sd_mm2,
    ylab = "mean of stomatal density", xlab = "time", trace.label = "treatment"))

#repeated measures lettuce

lettuce.aov <- aov(sd_mm2 ~ treatment * week + Error(plant),
                   data = lettuce)
summary(lettuce.aov)

library(emmeans)
#within and across treatments (use for by week comparisons)
pairwise_treatment <- emmeans(lettuce.aov, ~ treatment * week )
pairs(pairwise_treatment)

##test are p value same if limit to just one way
lettucesoil.aov <- aov(sd_mm2 ~ week + Error(plant), data = lettucesoil)
summary(lettucesoil.aov)
#SD changed by week (p<0.001)
lettuceaqua.aov <- aov(sd_mm2 ~ week + Error(plant), data = lettuceaqua)
summary(lettuceaqua.aov)
#SD did not change by week

#repeated measures broc
with(broc, interaction.plot(week, treatment, sd_mm2,
    ylab = "mean of stomatal density", xlab = "time", trace.label = "treatment"))

broc.aov <- aov(sd_mm2 ~ treatment * week + Error(plant), data = broc)
summary(broc.aov)

#within and across treatments (use for by week comparisons)
pairwise_treatment_broc <- emmeans(broc.aov, ~ treatment * week )
pairs(pairwise_treatment_broc)

##test are p value same if limit to just one way
brocsoil.aov <- aov(sd_mm2 ~ week + Error(plant), data = brocsoil)
summary(brocsoil.aov)
#SD changed by week (p<0.001)
brocaqua.aov <- aov(sd_mm2 ~ week + Error(plant), data = brocaqua)
summary(brocaqua.aov)
#SD did not change by week (p<0.964)

#repeated measures pacchoy-------
pac.aov <- aov(sd_mm2 ~ treatment * week + Error(plant), data = pac)
summary(pac.aov)

with(pac, interaction.plot(week, treatment, sd_mm2,
      trace.label = "treatment"))

#within and across treatments (use for by week comparisons)
pairwise_treatment_pac<- emmeans(pac.aov, ~ treatment * week )
pairs(pairwise_treatment_pac)

##test are p value same if limit to just one way
pacsoil.aov <- aov(sd_mm2 ~ week + Error(plant), data = pacsoil)
summary(pacsoil.aov)
#SD changed by week (p<0.001)
pacaqua.aov <- aov(sd_mm2 ~ week + Error(plant), data = pacaqua)
summary(pacaqua.aov)
#SD did not change by week (p<0.964)