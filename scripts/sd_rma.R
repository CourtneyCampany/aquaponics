source("scripts/functions.R")
source("scripts/plot_objects.R")

library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(lmerTest)
library(LMERConvenienceFunctions)
library(stats)

stomata <- read.csv("raw_data/stomata_traits.csv")
  stomata$treatment <- as.factor(stomata$treatment)
  stomata$week <- as.factor(stomata$week)
  stomata$species <- as.factor(stomata$species)

#species
lettuce <- stomata[stomata$species == "S",]
  lettuce$week <- droplevels(lettuce$week)
  
broc <- stomata[stomata$species == "B" ,]
  broc$week <- droplevels(broc$week)

pac <- stomata[stomata$species == "P",]

##species by treatment
lettuceaqua <- lettuce[lettuce$species == "S" & lettuce$treatment == "A",]
lettucesoil <- lettuce[lettuce$species == "S" & lettuce$treatment == "C",]

brocaqua <- broc[broc$species == "B" & broc$treatment == "A",]
brocsoil <- broc[broc$species == "B" & broc$treatment == "C",]

pacaqua <- pac[pac$species == "P" & pac$treatment == "A",]
pacsoil <- pac[pac$species == "P" & pac$treatment == "C",]

#plot lettuce
with(lettuce, interaction.plot(week, treatment, sd_mm2,
    ylab = "mean of stomatal density", xlab = "time", trace.label = "treatment"))

#repeated measures lettuce
lettuce.lm <- lm(sd_mm2 ~ treatment * week,data = lettuce)
qqPlot(residuals(lettuce.lm))#pretty good
plot(lettuce.lm) 
anova(lettuce.lm) #interaction 
summary(lettuce.lm)
#interaction treatment and species

#within and across treatments (use for by week comparisons)
pairwise_interaction <- emmeans(lettuce.lm, ~ treatment* week)
pairwise_treatment <- emmeans(lettuce.lm, ~ treatment )
pairwise_week <- emmeans(lettuce.lm, ~ week )

emmeans(pairwise_treatment, list(pairwise ~ treatment), adjust = "tukey")
emmeans(pairwise_interaction, list(pairwise ~ treatment * week), adjust = "tukey")
emmeans(pairwise_week, list(pairwise ~ week), adjust = "tukey")
#sd higher in week 2 for soil, aqua doesnt change (0.0001)
#SD in soil higher than aqua in weeks 2/3

##test are p value same if limit to just one way
lettucesoil.lm<- lm(sd_mm2 ~ week, data = lettucesoil)
summary(lettucesoil.lm)
emmeans(lettucesoil.lm, list(pairwise ~ week), adjust = "tukey")
#SD increased after week 1(p<0.001)

lettuceaqua.lm <- lm(sd_mm2 ~ week , data = lettuceaqua)
summary(lettuceaqua.lm)
emmeans(lettuceaqua.lm, list(pairwise ~ week), adjust = "tukey")
#SD did not change by week

#BROCCOLI
with(broc, interaction.plot(week, treatment, sd_mm2,
    ylab = "mean of stomatal density", xlab = "time", trace.label = "treatment"))
plot(sd_mm2~week, data=brocaqua)
plot(sd_mm2~week, data=brocsoil)

broc.lm <- lm(sd_mm2 ~ treatment * week, data = broc)
qqPlot(residuals(broc.lm))#pretty good
plot(broc.lm) 
anova(broc.lm)
summary(broc.lm)
##interaction with treatment and week (p=0.0148)

#within and across treatments (use for by week comparisons)
pairwise_treatment_broc <- emmeans(broc.lm, ~ treatment)
  pairs(pairwise_treatment_broc)
pairwise_week_broc <- emmeans(broc.lm, ~ week )
  pairs(pairwise_week_broc)
pairwise_interaction_broc <- emmeans(broc.lm, ~ treatment * week )
  pairs(pairwise_interaction_broc)

emmeans(pairwise_interaction_broc, list(pairwise ~ treatment * week), adjust = "tukey")
  ##SD always higher in broccoli
  ##sd higher in week 2 for soil, aqua doesnt change (0.0148)
  
##test are p value same if limit to just one way
brocsoil.lm<- lm(sd_mm2 ~ week, data = brocsoil)
  summary(brocsoil.lm)
  anova(brocsoil.lm)
  emmeans(brocsoil.lm, list(pairwise ~ week), adjust = "tukey")
#SD increased week2 (p<0.001)
brocaqua.lm <- lm(sd_mm2 ~ week, data = brocaqua)
  summary(brocaqua.lm)
  anova(brocaqua.lm)
  emmeans(brocaqua.lm, list(pairwise ~ week), adjust = "tukey")
###no change
#SD did not change by week (p<0.964)


#repeated measures pacchoy-------
with(pac, interaction.plot(week, treatment, sd_mm2,trace.label = "treatment"))
  plot(sd_mm2~week, data=pacaqua)
  plot(sd_mm2~week, data=pacsoil)
  
pac.lm <- lm(sd_mm2 ~ treatment * week, data = pac)
  qqPlot(residuals(pac.aov))#pretty good
  plot(pac.lm) 
  anova(pac.lm)
  summary(pac.lm)
#interaction with treatment and week
  
#within and across treatments (use for by week comparisons)
pairwise_interaction_pac<- emmeans(pac.lm, ~ treatment * week )
pairs(pairwise_interaction_pac)
emmeans(pairwise_interaction_pac, list(pairwise ~ treatment * week), adjust = "tukey")


pairwise_treatment_pac<- emmeans(pac.lm, ~ treatment)
pairs(pairwise_treatment_pac)
#treatments same

pairwise_week_pac<- emmeans(pac.lm, ~ week )
pairs(pairwise_week_pac)
#increases after week 1

##test are p value same if limit to just one way
pacsoil.lm <- lm(sd_mm2 ~ week, data = pacsoil)
summary(pacsoil.lm)
anova(pacsoil.lm)
emmeans(pacsoil.lm, list(pairwise ~ week), adjust = "tukey")
#SD increased after first week (p<0.001)

pacaqua.lm <- lm(sd_mm2 ~ week, data = pacaqua)
summary(pacaqua.lm)
anova(pacaqua.lm)
emmeans(pacaqua.lm, list(pairwise ~ week), adjust = "tukey")
##SD increased after first week (p<0.001)