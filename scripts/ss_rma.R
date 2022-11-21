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


#LETTUCE
with(lettuce, interaction.plot(week, treatment, stomatasize_mm2,
    ylab = "mean of stomatal size", xlab = "time", trace.label = "treatment"))
plot(stomatasize_mm2~week, data=lettuceaqua)
plot(stomatasize_mm2~week, data=lettucesoil)

#repeated measures lettuce
lettuce.lm <- lm(stomatasize_mm2 ~ treatment * week,data = lettuce)
qqPlot(residuals(lettuce.lm))#pretty good
plot(lettuce.lm) 
anova(lettuce.lm)
summary(lettuce.lm)
#interaction treatment and species

#within and across treatments (use for by week comparisons)
pairwise_interaction <- emmeans(lettuce.lm, ~ treatment* week)
pairwise_treatment <- emmeans(lettuce.lm, ~ treatment )
pairwise_week <- emmeans(lettuce.lm, ~ week )

emmeans(pairwise_treatment, list(pairwise ~ treatment), adjust = "tukey")
emmeans(pairwise_interaction, list(pairwise ~ treatment * week), adjust = "tukey")
emmeans(pairwise_week, list(pairwise ~ week), adjust = "tukey")
#SS same between treatments
#sd variable each week
#same on first week

##test are p value same if limit to just one way
lettucesoil.lm<- lm(stomatasize_mm2 ~ week, data = lettucesoil)
summary(lettucesoil.lm)
emmeans(lettucesoil.lm, list(pairwise ~ week), adjust = "tukey")
#SD decreased in week 3

lettuceaqua.lm <- lm(stomatasize_mm2 ~ week, data = lettuceaqua)
summary(lettuceaqua.lm)
emmeans(lettuceaqua.lm, list(pairwise ~ week), adjust = "tukey")
#variable across weeks

#BROCCOLI
with(broc, interaction.plot(week, treatment, sd_mm2,
    ylab = "mean of stomatal size", xlab = "time", trace.label = "treatment"))
plot(stomatasize_mm2~week, data=brocaqua)
plot(stomatasize_mm2~week, data=brocsoil)

broc.lm <- lm(sd_mm2 ~ treatment * week, data = broc)
qqPlot(residuals(broc.lm))#pretty good
plot(broc.lm) 
anova(broc.lm)
summary(broc.lm)
##interaction with treatment and week (0.0148)

#within and across treatments (use for by week comparisons)
pairwise_treatment_broc <- emmeans(broc.lm, ~ treatment)
  pairs(pairwise_treatment_broc)
#aquaponics higher
pairwise_week_broc <- emmeans(broc.lm, ~ week )
  pairs(pairwise_week_broc)
##generally goes down after week 1 (soil only?)
pairwise_interaction_broc <- emmeans(broc.lm, ~ treatment * week )
  pairs(pairwise_interaction_broc)
##SD higher in broccoli
  #increases in week 2 for soil only
  
##test are p value same if limit to just one way
brocsoil.lm<- lm(stomatasize_mm2 ~ week, data = brocsoil)
  summary(brocsoil.lm)
  anova(brocsoil.lm)
  emmeans(brocsoil.lm, list(pairwise ~ week), adjust = "tukey")
#SD decreased every week (p<0.0001)
brocaqua.lm <- lm(stomatasize_mm2 ~ week, data = brocaqua)
  summary(brocaqua.lm)
  anova(brocaqua.lm)
  emmeans(brocaqua.lm, list(pairwise ~ week), adjust = "tukey")
###no change
#SD decreased week2 (p<0.003)


#PACCHOI
with(pac, interaction.plot(week, treatment, sd_mm2,
  ylab = "mean of stomatal size", xlab = "time", trace.label = "treatment"))
  plot(stomatasize_mm2~week, data=pacaqua)
  plot(stomatasize_mm2~week, data=pacsoil)
  
pac.lm <- lm(stomatasize_mm2 ~ treatment * week, data = pac)
  qqPlot(residuals(pac.lm))#pretty good
  plot(pac.lm) 
  anova(pac.lm)
  summary(pac.lm)
#only week, no treatment or interaction
  
#within and across treatments (use for by week comparisons)
pairwise_interaction_pac<- emmeans(pac.lm, ~ treatment * week )
pairs(pairwise_interaction_pac)
#same first week

pairwise_treatment_pac<- emmeans(pac.lm, ~ treatment)
pairs(pairwise_treatment_pac)
#treatments same

pairwise_week_pac<- emmeans(pac.lm, ~ week )
pairs(pairwise_week_pac)
#got smaller weekly

##test are p value same if limit to just one way
pacsoil.lm <- lm(stomatasize_mm2 ~ week, data = pacsoil)
summary(pacsoil.lm)
anova(pacsoil.lm)
emmeans(pacsoil.lm, list(pairwise ~ week), adjust = "tukey")
#SD generally decreases (p<0.001)

pacaqua.lm <- lm(stomatasize_mm2 ~ week, data = pacaqua)
summary(pacaqua.lm)
anova(pacaqua.lm)
emmeans(pacaqua.lm, list(pairwise ~ week), adjust = "tukey")
##generally gets smaller except for last week (p<0.001)