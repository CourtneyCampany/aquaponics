source("scripts/functions.R")

#packages --------
library(visreg)
library(multcomp)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(arm)
library(car)
library(plotrix)
library(lmerTest)
library(effects)

gasex <- read.csv("raw_data/gasexchange_master.csv")

stomata <- read.csv("raw_data/stomata_traits.csv")

alldata <- merge(gasex, stomata, by=c('species', 'treatment', 'week', 'plant'), all=TRUE)
  alldata$species <- as.factor(alldata$species)
  alldata$trial <- as.factor(alldata$trial)
  alldata$treatment <- as.factor(alldata$treatment)
  alldata$ITE <- with(alldata, A/(E*1000))
  
soil <- alldata[alldata$treatment == 'C',]
aqua <- alldata[alldata$treatment == 'A',]


#photo sd - overall-----------------
itesd_mod <- lmer(log(ITE) ~ sd_mm2 * treatment + (1|species), data=alldata)
# itesd_mod2 <- lmer(ITE ~ sd_mm2 * treatment * species +  (1|plant), data=alldata)

#test assumptions
qqPlot(residuals(itesd_mod))#residual look good after log transform
plot(itesd_mod) #pretty good homogeneity of variance

Anova(itesd_mod) #sd_mm2:treatment interaction
summary(itesd_mod)
r.squaredGLMM(itesd_mod)
