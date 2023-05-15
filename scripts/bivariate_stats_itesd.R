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
  alldata$week <- as.factor(alldata$week)
  
soil <- alldata[alldata$treatment == 'C',]
aqua <- alldata[alldata$treatment == 'A',]

#ITE sd with species as a random effect-----------------
itesd_mod <- lmer(log(ITE) ~ sd_mm2 * treatment + (1|species), data=alldata)

#test assumptions
qqPlot(residuals(itesd_mod))#residual look good after log transform
plot(itesd_mod) #pretty good homogeneity of variance

Anova(itesd_mod) #no relationship between ITE and SD
summary(itesd_mod)
r.squaredGLMM(itesd_mod)

#ITE sd with species as a fixed effect-----------------
itesd_mod2 <- lmer(log(ITE) ~ sd_mm2 * treatment * species + (1|week), data=alldata)

#test assumptions
qqPlot(residuals(itesd_mod2))#residual look good after log transform
plot(itesd_mod2) #pretty good homogeneity of variance

Anova(itesd_mod2) #sd_mm2:treatment interaction
summary(itesd_mod2)
r.squaredGLMM(itesd_mod2)
# R2m       R2c
# [1,] 0.6118594 0.6616367

pairwise_treatment <- emmeans(itesd_mod2, ~ sd_mm2 * treatment)
pairs(pairwise_treatment) 
ACross species, slopes of ITE and SD differed by treatment (SD * treatment, p= 0.024).
ITE increased with higher SD in aquaponics plants, while ITE and SD appear decoupled
in plants growtn in soil. 
visreg(itesd_mod2, "sd_mm2", by="treatment")
