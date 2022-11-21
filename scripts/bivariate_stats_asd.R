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
  gasex$ITE <- with(gasex, A/(E*1000))

stomata <- read.csv("raw_data/stomata_traits.csv")

alldata <- merge(gasex, stomata, by=c('species', 'treatment', 'week', 'plant'), all=TRUE)
  alldata$species <- as.factor(alldata$species)
  alldata$trial <- as.factor(alldata$trial)
  alldata$treatment <- as.factor(alldata$treatment)


#photo sd - overall-----------------
asd_mod <- lmer(A ~ sd_mm2 * treatment + (1|species), data=alldata)
# asd_mod2 <- lmer(A ~ sd_mm2 * treatment * species +  (1|plant), data=alldata)

#test assumptions
qqPlot(residuals(asd_mod))#residual look good (no outliers)
plot(asd_mod) #pretty good homogenity of variance

Anova(asd_mod)
summary(asd_mod)
r.squaredGLMM(asd_mod)
# R2m       R2c 
# 0.2482591 0.8060757
#P<0.001328

#Slopes of relationship broadly differ between treatments, across species
asd_mod_slopes <- lstrends(asd_mod, "treatment", var="sd_mm2") #obtain slopes
pairs(asd_mod_slopes)

#slopes different
#   contrast estimate   SE  df t.ratio p.value
#   A - C     0.00582 0.00181 370   3.209  0.0014
#   
#   Degrees-of-freedom method: kenward-roger 


visreg(asd_mod, "sd_mm2", by="treatment", overlay=TRUE)
# 
# print(summary(asd_mod,ddf="Satterthwaite"),correlation=FALSE)

library(emmeans)
emmeans(asd_mod, list(pairwise ~ sd_mm2:treatment), adjust = "tukey")

