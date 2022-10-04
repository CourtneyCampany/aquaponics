source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$species <- as.factor(gasex$species)
  gasex$trial <- as.factor(gasex$trial)
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$ITE <- with(gasex, A/(E*1000))


#Fern vs Sela stats
library(visreg)
library(multcomp)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(arm)
library(car)
library(plotrix)
  
#photo gs
ags_mod <- lmer(A ~ gsw * treatment + (1|species), data=gasex)
  
Anova(ags_mod)
summary(ags_mod)
r.squaredGLMM(ags_mod)
  # R2m       R2c 
  # 0.5562165 0.7796723
  #P<o.ooo1
  
#no interaction with habitat, but.....
ags_slopes <- lstrends(ags_mod, "treatment", var="gsw") #obtain slopes
  pairs(ags_slopes)
#slopes different
#   contrast estimate   SE  df t.ratio p.value
#   a - c       -9.07 1.16 373  -7.845  <.0001
#   
#   Degrees-of-freedom method: kenward-roger 
  