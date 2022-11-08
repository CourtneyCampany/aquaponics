source("scripts/functions.R")
source("scripts/plot_objects.R")

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

gasex <- read.csv("raw_data/gasexchange_master.csv")
gasex$ITE <- with(gasex, A/(E*1000))

stomata <- read.csv("raw_data/stomata_traits.csv")

alldata <- merge(gasex, stomata, by=c('species', 'treatment', 'week', 'plant'), all=TRUE)
  alldata$species <- as.factor(alldata$species)
  alldata$trial <- as.factor(alldata$trial)
  alldata$treatment <- as.factor(alldata$treatment)

container <- alldata[alldata$treatment == 'C',]
aquaponic <- alldata[alldata$treatment == 'A',]


#photo gs - overall
ags_mod <- lmer(A ~ gsw * treatment + (1|species), data=gasex)
ags_mod2 <- lmer(A ~ gsw * treatment * species +  (1|plant), data=gasex)

###missing normality tests

Anova(ags_mod)
summary(ags_mod)
r.squaredGLMM(ags_mod)
  # R2m       R2c 
  # 0.5562165 0.7796723
  #P<o.ooo1
  
#Slopes of relationship broadly differ between treatments, across species
ags_mod_slopes <- lstrends(ags_mod, "treatment", var="gsw") #obtain slopes
  pairs(ags_mod_slopes)

#slopes different
#   contrast estimate   SE  df t.ratio p.value
#   a - c       -9.07 1.16 373  -7.845  <.0001
#   
#   Degrees-of-freedom method: kenward-roger 


Anova(ags_mod2)
  summary(ags_mod2)
  r.squaredGLMM(ags_mod2)
##lots to unpack
  
#photo sd - overall-----------------
asd_mod <- lmer(A ~ sd_mm2 * treatment + (1|species), data=alldata)
asd_mod2 <- lmer(A ~ sd_mm2 * treatment * species +  (1|plant), data=alldata)
  
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
  #   a - c       -9.07 1.16 373  -7.845  <.0001
  #   
  #   Degrees-of-freedom method: kenward-roger 
  
  
  Anova(asd_mod2)
  summary(asd_mod2)
  r.squaredGLMM(asd_mod2)
  