source("scripts/functions.R")

##full model results text pasted at the bottom in case reviewers need it.
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
  gasex$species <- as.factor(gasex$species)
  gasex$trial <- as.factor(gasex$trial)
  gasex$treatment <- as.factor(gasex$treatment)

# 2 possible approaches = average of A across weeks, or choose week closest to N harvest
photo_agg <- doBy::summaryBy(A ~ treatment + species + plant , 
                             data =gasex, FUN=mean, keep.names=TRUE)

photo_lastweek <- gasex[gasex$week == 3, ] ##need to fix with broc week4

element <- read.csv("raw_data/elemental_leaves.csv")
  element$species <- as.factor(element$species)
  element$trial <- as.factor(element$trial)
  element$treatment <- as.factor(element$treatment)
  element$uniqueid <- paste(element$species, element$treatment, sep="-")
  element$nmass <- element$n_perc/100

#Merge
photonitro <- merge(photo_agg, element)
photonitro_lastweek <- merge(photo_agg, element)

aqua <- photonitro[photonitro$treatment == "A",]
container <- photonitro[photonitro$treatment == "C",]

###model tests - species as random effect
anitro_mod <- lmer(A ~ nmass * treatment + (1|species), data=photonitro)
qqPlot(residuals(anitro_mod))#pretty good
plot(anitro_mod) #pretty good

Anova(anitro_mod)
summary(anitro_mod)

# nmass           5.0655  1   0.024406 * 
#   treatment       9.8619  1   0.001687 **
#   nmass:treatment 0.2236  1   0.636337   

#photosynthesis varies related to nitrogen (higher = higher)
#photosynthesis varies by treatment (aqua higher)
# no interaction

coef(summary(anitro_mod))[ , "Estimate"]
ranef(anitro_mod)$species
colMeans(ranef(anitro_mod)$species)

r.squaredGLMM(anitro_mod) #lots of variation due to species but all
# R2m       R2c 
# 0.3725351 0.8980256
#P<o.ooo1

#Slopes of relationship broadly differ between treatments, across species
anitro_mod_slopes <- emmeans(anitro_mod, "treatment", var="nmass") #obtain slopes
pairs(anitro_mod_slopes) ##slopes not different
windows()
visreg(anitro_mod, "nmass", by="treatment", overlay=TRUE)

# 
# 
# ###model tests - species as fixed effect - not using right now
# anitro_mod2 <- lmer(A ~ nmass * treatment * species +  (1|plant), data=photonitro)
# qqPlot(residuals(anitro_mod2))#pretty good
# plot(anitro_mod2) #pretty good
# 
# Anova(anitro_mod2, type=3)
# summary(anitro_mod2)
# r.squaredGLMM(anitro_mod2)
# # R2m       R2c
# # [1,] 0.9004234 0.9055184
# 
# # nmass:treatment:species = 0.0204842 *  
# ##interaction with all 3
# 
# pairwise_interaction <- emmeans(anitro_mod2, ~ nmass * treatment * species)
# pairs(pairwise_interaction)
# 
##find interactions with visreg by splitting treatments to visualize
anitro_aqua <- lmer(A ~ nmass * species + (1|plant), data=aqua)
  pairwise_aqua <- emmeans(anitro_aqua, ~ nmass *  species)
  pairs(pairwise_aqua)
  Anova(anitro_aqua, type=3)

# visreg(anitro_aqua, "nmass", by="species")
# #photosynthesis and nitrogen decoupled as demand saturates rates
# #next steps - rates are near capacity at this light and temperature level
# #true for species, slopes were flat or even slightly negative
# 
# 
anitro_container <- lmer(A ~ nmass * species + (1|plant), data=container)
  pairwise_soil <- emmeans(anitro_container, ~ nmass *  species)
  pairs(pairwise_soil)
  Anova(anitro_container, type=3) ##nmass by species 0.04873 *
visreg(anitro_container, "nmass", by="species")
# #nmass positively coupled with photosynthesis broccoli and pak choi
# #salanova, with lowest rates of A was not correlated with N content
# 
# 
# 
# #slopes and elevations
# nitrolma2 <- sma(nitro_area ~ lma * niche2,data=alldata, 
#                  multcomp = TRUE, multcompmethod='adjusted')
# 
# 
# 
# summary(nitrolma2) #slopes are equal
# #P-value : 0.26588  
# 
# nitrolma3 <- sma(nitro_area ~ lma + niche2,
#                  data=alldata, multcomp = TRUE,
#                  multcompmethod='adjusted') 
# summary(nitrolma3) #elevations not equal P-value : 1.1368e-05
# #elevations different for terr and epi

# The relationship between An and Nm varied by growth treatment and species 
# (Nm * Treatment *Species, p = 0.02). In aquaponics plants, An and Nm were 
# broadly decoupled, as increases in shoot nitrogen did not increase already
# high photosynthesis rates (Figure X). Alternatively, An and Nm were positively
# coupled with photosynthesis broccoli and pak choi plants grown in soil. Salanova 
# plants grown in soil, with the lowest overall rates of An were not correlated with 
# N content.  
