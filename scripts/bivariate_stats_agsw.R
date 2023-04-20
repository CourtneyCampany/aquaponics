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

gasex <- read.csv("raw_data/gasexchange_master.csv")

soil <- gasex[gasex$treatment == 'C',]
aqua <- gasex[gasex$treatment == 'A',]

broc <- aqua[aqua$species == "B",]
anova(lm(A ~ gsw , data=broc))

broc2 <- soil[soil$species == "B",]
test <- lm(A ~ gsw , data=broc2)
visreg(test, "gsw", by="treatment", overlay=TRUE)

#photo gs - simple model to check for treatment effects
ags_mod <- lmer(A ~ gsw * treatment + (1|species), data=gasex)
###model tests
qqPlot(residuals(ags_mod))#pretty good
plot(ags_mod) #pretty good

Anova(ags_mod) #gsw:treatment:species (p<0.001) there is an interactions
summary(ags_mod)
r.squaredGLMM(ags_mod)
# R2m       R2c
# [1,] 0.5562165 0.7796723

windows()
visreg(ags_mod, "gsw", by="treatment", overlay=TRUE)

pairwise_interaction <- emmeans(ags_mod, ~ gsw * treatment)
pairs(pairwise_interaction)

#Slopes of relationship broadly differ between treatments, across species
ags_mod_slopes <- lstrends(ags_mod, "treatment", var="gsw") #obtain slopes
pairs(ags_mod_slopes)
# contrast estimate   SE  df t.ratio p.value
# A - C       -12.6 2.27 365  -5.542  <.0001
##3x fold increase in gsw in aqua lead to shallower slopes of A gsw (compared to soil) 
## as high rates likely begin to saturate A


##run full model with inteaction (not using right now)
# ags_mod2 <- lmer(A ~ gsw * treatment * species +  (1|plant), data=gasex)
# 
# pairwise_interaction <- emmeans(ags_mod2, ~ gsw * treatment * species)
# pairs(pairwise_interaction) #hard to diagnose, use plotting to help
# #summary of differences
# # aqua b vs aqua P
# # aqua b vs soil p
# # soil b vs aqua p
# # soil b vs soil p 
# # aqua p vs aqua S
# # soil P vs aqua S
# 
# ##find interactions with visreg by splitting treatments to visualize
# agsw_aqua <- lmer(A ~ gsw * species + (1|plant), data=aqua)
# pairwise_aqua <- emmeans(agsw_aqua, ~ gsw *  species)
# pairs(pairwise_aqua) ##broc and pak differ, pak and salanova differ
# Anova(agsw_aqua, type=3)
# 
# visreg(agsw_aqua, "gsw", by="species") 
# #all positive but slopes likely differ
# 
# 
# agsw_container <- lmer(A ~ gsw * species + (1|plant), data=container)
# pairwise_soil <- emmeans(agsw_container, ~ gsw *  species)
# pairs(pairwise_soil)
# Anova(agsw_container, type=3) ##nmass by species 0.04873 *
# visreg(agsw_container, "gsw", by="species")
# #nmass positively coupled with photosynthesis broccoli and pak choi
# #salanova, with lowest rates of A was not correlated with N content


  
  
