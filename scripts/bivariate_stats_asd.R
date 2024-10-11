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
library(mgcv)

gasex <- read.csv("raw_data/gasexchange_master.csv")

stomata <- read.csv("raw_data/stomata_traits.csv")

alldata <- merge(gasex, stomata, by=c('species', 'treatment', 'week', 'plant'), all=TRUE)
  alldata$species <- as.factor(alldata$species)
  alldata$trial <- as.factor(alldata$trial)
  alldata$treatment <- as.factor(alldata$treatment)
  alldata$week <- as.factor(alldata$week)
  
soil <- alldata[alldata$treatment == 'C',]
aqua <- alldata[alldata$treatment == 'A',]


#photo sd - overall-----------------
asd_mod <- lmer(A ~ sd_mm2 * treatment + (1|species), data=alldata)

#test assumptions
qqPlot(residuals(asd_mod))#residual look good (no outliers)
plot(asd_mod) #pretty good homogeneity of variance

Anova(asd_mod) #sd_mm2:treatment interaction
summary(asd_mod)
r.squaredGLMM(asd_mod)
# R2m       R2c 
# 0.2482591 0.8060757
#P<0.001328, lots of variation attributed to species

pairwise_interaction <- emmeans(asd_mod, ~ sd_mm2 * treatment)
pairs(pairwise_interaction)

windows()
visreg(asd_mod, "sd_mm2", by="treatment", overlay=TRUE)
#both negative but slopes differe

#Slopes of relationship broadly differ between treatments
asd_mod_slopes <- lstrends(asd_mod, "treatment", var="sd_mm2") #obtain slopes
pairs(asd_mod_slopes)

#slopes different
#   contrast estimate   SE  df t.ratio p.value
#   A - C     0.00582 0.00181 370   3.209  0.0014
#   
#   Degrees-of-freedom method: kenward-roger 


# print(summary(asd_mod,ddf="Satterthwaite"),correlation=FALSE)

emmeans(asd_mod, list(pairwise ~ sd_mm2:treatment), adjust = "tukey")

anova(lm(A ~ sd_mm2, data=soil))
anova(lm(A ~ sd_mm2, data=aqua))

# ##based on the graphs, are any of the species relationships actually significant??
# lettuce <- alldata[alldata$species == "S",]
# broc <- alldata[alldata$species == "B" ,]
# pac <- alldata[alldata$species == "P",]
# 
# ## lettuce
# lettuce_mod <- lm(A ~ sd_mm2 * treatment, data=lettuce)
# qqPlot(residuals(lettuce_mod))
# Anova(lettuce_mod) ##treatment only, no SD
# summary(lettuce_mod)
# visreg(lettuce_mod, "sd_mm2", by="treatment", overlay=TRUE)
# 
# #photosynthesis not related to SD, but A differs between Aqua and Soil
# 
# ## broc
# broc_mod <- lm(A ~ sd_mm2 * treatment, data=broc)
# qqPlot(residuals(broc_mod))
# Anova(broc_mod) ##minor interaction with sd and treatment (otherwise not SD)
# summary(broc_mod)
# windows()
# visreg(broc_mod, "sd_mm2", by="treatment", overlay=TRUE)##aqua appears related
# 
# broc_interaction <- emmeans(broc_mod, ~ sd_mm2 * treatment)
# emmeans(broc_interaction, list(pairwise ~ sd_mm2 * treatment), adjust = "tukey")
# ##slopes different, no relationships with soil, aqua = positive A vs sd
# 
# brocaqua <- alldata[alldata$species == "B" & alldata$treatment == "A",]
# broc_mod2 <- lm(A ~ sd_mm2 , data=brocaqua)
# qqPlot(residuals(broc_mod2))
# Anova(broc_mod2) ##confirmed significant in only broc aqua
# visreg(broc_mod2, "sd_mm2", by="treatment", overlay=TRUE) ##both negative but slopes differ
# 
# 
# ## pak
# pac_mod <- lm(A ~ sd_mm2 * treatment, data=pac)
# qqPlot(residuals(pac_mod))
# Anova(pac_mod) ##interaction with sd and treatment, main effects are sig
# summary(pac_mod)
# windows()
# visreg(pac_mod, "sd_mm2", by="treatment", overlay=TRUE) ##both negative but slopes differ
# 
# pac_interaction <- emmeans(pac_mod, ~ sd_mm2 * treatment)
# emmeans(pac_interaction, list(pairwise ~ sd_mm2 * treatment), adjust = "tukey")

##is gam better?
asd_mod <- lmer(A ~ sd_mm2 * treatment + (1|species), data=alldata)
summary(asd_mod)
Anova(asd_mod)

##A vs SD non-linear with GAM
asd_gam <- gam(A ~ s(sd_mm2, by=treatment) + s(species, bs='re'), data=alldata)
summary(asd_gam) #p<0.001
anova(asd_gam)
summary(asd_gam)$s.table

AIC(asd_mod)
AIC(asd_gam)
#stick with linear - because of lower AIC scores.

##model with species as fixed effect ---------------
asd_mod2 <- lmer(A ~ sd_mm2 * treatment * species +  (1|week), data=alldata)
###model tests
qqPlot(residuals(asd_mod2))#pretty good
plot(asd_mod2) #pretty good

Anova(asd_mod2) #two interactions - sd * treatment and sd * species
r.squaredGLMM(asd_mod2)
# R2m       R2c
# [1,] 0.6802788 0.7813289
##small amount of variation due to week

pairwise_treatment <- emmeans(asd_mod2, ~ sd_mm2 * treatment)
pairs(pairwise_treatment) 
#slopes of A -sd differed by treatment, far more negative slope when Sd increased in soil
visreg(asd_mod2, "sd_mm2", by="treatment")

pairwise_species<- emmeans(asd_mod2, ~ sd_mm2 * species)
pairs(pairwise_species)
# slopes of A-sd differed for broccoli and salanova differed from pak choi
# A and SD appear decoupled in salanova but not in broc or pakschoi. 
visreg(asd_mod2, "sd_mm2", by="species")

# The relationship between An and SD differed by treatment (An * SD * treatment, p <
# 0.001) and by species (An * SD * species, p < 0.001). Across all species, An declined
# with more increasing SD in soil compared to aquaponics treatments. For individual 
# species, An and SD appeared decoupled in salanova but not in broccoli or pak choi. 

