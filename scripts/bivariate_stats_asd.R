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

##based on the graphs, are any of the species relationships actually significant??
lettuce <- alldata[alldata$species == "S",]
broc <- alldata[alldata$species == "B" ,]
pac <- alldata[alldata$species == "P",]

## lettuce
lettuce_mod <- lm(A ~ sd_mm2 * treatment, data=lettuce)
qqPlot(residuals(lettuce_mod))
Anova(lettuce_mod) ##treatment only, no SD
summary(lettuce_mod)
visreg(lettuce_mod, "sd_mm2", by="treatment", overlay=TRUE)

#photosynthesis not related to SD, but A differs between Aqua and Soil

## broc
broc_mod <- lm(A ~ sd_mm2 * treatment, data=broc)
qqPlot(residuals(broc_mod))
Anova(broc_mod) ##minor interaction with sd and treatment (otherwise not SD)
summary(broc_mod)
visreg(broc_mod, "sd_mm2", by="treatment", overlay=TRUE)##aqua appears related

broc_interaction <- emmeans(broc_mod, ~ sd_mm2 * treatment)
emmeans(broc_interaction, list(pairwise ~ sd_mm2 * treatment), adjust = "tukey")
##slopes different, no relationships with soil, aqua = positive A vs sd

brocaqua <- alldata[alldata$species == "B" & alldata$treatment == "A",]
broc_mod2 <- lm(A ~ sd_mm2 , data=brocaqua)
qqPlot(residuals(broc_mod2))
Anova(broc_mod2) ##confirmed significant in only broc aqua

## pak
pac_mod <- lm(A ~ sd_mm2 * treatment, data=pac)
qqPlot(residuals(pac_mod))
Anova(pac_mod) ##interaction with sd and treatment, main effects are sig
summary(pac_mod)
visreg(pac_mod, "sd_mm2", by="treatment", overlay=TRUE) ##both negative but slopes differ

pac_interaction <- emmeans(pac_mod, ~ sd_mm2 * treatment)
emmeans(pac_interaction, list(pairwise ~ sd_mm2 * treatment), adjust = "tukey")
