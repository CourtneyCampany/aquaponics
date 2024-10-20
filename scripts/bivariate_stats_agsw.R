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
library(mgcv)

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$week <- as.factor(gasex$week)
  gasex$species <- as.factor(gasex$species)

soil <- gasex[gasex$treatment == 'C',]
aqua <- gasex[gasex$treatment == 'A',]


#photo gs - model with species as random--------------
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



##model with species as fixed effect ---------------
ags_mod2 <- lmer(A ~ gsw * treatment * species +  (1|week), data=gasex)
###model tests
qqPlot(residuals(ags_mod2))#pretty good
plot(ags_mod2) #pretty good

Anova(ags_mod2) #gsw * treatment * species (p<0.0001) there is a 3 way interaction
# summary(ags_mod2)
r.squaredGLMM(ags_mod2)
# R2m       R2c
# [1,] 0.8057455 0.8746618
##small amount of variation due to week

pairwise_interaction <- emmeans(ags_mod2, ~ gsw * treatment * species)
pairs(pairwise_interaction) #hard to diagnose, use plotting to help
#summary of differences
# aqua b vs aqua P
# aqua B vs soil P
# soil B vs aqua P
# soil b vs soil p
# aqua p vs aqua S
# soil P vs aqua S 

##find interactions with visreg by splitting treatments to visualize
agsw_aqua <- lmer(A ~ gsw * species + (1|week), data=aqua)
pairwise_aqua <- emmeans(agsw_aqua, ~ gsw *  species)
pairs(pairwise_aqua) ##broc and pak differ, pak and salanova differ
Anova(agsw_aqua, type=3)

visreg(agsw_aqua, "gsw", by="species")
#all positive but slopes differ between broc and pak and pak and salanova. 


agsw_container <- lmer(A ~ gsw * species + (1|week), data=soil)
pairwise_soil <- emmeans(agsw_container, ~ gsw *  species)
pairs(pairwise_soil)
Anova(agsw_container, type=3) ## broc and pac differ, pac and sal differ
visreg(agsw_container, "gsw", by="species")

# An was positively  related to gs across species but slopes of this relationship
# differed between species and treatments (gsw x treatment x species, p < 0.001)
# In aquaponics, positive slopes of the A-gs relationship for broccoli and salanova 
# both differed from pak choi. In soil treatments, positive slopes of the A-gs 
# relationship differed between broccoli and pak choi.  By treatment, positive slopes
# of the A-gs relationship did not differ within a species. Instead, a few instances
# occurred where slopes differed across species (eg., aquaponics - broccoli vs soil - 
# pak choi).

##is gam better?
ags_mod <- lmer(A ~ gsw * treatment + (1|species), data=gasex)
summary(ags_mod)
Anova(ags_mod)
r.squaredGLMM(ags_mod)

##A vs GS non-linear with GAM
ags_gam <- gam(A ~ s(gsw, by=treatment) + s(species, bs='re'), data=gasex)
gam.check(ags_gam)
summary(ags_gam) #p<0.001
anova.gam(ags_gam)
summary(ags_gam)$s.table


library(marginaleffects)
plot_predictions(ags_gam, condition=c('gsw', 'treatment'), type = 'link')

plot_slopes(ags_gam, variables = 'gsw',
            condition = c('gsw', 'treatment'),
            type = 'link')

AIC(ags_mod)
AIC(ags_gam)
#use nonlinear