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
  gasex$ITE <- with(gasex, A/(E*1000))

# container <- alldata[alldata$treatment == 'C',]
# aquaponic <- alldata[alldata$treatment == 'A',]

#photo gs - overall
ags_mod <- lmer(A ~ gsw * treatment + (1|species), data=gasex)
# ags_mod2 <- lmer(A ~ gsw * treatment * species +  (1|plant), data=gasex)

###model tests
qqPlot(residuals(ags_mod))#pretty good
plot(ags_mod) #pretty good

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

visreg(ags_mod, "gsw", by="treatment", overlay=TRUE)

# Anova(ags_mod2)
#   summary(ags_mod2)
#   r.squaredGLMM(ags_mod2)
##lots to unpack
  
  
