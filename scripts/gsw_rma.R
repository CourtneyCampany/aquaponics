source("scripts/functions.R")
source("scripts/plot_objects.R")

library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(lmerTest)
library(LMERConvenienceFunctions)
library(stats)

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$species <- as.factor(gasex$species)
  gasex$trial <- as.factor(gasex$trial)
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$ITE <- with(gasex, A/(E*1000))
  gasex$week <- as.factor(gasex$week)

  
#extract species ---------
lettuce <- gasex[gasex$species == "S",]
    lettuce$week <- droplevels(lettuce$week)
broc <- gasex[gasex$species == "B" ,]
broc$week <- droplevels(broc$week)
pac <- gasex[gasex$species == "P",]
  pac_noweek4 <- pac[pac$week  %in% c(1,2,3),]
  pac_noweek4$week <- droplevels(pac_noweek4$week)
                          
#species by treatment
lettuceaqua <- gasex[gasex$species == "S" & gasex$treatment == "A",]
lettucesoil <- gasex[gasex$species == "S" & gasex$treatment == "C",]
  
brocaqua <- gasex[gasex$species == "B" & gasex$treatment == "A",]
brocsoil <- gasex[gasex$species == "B" & gasex$treatment == "C",]
  
pacaqua <- pac_noweek4[pac_noweek4$species == "P" & pac_noweek4$treatment == "A",]
pacsoil <- pac_noweek4[pac_noweek4$species == "P" & pac_noweek4$treatment == "C",]

##LETTUCE

#plot lettuce interation plot
with(lettuce, interaction.plot(week, treatment, gsw,
    ylab = "mean of photosynthesis", xlab = "time", trace.label = "treatment"))

#repeated measures lettuce
lettuce.lme <- lm(sqrt(gsw) ~ treatment*week, data = lettuce)
qqPlot(residuals(lettuce.lme))#pretty good
plot(lettuce.lme) #skewed
Anova(lettuce.lme)
summary(lettuce.lme)
#interaction, treatment x week 

#within and across treatments (use for by week comparisons)
pairwise_interaction <- emmeans(lettuce.lme, ~ treatment* week)
pairwise_treatment <- emmeans(lettuce.lme, ~ treatment )
pairwise_week <- emmeans(lettuce.lme, ~ week )

emmeans(pairwise_treatment, list(pairwise ~ treatment), adjust = "tukey")
emmeans(pairwise_interaction, list(pairwise ~ treatment * week), adjust = "tukey")
emmeans(pairwise_week, list(pairwise ~ week), adjust = "tukey")
#gsws higher in aquaponics (0.0001)
#gsw lowers in week 3 both species


##test are p value same by unique treatment, get pairwise comparisons
lettucesoil.mod <- lm(gsw ~ week, data = lettucesoil)
summary(lettucesoil.mod)
Anova(lettucesoil.mod)
emmeans(lettucesoil.mod, list(pairwise ~ week), adjust = "tukey")
##gsw lowers in soil decreased in week 3

lettuceaqua.mod <- lm(A ~ week, data = lettuceaqua)
summary(lettuceaqua.mod)
Anova(lettuceaqua.mod)
emmeans(lettuceaqua.mod, list(pairwise ~ week), adjust = "tukey")
##gsw lower in week3         

##PACCHOI

#plot lettuce interaction plot
with(pac_noweek4, interaction.plot(week, treatment, gsw,
     ylab = "mean of photosynthesis", xlab = "time", trace.label = "treatment"))
plot(gsw~week, data=pacaqua)
plot(gsw~week, data=pacsoil)

#repeated measures lettuce
pac.lme <- lm(gsw ~ treatment*week, data = pac_noweek4)
qqPlot(residuals(pac.lme))#pretty good
plot(pac.lme) 
Anova(pac.lme)
summary(pac.lme)
r.squaredGLMM(pac.lme)
#no interaction, only main effects

#within and across treatments (use for by week comparisons)
pairwise_week <- emmeans(pac.lme, ~ week )
pairs(pairwise_week)
pairwise_treatment <- emmeans(pac.lme, ~ treatment)
emmeans(pairwise_treatment, list(pairwise ~ treatment), adjust = "tukey")
# gsw drops after week 1
##gsw greater in aqua

##test are p value same by unique treatment, get pairwise comparisons
pacsoil.mod <- lm(gsw ~ week, data = pacsoil)
summary(pacsoil.mod)
Anova(pacsoil.mod)
emmeans(pacsoil.mod, list(pairwise ~ week), adjust = "tukey")
##gsw decreased after week 1

pacaqua.mod <- lm(gsw ~ week, data = pacaqua)
summary(pacaqua.mod)
Anova(pacaqua.mod)
emmeans(pacaqua.mod, list(pairwise ~ week), adjust = "tukey")
##gsw no change  
        
##BROCCOLI
## whe singular fit with lme used simple model 
#plot lettuce interaction plot
with(broc, interaction.plot(week, treatment, gsw,
     ylab = "mean of photosynthesis", xlab = "time", trace.label = "treatment"))

#repeated measures lettuce
broc.lme <- lm(gsw ~ treatment*week, data = broc)
qqPlot(residuals(broc.lme))#pretty good
plot(broc.lme) #skewed
Anova(broc.lme)
summary(broc.lme)
r.squaredGLMM(broc.lme)
#interaction with treatment and species

#within and across treatments (use for by week comparisons)
pairwise_interaction <- emmeans(broc.lme, ~ treatment* week)
pairwise_treatment <- emmeans(broc.lme, ~ treatment )
pairwise_week <- emmeans(broc.lme, ~ week )

emmeans(pairwise_treatment, list(pairwise ~ treatment), adjust = "tukey")
emmeans(pairwise_interaction, list(pairwise ~ treatment * week), adjust = "tukey")
emmeans(pairwise_week, list(pairwise ~ week), adjust = "tukey")
#gsws higher in aquaponics (0.0001)
#gsw changes after week1 (look at interaction - ?only for soil)


##test are p value same by unique treatment, get pairwise comparisons
brocsoil.mod <- lm(gsw ~ week, data = brocsoil)
summary(brocsoil.mod)
Anova(brocsoil.mod)
emmeans(brocsoil.mod, list(pairwise ~ week), adjust = "tukey")
#Photosynthesis in soil changes each week (variable)

brocaqua.mod <- lm(gsw ~ week, data = brocaqua)
summary(brocaqua.mod)
Anova(brocaqua.mod)
emmeans(brocaqua.mod, list(pairwise ~ week), adjust = "tukey")
##gsw does not change  