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
with(lettuce, interaction.plot(week, treatment, ITE,
    ylab = "mean of photosynthesis", xlab = "time", trace.label = "treatment"))

#repeated measures lettuce
lettuce.lm <- lm(sqrt(ITE) ~ treatment*week, data = lettuce)
qqPlot(residuals(lettuce.lm))#pretty good
plot(lettuce.lm) 
Anova(lettuce.lm)
summary(lettuce.lm)
#interaction, treatment x week 

#within and across treatments (use for by week comparisons)
pairwise_interaction <- emmeans(lettuce.lme, ~ treatment* week)
pairwise_treatment <- emmeans(lettuce.lme, ~ treatment )
pairwise_week <- emmeans(lettuce.lme, ~ week )

emmeans(pairwise_treatment, list(pairwise ~ treatment), adjust = "tukey")
emmeans(pairwise_interaction, list(pairwise ~ treatment * week), adjust = "tukey")
emmeans(pairwise_week, list(pairwise ~ week), adjust = "tukey")
#ITEs lower in aquaponics (0.0001))
#ITE lowers in week 3 (look at interaction - ?only for aqua)


##test are p value same by unique treatment, get pairwise comparisons
lettucesoil.mod <- lm(ITE ~ week, data = lettucesoil)
summary(lettucesoil.mod)
Anova(lettucesoil.mod)
emmeans(lettucesoil.mod, list(pairwise ~ week), adjust = "tukey")
##ITE varaible (lower in week 3 vs 1)

lettuceaqua.mod <- lm(A ~ week, data = lettuceaqua)
summary(lettuceaqua.mod)
Anova(lettuceaqua.mod)
emmeans(lettuceaqua.mod, list(pairwise ~ week), adjust = "tukey")
##variable each week but check interaction (is similar week 3?)        

##PACCHOI

#plot lettuce interaction plot
with(pac_noweek4, interaction.plot(week, treatment, ITE,
     ylab = "mean of photosynthesis", xlab = "time", trace.label = "treatment"))
plot(ITE~week, data=pacaqua)
plot(ITE~week, data=pacsoil)

#repeated measures lettuce
pac.lme <- lm(sqrt(ITE) ~ treatment*week, data = pac_noweek4)
qqPlot(residuals(pac.lme))#pretty good
plot(pac.lme) 
Anova(pac.lme)
summary(pac.lme)
#only treatment

#within and across treatments (use for by week comparisons)
pairwise_treatment <- emmeans(pac.lme, ~ treatment)
emmeans(pairwise_treatment, list(pairwise ~ treatment), adjust = "tukey")
##ITE lower in aqua, consistent across weeks

##test are p value same by unique treatment, get pairwise comparisons
pacsoil.mod <- lm(ITE ~ week, data = pacsoil)
summary(pacsoil.mod)
Anova(pacsoil.mod)
emmeans(pacsoil.mod, list(pairwise ~ week), adjust = "tukey")
##ITE no change

pacaqua.mod <- lm(ITE ~ week, data = pacaqua)
summary(pacaqua.mod)
Anova(pacaqua.mod)
emmeans(pacaqua.mod, list(pairwise ~ week), adjust = "tukey")
##ITE no change  
        
##BROCCOLI
## whe singular fit with lme used simple model 
#plot lettuce interaction plot
with(broc, interaction.plot(week, treatment, ITE,
     ylab = "mean of photosynthesis", xlab = "time", trace.label = "treatment"))

#repeated measures lettuce
broc.lm <- lm(log10(ITE) ~ treatment*week, data = broc)
qqPlot(residuals(broc.lm))#pretty good
plot(broc.lm) 
Anova(broc.lm)
summary(broc.lm)
#interaction with treatment and species

#within and across treatments (use for by week comparisons)
pairwise_interaction <- emmeans(broc.lm, ~ treatment* week)
pairwise_treatment <- emmeans(broc.lm, ~ treatment )
pairwise_week <- emmeans(broc.lm, ~ week )

emmeans(pairwise_treatment, list(pairwise ~ treatment), adjust = "tukey")
emmeans(pairwise_interaction, list(pairwise ~ treatment * week), adjust = "tukey")
emmeans(pairwise_week, list(pairwise ~ week), adjust = "tukey")
#ITEs lower in aquaponics (0.0001)
#ITE changes after week1 (look at interaction - ?only for soil)


##test are p value same by unique treatment, get pairwise comparisons
brocsoil.mod <- lm(ITE ~ week, data = brocsoil)
summary(brocsoil.mod)
Anova(brocsoil.mod)
emmeans(brocsoil.mod, list(pairwise ~ week), adjust = "tukey")
#ITE in soil lower after week1

brocaqua.mod <- lm(ITE ~ week, data = brocaqua)
summary(brocaqua.mod)
Anova(brocaqua.mod)
emmeans(brocaqua.mod, list(pairwise ~ week), adjust = "tukey")
##ITE lower after week 2