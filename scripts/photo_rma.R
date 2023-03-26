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
with(lettuce, interaction.plot(week, treatment, A,
    ylab = "mean of photosynthesis", xlab = "time", trace.label = "treatment"))

#repeated measures lettuce
lettuce.lme <- lm(A ~ treatment*week, data = lettuce)
qqPlot(residuals(lettuce.lme))#pretty good
plot(lettuce.lme) #skewed
Anova(lettuce.lme)
summary(lettuce.lme)
#no interaction, treatment and week main effects

#within and across treatments (use for by week comparisons)
pairwise_treatment <- emmeans(lettuce.lme, ~ treatment )
pairwise_week <- emmeans(lettuce.lme, ~ week )
emmeans(pairwise_treatment, list(pairwise ~ treatment), adjust = "tukey")
emmeans(pairwise_week, list(pairwise ~ week), adjust = "tukey")
#photosynthesis higher in aquaponics (0.0001)
#photosynthesis declines after week 3 (0,0001)

# pairwise_treatment <- emmeans(lettuce.lme, ~ treatment * week )
# pairs(pairwise_treatment)

##test are p value same by unique treatment, get pairwise comparisons
## singular fit with lme so used simple model 
lettucesoil.mod <- lm(A ~ week, data = lettucesoil)
summary(lettucesoil.mode)
Anova(lettucesoil.mode)
emmeans(lettucesoil.mode, list(pairwise ~ week), adjust = "tukey")
##Photosynthesis in soil decreased in week 3

lettuceaqua.mod <- lm(A ~ week, data = lettuceaqua)
summary(lettuceaqua.mod)
Anova(lettuceaqua.mod)
emmeans(lettuceaqua.mod, list(pairwise ~ week), adjust = "tukey")
##photosynthesis dropped in week3         

##PACCHOI
pac_stats <- pac_noweek4[pac_noweek4$A >2,]#remove two leaves that had shutdown

#plot lettuce interaction plot
with(pac_stats, interaction.plot(week, treatment, A,
     ylab = "mean of photosynthesis", xlab = "time", trace.label = "treatment"))

#repeated measures lettuce
pac.lme <- lm(A ~ treatment*week, data = pac_stats)
qqPlot(residuals(pac.lme))
plot(pac.lme) 
Anova(pac.lme)
summary(pac.lme)
#interaction with treatment and week

#within and across treatments (use for by week comparisons)
pairwise_treatmentxweek <- emmeans(pac.lme, ~ treatment * week )
pairs(pairwise_treatmentxweek)
pairwise_treatment <- emmeans(pac.lme, ~ treatment)
emmeans(pairwise_treatment, list(pairwise ~ treatment), adjust = "tukey")
pairwise_week <- emmeans(pac.lme, ~ week)
emmeans(pairwise_week, list(pairwise ~ week), adjust = "tukey")
pairwise_inter <- emmeans(pac.lme, ~ treatment * week)
emmeans(pairwise_inter, list(pairwise ~ treatment * week), adjust = "tukey")
##A higher in aquaponics, decreases in weeks 1 both treatments

##treatment x time interaction (aqua > soil)

##test are p value same by unique treatment, get pairwise comparisons
pacsoil.mod <- lm(A ~ week, data = pacsoil)
summary(pacsoil.mod)
Anova(pacsoil.mod)
emmeans(pacsoil.mod, list(pairwise ~ week), adjust = "tukey")
##Photosynthesis decreased after week 1

pacaqua.mod <- lm(A ~ week, data = pacaqua)
summary(pacaqua.mod)
Anova(pacaqua.mod)
emmeans(pacaqua.mod, list(pairwise ~ week), adjust = "tukey")
##Photosynthesis decreased after week 1   
        
##BROCCOLI
## whe singular fit with lme used simple model 
#plot lettuce interaction plot
with(broc, interaction.plot(week, treatment, A,
     ylab = "mean of photosynthesis", xlab = "time", trace.label = "treatment"))

#repeated measures lettuce
broc.lme <- lm(A ~ treatment*week, data = broc)
qqPlot(residuals(broc.lme))#pretty good
plot(broc.lme) 
Anova(broc.lme)
summary(broc.lme)
#main effects only

#within and across treatments (use for by week comparisons)
pairwise_treatment <- emmeans(broc.lme, ~ treatment )
pairwise_week <- emmeans(broc.lme, ~ week )
emmeans(pairwise_treatment, list(pairwise ~ treatment), adjust = "tukey")
emmeans(pairwise_week, list(pairwise ~ week), adjust = "tukey")
#photosynthesis higher in aquaponics (0.0001)
#photosynthesis declines in week 3 (0.012)


##test are p value same by unique treatment, get pairwise comparisons
brocsoil.mod <- lm(A ~ week, data = brocsoil)
summary(brocsoil.mod)
Anova(brocsoil.mod)
#Photosynthesis same across weeks

brocaqua.mod <- lm(A ~ week, data = brocaqua)
summary(brocaqua.mod)
Anova(brocaqua.mod)
emmeans(brocaqua.mod, list(pairwise ~ week), adjust = "tukey")
##photosynthesis dropped in week3   