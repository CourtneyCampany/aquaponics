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

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$species <- as.factor(gasex$species)
  gasex$trial <- as.factor(gasex$trial)
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$ITE <- with(gasex, A/(E*1000))

  
#extract lettuce ---------
lettuce <- gasex[gasex$species == "S",]
broc <- gasex[gasex$species == "B" ,]
pac <- gasex[gasex$species == "P",]
  
#species by treatment
lettuceaqua <- gasex[gasex$species == "S" & gasex$treatment == "a",]
lettucesoil <- gasex[gasex$species == "S" & gasex$treatment == "c",]
  
brocaqua <- gasex[gasex$species == "B" & gasex$treatment == "a",]
brocsoil <- gasex[gasex$species == "B" & gasex$treatment == "c",]
  
pacaqua <- gasex[gasex$species == "P" & gasex$treatment == "a",]
pacsoil <- gasex[gasex$species == "P" & gasex$treatment == "c",]


#plot lettuce
with(lettuce, interaction.plot(week, treatment, A,
    ylab = "mean of photosynthesis", xlab = "time", trace.label = "treatment"))

#repeated measures lettuce
lettuce.aov <- aov(A ~ treatment * week + Error(plant),
                   data = lettuce)
summary(lettuce.aov)

lettuce.lme <- lmer(A ~ treatment*week + (1|plant), data = lettuce)
qqPlot(residuals(lettuce.lme))#pretty good
plot(lettuce.lme) #skewed
Anova(lettuce.lme)
summary(lettuce.lme)
r.squaredGLMM(lettuce.lme)

library(emmeans)
#within and across treatments (use for by week comparisons)
pairwise_treatment <- emmeans(lettuce.lme, ~ treatment * week )
pairs(pairwise_treatment)

##test are p value same if limit to just one way
lettucesoil.lme <- lmer(A ~ week + 1|plant, data = lettucesoil)
summary(lettucesoil.lme)
pairwise_lettuce <- emmeans(lettucesoil.lme, ~ week )
pairs(pairwise_lettuce)

#SD changed by week (p<0.001)
lettuceaqua.aov <- aov(A ~ week + Error(plant), data = lettuceaqua)
summary(lettuceaqua.aov)
pairwise_lettuce <- emmeans(lettucesoil.lme, ~ week )
pairs(pairwise_lettuce)
#SD did not change by week