source("scripts/functions.R")

element <- read.csv("raw_data/elemental_roots.csv")
  element$species <- as.factor(element$species)
  element$trial <- as.factor(element$trial)
  element$treatment <- as.factor(element$treatment)
  element$cn_ratio <- with(element, c_perc/n_perc)
  element$uniqueid <- paste(element$species, element$treatment, sep="-")
  element$nmass <- element$n_perc/100

element_agg <- doBy::summaryBy(nmass + cn_ratio ~ treatment + species ,
                               data =element, FUN=c(mean, se), keep.names=TRUE)

#lettuce stats ----------
lettuceaqua <- element[element$species == "S" & element$treatment == "A",]
lettucesoil <- element[element$species == "S" & element$treatment == "C",]


lettuce_ttest_nitro <- t.test(lettuceaqua$n_perc, lettucesoil$n_perc, 
                              alternative = "two.sided") #yes 0.0001

lettuce_ttest_ratio <- t.test(lettuceaqua$cn_ratio, lettucesoil$cn_ratio, 
                              alternative = "two.sided") #yes 0.0001


#broc stats ----------
brocaqua <- element[element$species == "B" & element$treatment == "A",]
brocsoil <- element[element$species == "B" & element$treatment == "C",]


broc_ttest_nitro <- t.test(brocaqua$n_perc, brocsoil$n_perc, 
                           alternative = "two.sided") #yes 0.0001

broc_ttest_ratio <- t.test(brocaqua$cn_ratio, brocsoil$cn_ratio, 
                           alternative = "two.sided") #yes 0.0001

#pac stats and graphs ----------
pakaqua <- element[element$species == "P" & element$treatment == "A",]
paksoil <- element[element$species == "P" & element$treatment == "C",]


pak_ttest_nitro <- t.test(pakaqua$n_perc, paksoil$n_perc, 
                          alternative = "two.sided") #yes 0.0001

pak_ttest_ratio <- t.test(pakaqua$cn_ratio, paksoil$cn_ratio, 
                          alternative = "two.sided") #yes 0.0001
