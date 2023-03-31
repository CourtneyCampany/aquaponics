source("scripts/functions.R")
source("scripts/plot_objects.R")

element <- read.csv("raw_data/elemental_leaves.csv")
  element$species <- as.factor(element$species)
  element$trial <- as.factor(element$trial)
  element$treatment <- as.factor(element$treatment)
  element$cn_ratio <- with(element, c_perc/n_perc)
  element$uniqueid <- paste(element$species, element$treatment, sep="-")
  element$nmass <- element$n_perc/100

element_agg <- doBy::summaryBy(nmass + cn_ratio + c13 ~ treatment + species ,
                               data =element, FUN=c(mean, se), keep.names=TRUE)

#lettuce stats and graphs ----------
lettuceaqua <- element[element$species == "S" & element$treatment == "A",]
lettucesoil <- element[element$species == "S" & element$treatment == "C",]


lettuce_ttest_nitro <- t.test(lettuceaqua$n_perc, lettucesoil$n_perc, 
                              alternative = "two.sided") #yes

lettuce_ttest_c13 <- t.test(lettuceaqua$c13, lettucesoil$c13, 
                              alternative = "two.sided") #no

lettuce_ttest_ratio <- t.test(lettuceaqua$cn_ratio, lettucesoil$cn_ratio, 
                              alternative = "two.sided") #yes


#broc stats and graphs ----------
brocaqua <- element[element$species == "B" & element$treatment == "A",]
brocsoil <- element[element$species == "B" & element$treatment == "C",]


broc_ttest_nitro <- t.test(brocaqua$n_perc, brocsoil$n_perc, 
                              alternative = "two.sided") #yes

broc_ttest_c13 <- t.test(brocaqua$c13, brocsoil$c13, 
                            alternative = "two.sided") #yes

broc_ttest_ratio <- t.test(brocaqua$cn_ratio, brocsoil$cn_ratio, 
                              alternative = "two.sided") #yes

#pac stats and graphs ----------
pakaqua <- element[element$species == "P" & element$treatment == "A",]
paksoil <- element[element$species == "P" & element$treatment == "C",]


pak_ttest_nitro <- t.test(pakaqua$n_perc, paksoil$n_perc, 
                           alternative = "two.sided") #yes

pak_ttest_c13 <- t.test(pakaqua$c13, paksoil$c13, 
                         alternative = "two.sided") #yes

pak_ttest_ratio <- t.test(pakaqua$cn_ratio, paksoil$cn_ratio, 
                           alternative = "two.sided") #yes
