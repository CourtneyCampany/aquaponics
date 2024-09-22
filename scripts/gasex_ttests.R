#read in functions that we need and all plot labels and colors
source("scripts/functions.R")
source("scripts/plot_objects.R")

# read in gas exchange data
gasex <- read.csv("raw_data/gasexchange_master.csv")
# make variables into factors
  gasex$species <- as.factor(gasex$species)
  gasex$trial <- as.factor(gasex$trial)
  gasex$treatment <- as.factor(gasex$treatment)
#calculate new variables 
  gasex$ITE <- with(gasex, A/gsw)
  gasex$CICA <- with (gasex, Ci/Ca)

##new dataframes by species and treatment----
lettuceaqua <- gasex[gasex$species == "S" & gasex$treatment == "A",]
lettucesoil <- gasex[gasex$species == "S" & gasex$treatment == "C",]
  
brocaqua <- gasex[gasex$species == "B" & gasex$treatment == "A",]
brocsoil <- gasex[gasex$species == "B" & gasex$treatment == "C",]
  
pacaqua <- gasex[gasex$species == "P" & gasex$treatment == "A",]
pacsoil <- gasex[gasex$species == "P" & gasex$treatment == "C",]

##t-test lettuce
lettuce_ttest_photo <- t.test(lettuceaqua$A, lettucesoil$A, 
                        alternative = "two.sided")

lettuce_ttest_gsw <- t.test(lettuceaqua$gsw, lettucesoil$gsw, 
                              alternative = "two.sided")

lettuce_ttest_ITE <- t.test(lettuceaqua$ITE, lettucesoil$ITE, 
                              alternative = "two.sided")

lettuce_ttest_CICA <- t.test(lettuceaqua$CICA, lettucesoil$CICA, 
                            alternative = "two.sided")

##t-test broc
broc_ttest_photo <- t.test(brocaqua$A, brocsoil$A, 
                              alternative = "two.sided")

broc_ttest_gsw <- t.test(brocaqua$gsw, brocsoil$gsw, 
                            alternative = "two.sided")

broc_ttest_ITE <- t.test(brocaqua$ITE, brocsoil$ITE, 
                            alternative = "two.sided")

broc_ttest_CICA <- t.test(brocaqua$CICA, brocsoil$CICA, 
                             alternative = "two.sided")

##t-test pac
pac_ttest_photo <- t.test(pacaqua$A, pacsoil$A, 
                              alternative = "two.sided")

pac_ttest_gsw <- t.test(pacaqua$gsw, pacsoil$gsw, 
                            alternative = "two.sided")

pac_ttest_ITE <- t.test(pacaqua$ITE, pacsoil$ITE, 
                            alternative = "two.sided")

pac_ttest_CICA <- t.test(pacaqua$CICA, pacsoil$CICA, 
                             alternative = "two.sided")

## standard errors for table
pac_aqua_ITE_se <- se(pacaqua$ITE) #1.495157
pac_soil_ITE_se <- se(pacsoil$ITE) #3.836439

broc_aqua_ITE_se <- se(brocaqua$ITE)#0.4128896
broc_soil_ITE_se <- se(brocsoil$ITE)#2.135583

lett_aqua_ITE_se <- se(lettuceaqua$ITE)#1.19565
lett_soil_ITE_se <- se(lettucesoil$ITE)#2.378491
