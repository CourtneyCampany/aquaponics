source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$species <- as.factor(gasex$species)
  gasex$trial <- as.factor(gasex$trial)
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$ITE <- with(gasex, A/(E*1000))
  gasex$CICA <- with (gasex, Ci/Ca)

  ##add CI/CA

##data for stats----
lettuceaqua <- gasex[gasex$species == "S" & gasex$treatment == "a",]
lettucesoil <- gasex[gasex$species == "S" & gasex$treatment == "c",]
  
brocaqua <- gasex[gasex$species == "B" & gasex$treatment == "a",]
brocsoil <- gasex[gasex$species == "B" & gasex$treatment == "c",]
  
pacaqua <- gasex[gasex$species == "P" & gasex$treatment == "a",]
pacsoil <- gasex[gasex$species == "P" & gasex$treatment == "c",]

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
