source("scripts/functions.R")
source("scripts/plot_objects.R")

ss <- read.csv("raw_data/stomatal_size.csv")
ss$date <- as.Date(ss$date, format = "%m/%d/%Y")
#calculate stomatal size
ss$guardcell_width <- with(ss, (gc_width_1 + gc_width_2))
ss$stomatasize_mm2 <- with(ss, guardcell_width*guardcell_length)


ss_plant <- doBy::summaryBy(stomatasize_mm2 +guardcell_width + guardcell_length ~ 
                            treatment + species + plant + week, 
                            data =ss, FUN=mean2, keep.names=TRUE)

ss_agg <- doBy::summaryBy(stomatasize_mm2 +guardcell_width + guardcell_length ~ 
                          treatment + species, data =ss_plant, FUN=c(mean2,se))

ss_agg$treatment <- as.factor(ss_agg$treatment)


##data for stats----
lettuceaqua <- ss_plant[ss_plant$species == "S" & ss_plant$treatment == "A",]
lettucesoil <- ss_plant[ss_plant$species == "S" & ss_plant$treatment == "C",]

brocaqua <- ss_plant[ss_plant$species == "B" & ss_plant$treatment == "A",]
brocsoil <- ss_plant[ss_plant$species == "B" & ss_plant$treatment == "C",]

pacaqua <- ss_plant[ss_plant$species == "P" & ss_plant$treatment == "A",]
pacsoil <- ss_plant[ss_plant$species == "P" & ss_plant$treatment == "C",]

##t-test stomatal size
lettuce_ttest <- t.test(lettuceaqua$stomatasize_mm2, lettucesoil$stomatasize_mm2, 
                              alternative = "two.sided") 

broc_ttest <- t.test(brocaqua$stomatasize_mm2, brocsoil$stomatasize_mm2, 
                           alternative = "two.sided")

pac_ttest <- t.test(pacaqua$stomatasize_mm2, pacsoil$stomatasize_mm2, 
                          alternative = "two.sided")


##t-test broccoli size components

broc_ttest_photo2 <- t.test(brocaqua$guardcell_width, brocsoil$guardcell_width, 
                           alternative = "two.sided")

broc_ttest_photo <- t.test(brocaqua$guardcell_length, brocsoil$guardcell_length, 
                           alternative = "two.sided")

