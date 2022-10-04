source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
gasex$species <- as.factor(gasex$species)
gasex$trial <- as.factor(gasex$trial)
gasex$treatment <- as.factor(gasex$treatment)
gasex$ITE <- with(gasex, A/(E*1000))
