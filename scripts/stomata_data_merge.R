
#stomatal size
ss <- read.csv("raw_data/stomatal_size.csv")
  ss$date <- as.Date(ss$date, format = "%m/%d/%Y")
  #calculate stomatal size
  ss$guardcell_width <- with(ss, (gc_width_1 + gc_width_2))
  ss$stomatasize_mm2 <- with(ss, guardcell_width*guardcell_length)


ss_plant <- doBy::summaryBy(stomatasize_mm2 ~ treatment + species + plant + week, 
                            data =ss, FUN=mean, keep.names=TRUE)

#stomata density

sd <- read.csv("raw_data/stomatadensity.csv")
  sd$date <- as.Date(sd$date, format = "%m/%d/%Y")
  #calculate density from FOV diameter
  sd$sd_mm2 <- with(sd, (stomata_count/(3.14 * (fov_mm/2)^2)))

sd_plant <- doBy::summaryBy(sd_mm2 ~ treatment + species + plant + week, 
                            data =sd, FUN=mean, keep.names=TRUE)



stomatal_traits <- merge(ss_plant, sd_plant, all.x=TRUE)

write.csv(stomatal_traits, "raw_data/stomata_traits.csv", row.names = FALSE)
