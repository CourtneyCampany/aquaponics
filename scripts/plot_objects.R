# plot objects

wp1 <- wesanderson::wes_palette("Darjeeling2") 
wp2 <- wesanderson::wes_palette("Cavalcanti1")

trtcols <- c(wp1[2], wp2[5])
trtcols2 <- scales::alpha(trtcols, .6)
plantcols <- c( "darkgoldenrod4","forestgreen")


ratio_lab <- "Root to Shoot ratio"
biomasslab <- "Total Biomass (g)"
boxlabs <- c("Aquaponics", "Containerized")
satlab <- expression(italic(A)[sat]~~(mu*mol~m^-2~s^-1))
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
itelab <- expression(ITE~~(mmol~CO[2]~mol~H[2]*O^-1))
denslab <- expression(Stomatal~Density~~(mm^-2))
treelab <- c( "Shoots", "Roots")
