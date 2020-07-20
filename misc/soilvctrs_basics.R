##
## test it out
##
##
##
library(soilvctrs)
library(aqp)
library(soilDB)

data(loafercreek, package = "soilDB")

sites <- site(loafercreek)
horizons <-  horizons(loafercreek)

# check internal constructor
soilvctrs:::.new_soil_profile()

# check public constructor and is.* method
is.soil_profile(soil_profile())
horizons$peiid <- as.integer(horizons$peiid)

# construct a list of soil_layer objects
layer.list <- soil_layer(
  layer = as.data.frame.list(horizons),
  metadata =  list(
    pid = "peiid",
    ztop = "hzdept",
    zbot = "hzdepb",
    hid = "phiid"
  )
)

# plot third profile, first to third horizon, geometry
plot(layer.list[[5]]$geom,
     xlim = c(3,5), ylim = c(0,-200),
     col = 1:6)

sites$peiid <- as.integer(sites$peiid)

profiles <- soil_profile(
  layer = horizons,
  site = sites,
  metadata = list(
    pid = "peiid",
    x = "x",
    y = "y",
    z = "elev_field",
    ztop = "hzdept",
    zbot = "hzdepb",
    hid = "phiid"
  )
)

# geom at site level is x,y,z position (e.g. lat, long, elevation)
plot(profiles$geom)

# site level attributes are stored in the soil_profile class
profiles

# plot method for soil_profile
plot(profiles, ymax = 100)

# plot method for soil_layer
p <- profiles$profile[[1]]
plot(, ymax = 150)
coord <- geovctrs::geo_coordinates(profiles$profile[[1]]$geom)
lines(1+0.5*cos(seq(0, 1, 0.01)*pi), 150*seq(0, 1, 0.01))
abline(v=0)
library(ggplot2)

(profiles[,c('pid','geom')])

