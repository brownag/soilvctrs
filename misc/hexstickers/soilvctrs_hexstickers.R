# soilvctrs hexstickers

### soilvctrs sticker v1
library(hexSticker)
library(elevatr)
library(raster)
library(sp)
library(rayshader)

# two-tone themes
soil.color1 <- aqp:: munsell2rgb("10Y", 9, 6)
soil.color2 <- aqp::munsell2rgb("10BG", 4, 2) # OK.. sorta soil colors...

# making the rayshader image for the first version of sticker
# loc_df <- data.frame(x = c(-120.294335, -120.285462),
#                      y = c(37.917960, 37.911290))
# coordinates(loc_df) <- ~ x + y
# proj4string(loc_df) <- "+proj=longlat +datum=WGS84"
#
# dem <- get_elev_raster(loc_df, prj = "+proj=longlat +datum=WGS84", z = 14)
#
# dem.c <- crop(dem, extent(loc_df))
# dem.c2 <- aggregate(dem.c, fact = 10, fun = min)
# dem.c3 <- disaggregate(dem.c2, fact = 10, method = '')
#
# mat.c <- raster_to_matrix(dem.c3)
#
# contour(dem.c, nlevels=20, drawlabels = FALSE)
# rgdal::writeOGR(obj = rasterToContour(dem.c),
#                 dsn = "~/geodata/dem", layer="/home_contour",
#                 driver =  "ESRI Shapefile")

#
# raymat <- ray_shade(mat.c)
# ambmat <- ambient_shade(mat.c)
#
# # important to clear the rgl window if any settings are adjusted
# rgl::rgl.clear()
#
# # interactive 3D plot via rgl
# mat.c %>%
#   height_shade() %>%
#   #ray_shade() %>%
#   #height_shade() %>%
#   #add_overlay(generate_contour_overlay(mat.c)) %>%
#   #add_water(detect_water(elmat, cutoff = 0.99, min_area = 4000), color="desert") %>%
#   add_shadow(raymat, max_darken = 0.01) %>%
#   plot_3d(mat.c, zscale=0.9, fov=0, theta=30, water = 0, shadow = FALSE,
#           zoom=1, phi=45, shadowwidth = 0.001, baseshape = "circle",
#           solidlinecolor = soil.color1, solidcolor = soil.color2)

s <- sticker("~/workspace/soilvctrs/misc/hexstickers/static/wardsferry_3d_blocks.png",
  package="soilvctrs", p_size=26, p_y = 1.5,
  url = "        http://brownag.github.io/soilvctrs/", u_size = 4, u_color = soil.color1,
  h_fill = soil.color2, p_color = soil.color1, h_color = soil.color1,
  s_x=1, s_y=0.75, s_width=0.8, s_height=0.8,
  filename = "misc/hexstickers/soilvctrs_sticker_v1.png")

d <- data.frame(pid = c(1,1,1,1,1),
                hzid = 1:5,
                ztop = c(0,18,54,75,105),
                zbot=c(18,54,75,105,200))
plot(soil_layer(d)[[1]], xlab="", col = soil.color1)
idx <- seq(0, 2*pi, 0.001)
lines(x = 1+2^-exp(20*idx)*sin(333*idx),
      y = -cumsum(rep(1, length(idx))),
      lty = 3, lwd=3, col=soil.color2)

library(soilvctrs)
s <- sticker("misc/hexstickers/static/decay_vctr.png",
             package="soilvctrs", p_size=26, p_y = 1.5,
             url = "     http://brownag.github.io/soilvctrs/", u_size = 4, u_color = soil.color1,
             h_fill = soil.color2, p_color = soil.color1, h_color = soil.color1,
             s_x=1, s_y=0.8, s_width=0.2, s_height=0.2,
             filename = "misc/hexstickers/soilvctrs_sticker_v1.png")

plot(s)
