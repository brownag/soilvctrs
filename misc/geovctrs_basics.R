library(tibble)
library(vctrs)
library(geovctrs)

geometries <- wkt(
  c( "POLYGON ((0 0, 1 0, 1 1, 0 1, 1 1))" )
)

centroid <- function(x) {
  UseMethod("centroid")
}

centroid.default <- function(x) {
  if (is_geovctr(x)) {
    envelope_tbl <- as_tibble(geo_envelope(as_wksxp(x)))
    geo_xy(
      (envelope_tbl$xmax + envelope_tbl$xmin) / 2,
      (envelope_tbl$ymax + envelope_tbl$ymin) / 2
    )
  } else {
    result <- centroid(as_geovctr(x))
    restore_geovctr(x, result)
  }
}
res <- geo_envelope(tibble(geom = geometries)) # data.frame

plot(res$geom)

# very tiny rectangle centroid
plot(geo_envelope(tibble(geom = centroid(res$geom)))$geom, add = TRUE)

# generic soil profile data from aqp package (by horizon or layer -- depth stratififed)
data(sp1, package = "aqp")
topdepth <- sp1$top
bottomdepth <- sp1$bottom

# build rects for each horizon, they have height == (bottom - top) and unit width
#  using some default coefficients for spacing the profiles out relative to one another,
#  which is not strictly necessary other than for plotting them clearly: they could all be [0, 1]
foo <- do.call('c', lapply(1:length(unique(sp1$id)), function(i) {
  idx <- which(sp1$id == unique(sp1$id)[i])
  return(geo_rect(2 * i - 1, -(bottomdepth[idx]), 2 * i - 0.5, -(topdepth[idx])))
}))
sp12 <- tibble(sp1, geom = foo)
plot(sp12$geom) # ok, this is convincing that this covers basic geometry

library(vctrs)
