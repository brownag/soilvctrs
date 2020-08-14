library(tibble)

context("soil_layer")

# test-file-wide variables in scope
dat <- NULL
res <- NULL
res2 <- NULL

test_that("soil_layer construction", {

  # check internal constructor with no inputs
  expect_equal(length(soilvctrs:::.new_soil_layer()), 0)

  #  empty soil_layers with empty geometry
  expect_silent(soil_layer(layer = NULL))
  expect_warning(soil_layer(layer = NULL, metadata = NULL))
  #expect_equal(soil_layer()$geom, geovctrs::geo_rect()[0])

  # check public constructor and is.* method
  expect_equal(length(soil_layer()), 0)

  cc <- function(l) {
    do.call('c', as.list(l))
  }

  dat <<- data.frame(
      id = cc(lapply(1:4, function(i)
        rep(i, 10))),
      top = cc(rep(0:9, 4)),
      bottom = cc(rep(1:10, 4)),
      siteprop = 8,
      x = 10,
      y = 10,
      z = 1000,
      prop = 18
    )

  # soil_layer
  dat$idd <- dat$id
  dat$topp <- dat$top
  dat$bott <- dat$bottom

  expect_silent(res <<- soil_layer(layer = dat,
                                   metadata = list(pid = "idd",
                                                   ztop = "topp",
                                                   zbot = "bott", hid = NULL)))
  expect_equal(vctrs::fields(res[[1]]), c("pid","hid","ztop","zbot","geom"))
  expect_true(is.soil_layer(res[[1]]))
  expect_equal(length(res), 4)
  expect_equal(length(res[[1]]), 10)
  expect_equal(vctrs::fields(res[[2]]), c("pid","hid","ztop","zbot","geom"))
  expect_equal(length(res[[1]][1:2, ]), 2)
  expect_equal(length(c(res[[1]][1,],res[[2]][2,])), 2)
})

test_that("$ / $<- for soil_layer", {
  expect_silent({foo <- res[[1]]})
  expect_equal(foo$pid, rep(1, 10))
  expect_error(foo$bar)
  expect_silent(foo$pid <- 100)
  expect_equal(foo$pid, rep(100, 10))
  expect_silent(foo$pid[2] <- 1)
  expect_equal(foo$pid[2], 1)
})

test_that("[ and [<-", {
  expect_silent({foo <- res[[3]]})
  expect_silent({foo1 <- foo[1,]})
  expect_equal(length(foo1), 1)
  expect_equal(foo[1, "pid"], 3)
  expect_s3_class(foo[1, c("pid","geom")], "tbl_df")

  expect_equal(foo[2:3, "pid"], c(3,3))
  expect_error(foo[1, "pid"] <- 100)

  expect_equal(foo[, "pid"], rep(3, 10))
})

geo <- NULL
colors <- NULL
test_that("[[ and [[<-", {
  foo <- res[[3]]
  expect_silent({geo <<- foo[["geom"]][1:2]})
  expect_silent({colors <- as.numeric(foo$hid[1:2])})
  expect_silent({ids <- foo[["pid"]][1:2]})
  expect_equal(foo[["pid"]],  rep(3, 10))
  expect_equal(foo[["hid"]],  21:30)
  expect_silent(foo[["hid"]] <-  1:10)
  expect_equal(foo[["hid"]],  1:10)
  expect_equal(length(foo[["geom"]]),  10)
})

test_that("error handling", {
  expect_error(soil_layer(layer = NA))
  expect_error(expect_warning(soil_layer(layer = NA, metadata = NA)))
})

test_that("plotting a single profile geom", {

  # layer data                                        ## SOIL LAYERS:
  layers <- tibble::tibble(pid  = c(1,1,1,2,2,3,3),           # - unique profile IDs
                   ztop = c(0,25,50,0,18,0,50),       # - layer upper boundaries
                   zbot = c(25,50,190,18,90,50,90))   # - layer lower boundaries

  expect_silent(plot(soil_layer(layers)[[1]]))
  expect_silent(plot(geo, col = colors))
})

test_that("default metadata", {
  expect_silent({l <- default_metadata_soil_layer()})
  expect_warning(l2 <- soil_layer(metadata = list(goo="bar")))
  expect_warning(l2 <- soilvctrs:::.new_soil_layer(metadata = list(goo="bar")))
  expect_silent({p <- default_metadata_soil_profile()})
  expect_warning(p2 <- soil_profile(metadata = list(goo="bar")))
  expect_warning(p2 <- soilvctrs:::.new_soil_profile(metadata = list(goo="bar")))
  expect_equal(length(l), 4)
  expect_equal(length(p), 7)
  expect_true(all(c("pid","hid","ztop","zbot") %in% l))
  expect_true(all(c("pid","hid","x","y","z","ztop","zbot") %in% p))
})

context("soil_profile")

test_that("constructing a soil_profile", {

  expect_silent(mono <- soil_profile())
  expect_equal(attr(mono, "x"), "x")
  expect_equal(attr(mono, "y"), "y")
  expect_equal(attr(mono, "z"), "z")
  expect_equal(attr(mono, "pid"), "pid")
  expect_equal(attr(mono, "ztop"), "ztop")
  expect_equal(attr(mono, "zbot"), "zbot")

  expect_equal(length(soil_profile()), 0)

  expect_equal(expect_warning(soil_profile(metadata = list(pid = "idd", hid = NULL,
                                            xpos = "xx", ypos="yy", zpos = "zz"))),
               soil_profile())

  expect_equal(vctrs::fields(soil_profile()), c("pid","profile","geom"))

  expect_silent(res2 <<- soil_profile(
    layer = dat,
    site = data.frame(
      id = 1:4,
      x = 1:4,
      y = 1:4,
      z = 1:4),
    metadata = list(
      pid = "id",
      x = "x",
      y = "y",
      z = "z",
      hid     = "hid",
      ztop     = "top",
      zbot     = "bottom"
    )
  ))

  # print methods
  expect_output(print.soil_profile(res2))
  expect_output(print.soil_layer(res2[[1]]$profile))

  expect_silent(print.soil_profile(NA))
  expect_silent(print.soil_layer(NA))

  # format methods
  expect_silent(format.soil_profile(res2))
  expect_silent(format.soil_layer(res2[[1]]$profile))

  expect_silent(format.soil_profile(NA))
  expect_silent(format.soil_layer(NA))

  expect_equal(length(res2), 4)
})

test_that("[ and [<-", {

  # extract pid
  expect_equal(res2$pid, 1:4)

  # change pid
  expect_silent(res2$pid <- 5:8)

  # check changed pid
  expect_equal(res2$pid, 5:8)

  # extract pid
  expect_equal(res2[1:4, "pid"], 5:8)

  # check i = NULL
  expect_equal(res2[1:4, "pid"], res2[, "pid"])

  # check i = 0
  expect_equal(res2[0, "pid"], integer(0))

  # check i < 0
  expect_equal(res2[-4, "pid"], 5:7)

  # check i = 0 with j names  > 1
  expect_equal(res2[0, c("pid", "geom")], tibble::tibble(pid = integer(0),
                                                         geom = geovctrs::geo_xyz()))

  # check i = NULL with j names > 1
  expect_equal(res2[, c("pid", "geom")], tibble::tibble(pid = 5:8,
                                                        geom = geovctrs::geo_xyz(x = 1:4,
                                                                                 y = 1:4,
                                                                                 z = 1:4)))
})

test_that("error handling", {

  # errors due to NA/NULL required inputs
  expect_error(soil_profile(layer = NA))
  expect_error(soil_profile(site = NA))
  expect_error(soil_profile(layer = NA, site = NA))
  expect_error(expect_warning(soil_profile(layer = NA, site = NA, metadata = NA)))
  expect_error(soil_profile(layer = NULL))
  expect_error(soil_profile(layer = NULL, site = NULL))
  expect_error(expect_warning(soil_profile(layer = NULL, site = NULL, metadata = NULL)))

  # empty, uses default metadata + empty layer, no site needed
  expect_silent(soil_profile(site = NULL))

  # 3 sites records, but 1 empty profile
  site <- data.frame(pid = 1:3, x = 1:3, y = 1:3, z = 1:3)
  expect_error(soil_profile(layer = data.frame(), site = site))

  # character id not allowed
  site <- data.frame(pid = LETTERS[1:3], x = 1:3, y = 1:3, z = 1:3)
  layer <- data.frame(pid=1:3, hid=1:3, ztop=1:3, zbot=1:3)
  expect_error(soil_profile(layer = layer,site = site))

  # with correct id and right number of elements in profile, no error
  site <- data.frame(pid = 1:3, x = 1:3, y = 1:3, z = 1:3)
  expect_silent(soil_profile(layer = layer, site = site))

  # with correct id and wrong number of elements  error
  site <- data.frame(pid = 1:3, x = 1:3, y = 1:3, z = 1:3)
  expect_error(soil_profile(layer = layer[1:2,], site = site))

  # with correct id and metadata not specified for nonstandard coordinates
  site <- data.frame(pid = 1:3, xx = 1:3, yy = 1:3, zz = 1:3)
  expect_error(soil_profile(layer = data.frame(pid=1:3, hid=1:3,
                                               ztop=1:3, zbot=1:3), site = site))
})

test_that("plotting a soil_profile", {
  expect_silent(plot(soil_profile())) #empty profile
  expect_silent(plot(res2$geom)) # spatial data [x,y,z]
  expect_silent(plot(res2))      # profile (horizon) data
})

test_that("one-depth-unit thick layers", {
 ten100 <- data.frame(pid = do.call('c',lapply(1:10, rep, 100)),
             hid = rep(1:100, 10), ztop = rep(0:99, 10), zbot = rep(1:100, 10))
 expect_silent(cake <- soil_profile(ten100, site = data.frame(pid = 1:10,
                                                              x = 1:10, y = 1, z = 0)))
 expect_silent(plot(cake, ymax = 100))
})

test_that("realistic data", {

  # loafercreek pedons from soilDB
  sites <- list(peiid = c(839192L, 839203L, 894094L, 894095L, 894118L, 894145L),
                pedon_id = c("2012CA6302033","2012CA6302046","2013CA6303021",
                             "2013CA6303013","2013CA6302026","2013CA6303041"),
                x = c(NA,NA,-120.65,-120.66,-120.79,-120.80),
                y = c(NA,NA,37.88,37.86,37.97,38.25),
                z = rep(0, 6),
                obs_date = c("2012-05-15","2012-06-19","2013-04-16","2013-03-27","2013-04-11","2013-07-23"),
                pmkind = rep("colluvium over residuum", 6),
                pmorigin = rep("metavolcanics", 6),
                landform_string = rep("hill", 6))

  horizons <- list(
    peiid = c(839192L, 839192L, 839192L, 839192L, 839192L, 839203L, 839203L,
              839203L, 839203L, 839203L, 894094L, 894094L, 894094L, 894094L,
              894094L, 894094L, 894095L, 894095L, 894095L, 894095L, 894095L,
              894118L, 894118L, 894118L, 894118L, 894145L, 894145L, 894145L,
              894145L, 894145L),
    hzdept = c(0L, 5L, 28L, 46L, 75L, 0L, 8L, 22L, 60L, 85L, 0L, 4L, 31L,
               48L, 64L, 80L, 0L, 3L, 15L, 37L, 53L, 0L, 5L, 25L, 59L, 0L, 6L,
               20L, 38L, 60L),
    hzdepb = c(5L, 28L, 46L, 75L, 100L, 8L, 22L, 60L, 85L, 200L, 4L, 31L,
               48L, 64L, 80L, 100L, 3L, 15L, 37L, 53L, 63L, 5L, 25L, 59L, 84L,
               6L, 20L, 38L, 60L, 200L),
    phiid = 1:30,
    hzname = c("A", "Bt1", "Bt2", "Bt3", "Cr", "A", "Bt1", "Bt2", "Cr", "R",
               "A", "Bt1", "Bt2", "Bt3", "C", "Cr", "A", "Bw", "Bt1", "Bt2",
               "Crt", "ABt", "Bt1", "Bt2", "Crt", "A", "BA", "Bt1", "Bt2", "R"),
    clay = c(16L, 19L, 22L, 26L, NA, 22L, 26L, 28L, NA, NA, 18L, 24L, 28L,
             35L, 28L, NA, 12L, 16L, 18L, 23L, NA, 24L, 28L, 33L, NA, 13L,
             17L, 21L, 24L, NA),
    sand = c(45L, 45L, 40L, 38L, NA, 45L, 40L, 40L, NA, NA, 45L, 40L, 40L,
             40L, 35L, NA, 45L, 45L, 55L, 55L, NA, NA, NA, NA, NA, 50L, 45L,
             50L, 56L, NA),
    total_frags_pct = c(2L, 5L, 20L, 30L, 0L, 5L, 7L, 5L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                        0L, 2L, 5L, 5L, 10L, 0L, 5L, 5L, 5L, 0L, 17L, 15L, 25L, 30L,
                        0L),
    texcl = structure(
      c(13L, 13L, 13L, 13L, NA, 13L, 13L, 17L, NA, NA, 13L, 13L, 17L,
        17L, 17L, NA, 13L, 13L, 13L, 13L, NA, 13L, 17L, 17L, NA, 13L,
        13L, 13L, 13L, NA),
      .Label = c("cos", "s", "fs", "vfs", "lcos", "ls", "lfs", "lvfs", "cosl",
                 "sl", "fsl", "vfsl", "l", "sil", "si", "scl", "cl", "sicl", "sc",
                 "sic", "c"), class = "factor"),
    texture = c("L", "L", "GR-L", "GR-L", "BR", "L", "L", "CL", "BR", "BR",
                "L", "L", "CL", "CL", "CL", "BR", "L", "L", "L", "L", "BR", "L",
                "CL", "CL", "BR", "GR-L", "GR-L", "CB-L", "CB-L", "BR"),
    soil_color = c("#64402B", "#624126", "#624126", "#64402B", NA, "#624126",
                   "#7E5A3B", "#7E5A3B", NA, NA, "#64402B", "#673E2F", "#723821",
                   "#A3703F", "#9D7338", NA, "#7A5C37", "#7A5C37", "#7A5C37", "#7A5C37",
                   NA, "#584537", "#584537", "#624126", NA, "#432C1B", "#624126",
                   "#7A5A48", "#A66E46", NA)
  )

  # zero length: bad metadata
  layer.list.bad <- soil_layer(layer = horizons)

  expect_equal(length(layer.list.bad), 0)

  #define metadata
  meta <- list(
    pid = "peiid",
    x = "x",
    y = "y",
    z = "z",
    ztop = "hzdept",
    zbot = "hzdepb",
    hid = "phiid"
  )

  layer.list <- soil_layer(layer = horizons, metadata = meta)

  expect_equal(length(layer.list), 6)

  # plot third profile, first to third horizon, geometry
  expect_silent(plot(layer.list[[5]]$geom, col = 1:6))

  expect_silent(profiles <- soil_profile(
    layer = as.data.frame.list(horizons),
    site = as.data.frame.list(sites),
    metadata = meta
  ))

  # geom at site level is x,y,z position (e.g. lat, long, elevation)
  expect_silent(plot(profiles$geom))

  # site level attributes are stored in the soil_list class
  expect_silent(profiles)

  # base plots (just set up axes etc)
  expect_silent(plot.soil_layer.base(profiles))
  expect_silent(plot.soil_profile.base(profiles))

  # plot method for soil_profile
  expect_silent(plot(profiles, ymax = 100))

  # plot method for soil_layer
  expect_silent(plot(layer.list[[3]], ymax=125))

})

