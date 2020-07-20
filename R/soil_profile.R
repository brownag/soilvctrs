
#' Constructor for soil_profile (from a list of soil_layers)
#'
#' @param layer data.frame-like object containing site data (1 row = 1 layer; each becomes an element of a soil_layer)
#' @param site data.frame-like object containing site data (1 rows = 1 profiles; n-length soil_layer per profile)
#' @param metadata Named list of field name aliases for profile ID, X, Y and Z.
#'
#' @details Metadata list specifications: \code{pid}: Field name containing profile ID in \code{site}; default: \code{"pid"}; \code{xpos}: Field name containing X-position in \code{site}; default: \code{"x"}; \code{ypos}: Field name containing Y-position in \code{site}; default: \code{"y"}; \code{zpos}: Field name containing Z-position in \code{site}; default: \code{"z"}
#'
#' @return A soil_profile
#'
#' @export soil_profile
#'
soil_profile <-
  function(layer = data.frame(), # layer or horizon-level fields
           site = data.frame(), # site-level fields
           metadata = list(     # metadata
             pid = "pid",
             x = "x",
             y = "y",
             z = "z",
             hid     = "hid",
             ztop     = "ztop",
             zbot     = "zbot"
           )) {

    if (!all(c("pid","hid","x","y","z","ztop","zbot") %in% names(metadata))) {
      warning("reverting to default metadata due to missing values")
      metadata <- default_metadata_soil_profile()
    }

  if (nrow(layer) == 0 & any(!names(metadata) %in% colnames(layer))) {
     profile <- list()
  } else profile <- soil_layer(layer, metadata)

  pid <- metadata$pid
  xpos <- metadata$x
  ypos <- metadata$y
  zpos <- metadata$z
  hid <- metadata$hid
  ztop <- metadata$ztop
  zbot <- metadata$zbot

  uid <- site[[pid]]

  if (is.null(uid))
    uid <- integer(0)

  ## simple checks on IDs and spatial coordinates of the list elements (site level)

  if (length(uid) != length(profile))
    stop("number of soil_profile IDs must match number of soil_layer elements in input vector")

  xposd <- site[[xpos]]
  yposd <- site[[ypos]]
  zposd <- site[[zpos]]

  if ((length(xposd) != length(profile)) |
      (length(yposd) != length(profile)) |
      (length(zposd) != length(profile)))
    stop(sprintf("number of soil_profile XYZ-positions [%s,%s,%s] must match number of soil_layer elements in vector", xpos, ypos, zpos))

  if (is.null(xposd))
    xposd <- numeric(0)
  if (is.null(yposd))
    yposd <- numeric(0)
  if (is.null(zposd))
    zposd <- numeric(0)

  # construct soil profile object
  .new_soil_profile(vctrs::vec_recycle_common(pid = vctrs::vec_cast(uid, integer()),
                        profile = vctrs::vec_cast(profile, list()),
                        geom = vctrs::vec_cast(geovctrs::geo_xyz(xposd, yposd, zposd),
                                               geovctrs::geo_xyz())),
                    metadata)
  }

#' Get default soil_profile metadata
#'
#' @return A named list containing default field names for soil_profile objects
#' @export default_metadata_soil_profile
#'
#' @examples
#'
#' # construct an empty soil_profile with default metadata
#'
#' soil_profile(metadata = default_metadata_soil_profile())
#'
default_metadata_soil_profile <- function() {
  c(list(
    pid = "pid",
    x = "x",
    y = "y",
    z = "z"   # NB: pid is duplicated, so remove from soil_layer result
  ), default_metadata_soil_layer()[-1])
}

#' Internal constructor for \code{soil_profile} object
#'
#' Requires minimum 3 vectors (all of same length) in a named list \code{x}, with following \code{name : class}:
#'
#'  - \code{pid : character}
#'  - \code{profile : soilvctrs::soil_layer}
#'  - \code{geom : geovctrs::geo_xyz}
#'
#' @param x A named list containing, at a minimum, \code{pid}, \code{profile}, and \code{geom} named vectors.
#'
#' @return A \code{soil_profile}
#'
#' @keywords internal
#'
.new_soil_profile <- function(x = list(
  pid = integer(),                  # unique profile ID
  profile = list(),                 # soil_layer for each element; horizon data
  geom = geovctrs::geo_xyz()        # geometry
  # TODO: additional site-level vectors
), metadata = list(pid = "pid", hid  = "hid",
                   x = "x", y = "y", z = "z",
                   ztop = "ztop", zbot = "zbot")) {

  # check for minimum required metadata
  if (!all(c("pid","x","y","z","hid","ztop","zbot") %in% names(metadata))) {
    warning("reverting to default metadata due to missing values", call. = FALSE)
    metadata <- default_metadata_soil_profile()
  }

  # check datatypes
  vctrs::vec_assert(x$pid, integer())
  vctrs::vec_assert(x$profile, list()) # how to assert soil_layers while keeping them separate?
  vctrs::vec_assert(x$geom, geovctrs::geo_xyz())

  # create record object
  res <-   vctrs::new_rcrd(x, class = "soil_profile")

  # set attributes in object for metadata
  attr(res, "pid") <- metadata$pid
  attr(res, "x") <- metadata$x
  attr(res, "y") <- metadata$y
  attr(res, "z") <- metadata$z
  attr(res, "hid") <- metadata$hid
  attr(res, "ztop") <- metadata$ztop
  attr(res, "zbot") <- metadata$zbot

  # TODO: non-standard metadata
  return(res)
}

#' Does an R object inherit from soil_profile?
#'
#' @param x An object
#'
#' @return logical
#' @export is.soil_profile
#' @method is soil_profile
#'
#'
is.soil_profile <- function(x) {
  return(inherits(x, 'soil_profile'))
}

#' Cash field values of a soil_profile by name
#'
#' @param x A soil_profile
#' @param name Field name (character; unit length)
#'
#' @return Values of field name or error if field name does not exist.
#' @export `$.soil_profile`
#'
#'
`$.soil_profile` <- function(x, name) {
  vctrs::field(x, name)
}

#' Cash setter for field values in a soil_profile
#'
#' @param x A soil_profile
#' @param name Field name (character; unit length)
#' @param i Numeric or logical index
#' @param value New values
#'
#' @return A soil_profile
#' @export `$<-.soil_profile`
#'
`$<-.soil_profile` <- function(x, name, i = NULL, value) {
  val <- try(vctrs::field(x, name))
  if(is.null(i)) {
    i <- 1:length(val)
  }
  if(!inherits(val, 'try-error')) {
    val[i] <- value
    value <- val
  }
  vctrs::field(x, name) <- value
  return(x)
}

#' Subset a soil_profile
#'
#' @param x A soil_profile
#' @param i Optional: Index positions (\code{1:length(x)} if not specified)
#' @param j Optional: field names for site-level attributes
#'
#' @return A subset of a soil_profile, by index \code{i}, and/or column name \code{j} to return a data.frame-like object
#'
#' @export `[.soil_profile`
#'
#'
`[.soil_profile`  <- function(x, i = NULL, j = NULL) {
  # TODO: completely refactor
  # i subset
  if(is.null(i))
    i <- 1:length(x)

  fldnm <- vctrs::fields(x)
  newdata <- lapply(fldnm, vctrs::field, x = x)
  newdata <- lapply(1:length(fldnm), function(ii) {
    ndi <- newdata[[ii]]
    if (any(abs(i) %in% 1:length(ndi))) {
      return(ndi[i])
    } else if (i == 0) {
      return(ndi[0])
    }
  })
  names(newdata) <- fldnm

  # new soil_profile
  p <- .new_soil_profile(x = newdata)

  # optional: j subset
  return(.singlebracketj(p, i, j))
}

#' format.soil_profile
#'
#' @param x A soil_profile
#' @param ... Additional arguments [not used]
#' @return Formatted soil_profile object
#' @export format.soil_profile
#' @method format soil_profile
#'
format.soil_profile <- function(x, ...) {
  if (is.soil_profile(x)) {
    dat <- list(pid = vctrs::field(x,  "pid"),
                profiles = vctrs::field(x,  "profile"),
                geometry = vctrs::field(x,  "geom"))
    names(dat)[1] <- attr(x, "pid")
    format.default(dat, ...)
  }
  format.default(x, ...)
}
#' print.soil_profile
#'
#' @param x A soil_profile
#' @param ... Additional arguments [not used]
#' @return Print a soil_profile object
#' @export print.soil_profile
#' @method print soil_profile
#'
print.soil_profile <- function(x, ...) {
  if (is.soil_profile(x)) {
    fldnm <- vctrs::fields(x)
    res <- lapply(fldnm, function(xx) vctrs::field(x, xx))
    names(res) <- fldnm

    (att1 <- capture.output(str(attributes(x))))
    att2 <- att1[!grepl("\\$ (names|class)", att1)]
    att3 <- trimws(gsub("\\$ ([A-Za-z\\.\\_]+).*:.*", "\\1", att2))
    attnm <- att3[2:length(att3)]
    nicefields <- paste(fldnm, collapse = ", ")
    niceattr <- paste(attnm, collapse = ", ")

    out <- sprintf("soil_profile<%s> with %s fields and %s attributes\nFields: %s\nAttributes: %s\n",
                   length(res[[1]]),length(fldnm),length(attnm), nicefields, niceattr)
    cat(out)
  }
}

#' Rudimentary plotting method for soil_profile
#'
#' For inspection of geometry of a soil_profile
#'
#' @param x A soil_profile
#' @param xmax Length (from 0) of X-axis (profile index axis)
#' @param ymax Length (from 0) in (negative Z direction) of plot Y-axis
#' @param base.plot.method An R function that takes \code{x} as input and creates a base plot to draw profile geometry on.
#' @param ... Additional arguments to \code{plot}
#'
#' @return A base graphics plot (to the current graphics device)
#' @export plot.soil_profile
#'
plot.soil_profile <- function(x, xmax = length(x), ymax = 200,
                              base.plot.method = plot.soil_profile.base, ...) {

  base.plot.method(x, xmax = xmax, ymax = ymax, ...) # this plot call (user-defined) sets up graphics pane

  if (length(x) > 0) {
    # TODO: inset plot of coordinates?
    # xyz_geoms <- lapply(1:length(x), function(xx) x[[xx]]$geom)

    profile_geoms <- lapply(1:length(x), function(xx) x[[xx]]$profile$geom)
    profile_colors <- lapply(profile_geoms, function(xx) 1:length(xx))

    # additional arguments are passed to the plot calls that draw the geoms and colors
    out <- lapply(1:length(profile_geoms), function(i) plot(profile_geoms[[i]],
                                                            col = profile_colors[[i]],
                                                            add = TRUE))
  }
}

#' Default plot function for soil_profile -- sets axes etc.
#' @param x A soil_profile
#' @param xmax Length (from 0) of X-axis (profile index axis)
#' @param ymax Length (from 0) in (negative Z direction) of plot Y-axis
#' @param ... Additional arguments to \code{plot}
#' @return A base graphics plot (to the current graphics device)
#' @export plot.soil_profile.base
#'
plot.soil_profile.base = function(x, xmax = length(x), ymax = 200, ...) {
  xax <- pretty(c(0, xmax), n = 1001)[1:1001]
  yax <- pretty(c(0,-ymax), n = 1001)[1:1001]
  xax[length(xax)] <- xmax + 1
  plot(xax, yax, ...,
       xlab = "Profile Index",
       ylab = "Z", type = "n", axes = FALSE)
}
