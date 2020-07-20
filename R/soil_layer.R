#' Constructor for soil_layer object
#'
#' @param layer A data.frame-like object containing layer (horizon) data -- records with top and bottom depths
#' @param metadata Named list of field name aliases for profile ID, layer/horizon ID and layer top and bottom depth.
#' @details Metadata list specifications: \code{pid}: Field name containing profile ID in \code{site}; default: \code{"pid"}; \code{hid}: Field name containing layer ID in \code{horizons}; default: \code{"hid"}; \code{ztop}: Field name containing layer top depth in \code{horizons}; default: \code{"ztop"}; \code{zbot}: Field name containing layer bottom depth in \code{horizons}; default: \code{"zbot"}

#' @return A soil_layer
#' @export soil_layer
#'
soil_layer <- function(layer = data.frame(pid = integer(0),
                                             hid = integer(0),
                                             ztop = integer(0),
                                             zbot = integer(0)),
                       metadata = list(
                         pid      = "pid",
                         hid      = "hid",  # assigned if not specified
                         ztop     = "ztop",
                         zbot     = "zbot"
                       )) {

  # check for minimum required metadata

  if (!all(c("pid","ztop","zbot","hid") %in% names(metadata))) {
    warning("reverting to default metadata due to missing values")
    metadata <- default_metadata_soil_layer()
  }

  pid <- metadata$pid
  hid <- metadata$hid
  ztop <- metadata$ztop
  zbot <- metadata$zbot

  uid <- layer[[pid]]
  if (is.null(uid))
    uid <- integer(0)

  topdepth <- layer[[ztop]]
  if (is.null(topdepth))
    topdepth <- numeric(0)

  bottomdepth <- layer[[zbot]]
  if (is.null(bottomdepth))
    bottomdepth <- numeric(0)

  uuid <- unique(uid)

  # default constructs hid as a vector of integers
  if (is.null(hid)) {
    hhzid <- 1:length(uid) # hid not specified
  } else if (is.null(layer[[hid]])) {
    hhzid <- 1:length(uid) # hid not present
  } else {
    hhzid <- as.integer(layer[[hid]]) # hid specified
  }
  if (is.null(hhzid) | length(uuid) == 0)
    hhzid <- integer(0)

  # construct profile-level geometries from horizon + ID
  if (length(uuid)) {
    foo <- do.call('c', lapply(1:length(uuid), function(i) {
      idx <- which(uid == uuid[i])

      # profiles have unit width in x dimension, centered about an integer x coordinate "i"
      # # soil "depths" are negative z position, which, in 2D is typically the "y" axis!
      return(geovctrs::geo_rect(i - 0.5,
                                -(bottomdepth[idx]),
                                i + 0.5,
                                -(topdepth[idx])))
    }))
  } else foo <- geovctrs::geo_rect()[0]

  grp <- as.numeric(factor(uid))
  if (length(grp)) {
    lapply(1:max(grp), function(xx) {
      idx <- which(xx == grp)
      if (length(idx)) {
        xxx <- c(vctrs::vec_recycle_common(pid = vctrs::vec_cast(uid[idx], integer()),
                  hid = vctrs::vec_cast(hhzid[idx], integer()),
                  ztop = vctrs::vec_cast(topdepth[idx], double()),
                  zbot = vctrs::vec_cast(bottomdepth[idx], double()),
                  geom = vctrs::vec_cast(foo[idx], geovctrs::geo_rect())))#,
               #as.list(layer[idx, !colnames(layer) %in% c("pid","hid","geom")]))
       .new_soil_layer(x = xxx)
      }
    })
  } else {
    .new_soil_layer()
  }
}

#' Get default soil_layer metadata
#'
#' @return A named list containing default field names for soil_layer objects
#' @export default_metadata_soil_layer
#'
#' @examples
#'
#' # construct an empty soil_layer with default metadata
#'
#' soil_layer(metadata = default_metadata_soil_layer())
#'
default_metadata_soil_layer <- function() {
  list(
    pid      = "pid",
    hid      = "hid",
    ztop     = "ztop",
    zbot     = "zbot"
  )
}
#' Internal constructor for \code{soil_layer} object
#'
#' Requires minimum 3 vectors (all of same length) in a named list \code{x}, with following \code{name : class}:
#'
#'  - \code{pid : character}
#'  - \code{hid : integer}
#'  - \code{geom : geovctrs::geo_rect}
#'
#' @param x A named list containing, at a minimum, \code{pid}, \code{profile}, and \code{geom} named vectors.
#'
#' @return A \code{soil_layer}
#'
#' @keywords internal
#'
.new_soil_layer <- function(x = list(pid = integer(), # unique profile ID
                                     hid = integer(), # unique horizon ID/index
                                     geom = geovctrs::geo_rect()),
                                     # TODO: additional horizon-level vectors
                            metadata = list(
                              pid      = "pid",
                              hid      = "hid", # assigned if not specified
                              ztop     = "ztop",
                              zbot     = "zbot"
                            ))  {


  if (!all(c("pid","hid","ztop","zbot") %in% names(metadata))) {
    warning("reverting to default metadata due to missing values")
    metadata <- default_metadata_soil_layer()
  }

  # check input classes
  vctrs::vec_assert(x$pid, integer())
  vctrs::vec_assert(x$hid, integer())
  vctrs::vec_assert(x$geom, geovctrs::geo_rect())

  res <-   vctrs::new_rcrd(x, class = "soil_layer")

  attr(res, "pid") <- metadata$pid
  attr(res, "hid") <- metadata$hid
  attr(res, "ztop") <- metadata$ztop
  attr(res, "zbot") <- metadata$zbot
  return(res)
}


# class methods
#' Does an R object inherit from soil_layer?
#'
#' @param x An object
#'
#' @return logical
#' @export is.soil_layer
#' @method is soil_layer
#'
#'
is.soil_layer <- function(x) {
  return(inherits(x, 'soil_layer'))
}

#' @export
vec_ptype2.list.soil_layer <- function(x, y, metadata, ...) {
  if (!requireNamespace("tibble"))
    stop("package `tibble` is required", call. = FALSE)
  if (!is.null(metadata))
    return(soil_layer(layer = tibble::tibble(x), metadata = metadata))
  soil_layer(layer = tibble::tibble(x))
}

#' Cash soil_layer
#'
#' @param x A soil_layer
#' @param name Field name (character; unit length)
#'
#' @return Values of \code{name} in \code{x}
#' @export `$.soil_layer`
#'
#'
`$.soil_layer` <- function(x, name) {
  vctrs::field(x, name)
}

#' Cash<- soil_layer
#'
#' @param x A soil_layer
#' @param name Field name (character; unit length)
#' @param i Numeric or logical index
#' @param value New values
#' @export `$<-.soil_layer`
#'
#'
`$<-.soil_layer` <- function(x,name, i = NULL, value) {
  val <- try(vctrs::field(x, name))
  if (is.null(i)) {
    i <- 1:length(val)
  }
  if (!inherits(val, 'try-error')) {
    val[i] <- value
    value <- val
  }
  vctrs::field(x, name) <- value
  return(x)
}

#' Subsub a soil_layer using a field name
#'
#' @param x A soil_layer
#' @param name Field name (character; unit length)
#'
#' @return Values of field name or error if field name does not exist.
#' @export `[[.soil_layer`
#'
`[[.soil_layer` <- function(x, name) {
  vctrs::field(x, name)
}

#' Subsub-setter for value of a soil_layer field with (variable) field name
#'
#' @param x A soil_layer
#' @param name Field name (character; unit length)
#' @param value New values
#'
#' @return Values of field name or error if field name does not exist.
#' @export `[[<-.soil_layer`
#'
#'
#'
`[[<-.soil_layer` <- function(x, name, value) {
  vctrs::field(x, name) <- value
  return(x)
}


#' Subset a soil_layer
#'
#' Using just the \code{i} index performs a simple subset of a soil_layer.
#'
#' A single value in the \code{j} index will access the column values of that name (\code{j}).
#'
#' If multiple Column names are specified with the \code{j} index, the result is a data.frame-like object.
#'
#' @param x A soil_layer
#' @param i Optional: Index positions (\code{1:length(x)} if not specified)
#' @param j Optional: field names for horizon-level attributess
#'
#' @return A subset of a soil_layer, by index \code{i}, and/or column name \code{j} to return a data.frame-like object
#'
#' @export `[.soil_layer`
#'
`[.soil_layer`  <- function(x, i = NULL, j = NULL) {
  # TODO: completely refactor this
  if(is.null(i))
    i <- 1:length(x[[attr(x, "pid")]])

  # first, subset the profile based on I
  p <- .new_soil_layer(x = list(pid   = x[[attr(x,"pid")]][i],
                                hid = x[["hid"]][i],
                                geom = x[["geom"]][i]))

  return(.singlebracketj(p, i, j))
}

.singlebracketj <- function(p, i = NULL, j = NULL) {
  # TODO: refactor this idea
  # if j is a character, return a tibble view

  if (!is.null(j)){
    if (is.character(j) & length(j) > 0) {

      fld <- vctrs::fields(p)
      keep.fld <- fld %in% j

      if (sum(keep.fld) > 1) {
        res <- do.call('cbind', lapply(fld[keep.fld], function(xx) {
          tbb <- tibble::tibble(vctrs::field(p, xx))
          colnames(tbb) <- xx
          return(tbb)
        }))

        if (requireNamespace("tibble"))
          res <- tibble::tibble(res)

        colnames(res) <- j

        return(res)
      } else {
        return(vctrs::field(p, fld[keep.fld]))
      }
    }
  }
  # otherwise, return the i-subsetted record
  return(p)
}

#' format.soil_layer
#'
#' @param x A soil_layer
#' @param ... Additional arguments [not used]
#' @return Formatted soil_layer object
#' @export format.soil_layer
#' @method format soil_layer
#'
format.soil_layer <- function(x, ...) {
  if (is.soil_layer(x)) {
    # fldnm <- vctrs::fields(x)
    # res <- (lapply(fldnm, function(xx) vctrs::field(x, xx)))
    # names(res) <- fldnm
    # # return(sprintf("(%s %s [%s])",
    # #                idn,
    # #                length(fldnm),
    # #              length(idn)))
    # (out <- capture.output(str(res, give.attr = FALSE)))
    # out2 <- paste(out, collapse = "\n")
    # return((cat(out2)))
    format.default("soil_layer", ...)
  }
}

#' print.soil_layer
#'
#' @param x A soil_layer
#' @param ... Additional arguments [not used]
#' @return Print a soil_layer object
#' @export print.soil_layer
#' @method print soil_layer
#'
print.soil_layer <- function(x, ...) {
  if (is.soil_layer(x)) {
    fldnm <- vctrs::fields(x)
    res <- lapply(fldnm, function(xx) vctrs::field(x, xx))
    names(res) <- fldnm

    (att1 <- capture.output(str(attributes(x))))
    att2 <- att1[!grepl("\\$ (names|class)", att1)]
    att3 <- trimws(gsub("\\$ ([A-Za-z\\.\\_]+).*:.*", "\\1", att2))
    attnm <- att3[2:length(att3)]
    nicefields <- paste(fldnm, collapse = ", ")
    niceattr <- paste(attnm, collapse = ", ")

    out <- sprintf("soil_layer<%s> with %s fields and %s attributes\nFields: %s\nAttributes: %s\n",
                      length(res[[1]]),length(fldnm),length(attnm), nicefields, niceattr)
    cat(out)
  }
}
#' Rudimentary plotting method for soil_profile
#'
#' For inspection of geometry of a soil_layer
#'
#' @param x A soil_layer
#' @param base.plot.method A function that can take x as an argument to set up plot pane. Default is \code{plot.soil_layer.base}.
#' @param xmax Length (from 0) of X-axis (profile index axis)
#' @param ymax Length (from 0) in (negative Z direction) of plot Y-axis
#' @param ... Additional arguments to \code{plot(..., add=TRUE)} call for each layer
#'
#' @return A base graphics plot (to the current graphics device)
#'
#' @export plot.soil_layer
#' @method plot soil_layer
#'
plot.soil_layer <- function(x,
                            base.plot.method = plot.soil_layer.base,
                            xmax = length(x), ymax = 200,
                            ...) {
  base.plot.method(x, xmax, ymax)
  layer_geoms <- vctrs::field(x, "geom")
  out <- lapply(layer_geoms, function(pg) plot(pg, ..., add = TRUE))
}

#'  Default plot function for soil_layer -- sets axes etc.
#'
#' @param x A soil_layer (generally from single soil_profile)
#' @param xmax Length (from 0) of X-axis (profile index axis)
#' @param ymax Length (from 0) in (negative Z direction) of plot Y-axis
#' @param xlab X-axis Label
#' @param ylab Y-axis Label
#' @return A base graphics plot (to the current graphics device)
#' @export plot.soil_layer.base
#'
plot.soil_layer.base = function(x,
                                xmax = length(x),
                                ymax = 200,
                                xlab = "Profile Index",
                                ylab = "Z") {
  xlim <- -1:2
  if(length(x) > 0) {
    xlim_pad <- vctrs::field(geovctrs::geo_bbox(x$geom),"xmin")
    if(!is.na(xlim_pad))
      xlim <- xlim + xlim_pad
  }

  xax <- pretty(xlim, n = 1001)[1:1000]
  yax <- pretty(0:-ymax, n = 1001)[1:1000]
  plot(x = xax, y = yax, xlab = xlab, ylab = ylab, type = "n", axes = FALSE)
}
