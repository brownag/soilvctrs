## Package: soilvctrs (0.0.0.1)
### Geometric operations on stratified environmental data
### Author: Andrew G. Brown

#### Purpose

_soilvctrs_ is an experimental R package to abstract geometric operations on stratified environmental data [such as soil profile descriptions]. Under the hood, _soilvctrs_ uses [vctrs](https://cran.r-project.org/web/packages/vctrs/index.html) and [geovctrs](https://github.com/paleolimbot/geovctrs)! This package development is very much a learning _process_ and and a work in _progress_. Things may be rearranged, organized or removed at any time. 

<img src="misc/hexstickers/soilvctrs_sticker_v1.png" alt="soilvctrs hexsticker"
	title="soilvctrs" width="37%" height="37%" align="right" hspace="15" vspace="15"/>

##### Install the development version of the package

```
install.packages("remotes")
remotes::install_github("brownag/soilvctrs")
```

##### Getting started

```
library(soilvctrs)
library(tibble)

# layer data                                        ## SOIL LAYERS:
layers <- tibble(pid  = c(1,1,1,2,2,3,3),           # - unique profile IDs
                 ztop = c(0,25,50,0,18,0,50),       # - layer upper boundaries
                 zbot = c(25,50,190,18,90,50,90))   # - layer lower boundaries

# site data                                         ## SITE PROPERTIES:
sites <- tibble(pid = c(1,2,3),                     # - unique profile IDs 
                x   = c(37,38,37), y = c(34,35,36), # - XY coordinates
                z   = c(203,224,212))               # - elevation

# construct a soil_profile vctr from data.frame-like objects
profiles <- soil_profile(layers, sites)
```

The primary goal is to demonstrate concise soil-themed classes that “just work” with `tidyverse` principles. The author is not an expert in the internals of `vctrs` (or the new `geovctrs` package) so he expects to learn much more about both in the process of expanding this package. There is an opportunity to show "new" ways soil algorithms can be implemented: using efficient vctrs representations of properties and geometry.

#### Classes defined by this package

The current `soilvctrs` implementation represents parts of soil descriptions as S3 `vctrs::rcrd` objects: `soil_layer` and `soil_profile`. 

`soil_profile` instances contain a list of `soil_layer` in the field `$profile` with length zero or more. `geom` fields are defined at both profile and layer levels to visualize and analyze associated data and can also be zero-length. 

##### `soil_layer`

An empty `soil_layer` is the fundamental building block of all `soilvctrs`. 

A `soil_layer` contains three fields: `pid` (profile ID: _integer_), `hid` (layer or horizon ID: _integer_), `geom` (layer or horizon geometry: _geovctrs_rect_). `geom` is computed from the input boundary data -- currently the constructor only supports 1-dimensional profiles (Z only); this is temporary. The attributes describe the column names in the input data that were fed into fields of the same name; containing the profile ID, horizon ID and upper and lower (depth axis) boundaries, respectively. This basic `geom` concept is (probably?) extensible to complex geometric representations of the `soil_layer` (and thus the `soil_profile`) via nested `geovctrs`, the `sf` package or ???

```
soil_layer()

# soil_layer<0> with 3 fields and 4 attributes
# Fields: pid, hid, geom
# Attributes: pid, hid, ztop, zbot
```

`soil_layer` objects with length greater than zero can be constructed from data (`data.frame` subclasses + metadata) via `soil_layer()`. The default result of this public constructor is a `list` not a `soil_layer`. Each element of the `list`, presuming there were valid data entered, is a `soil_layer` with length equal to the appropriate number of layers grouped into vctrs by `pid`. 

Note that the internal constructor `.new_soil_layer` is hidden -- but can be used via `soilvctrs:::.new_soil_layer`. Let me know if you have use cases where it is valuable to have this public -- say to build a single `soil_layer` vctr with several unique `pid` values?

```
layer <- data.frame(pid = c(1,1,2), ztop = c(0,10,0), zbot = c(10,50,50))

soil_layer(layer)

# [[1]]
# soil_layer<2> with 5 fields and 4 attributes
# Fields: pid, hid, ztop, zbot, geom
# Attributes: pid, hid, ztop, zbot
# 
# [[2]]
# soil_layer<1> with 5 fields and 4 attributes
# Fields: pid, hid, ztop, zbot, geom
# Attributes: pid, hid, ztop, zbot

```

In default cases it is expected that unit-width rectangular geometries are used for horizon/layer representation. In this case the primary geometric variable is vertical depth thickness of a layer. This allows for easy re-scaling for plots, analysis etc.

Applying scaling factors to width in this model is analogous to applying depth-weighting while preserving the geometry in Z -- though this is probably not going to be used much, it is an interesting angle to consider for a set of profile geometries tessellated along an axis.

##### `soil_profile`
 
The `pid` field matches the `pid` found in the `soil_layer` vector `profile`. `geom` is a `geovctrs_xyz` ternary coordinate. Three additional attributes (beyond those created in the `soil_layer`) can be found in the `soil_profile` -- these are the names of the columns in the input data that contained X, Y and Z coordinates of the site. 

Currently attributes are the only way to "track" provenance of data that gets funneled into the standard field names. This is in lieu of a true coordinate reference system / units / ability to handle metadata properly. _The metadata concept is likely to undergo significant revision soon with above things being addressed._ 

You can use the `plot` method directly on a `soil_profile` to see all profiles tessellated and colored based on layer index.

```
> soil_profile()

# soil_profile<0> with 3 fields and 7 attributes
# Fields: pid, profile, geom
# Attributes: pid, x, y, z, hid, ztop, zbot
```

The `soil_layer` and `soil_profile` are comprised of vector "fields" of length equal to number of horizons (in a profile) or number of profiles, respectively.

The beauty of `rcrd` objects is that despite supporting arbitrary hierarchical complexity they are able to be manipulated like any `vctr`, added as columns in `tibble` data.frame subclasses and more. The specific `soil_layer` and `soil_profile` classes also (at least outwardly) draw some syntax from from the `data.frame` / `tibble`, define custom (rudimentary) plot / print / format methods. S3 methods including `$`, `[[`, `[`, and their `<-` counterparts are available for operations involving field names, indexes, access and replacement.

#### Examples

```
# soil_profile plot method (default color by layer number within profile)
plot(profiles)

# "spatial" plot, using raw xy coordinates from geovctrs_xyz geom
plot(profiles$geom)

# get just first profile
first_profile <- profiles[[1]]

# get first TWO profiles
two_profiles <- profiles[1:2,]

# get a tbl_df with pid <int> and geom <xyz> for profiles 1 and 3
profiles[c(1,3), c("pid", "geom")]

# # A tibble: 2 x 2
#     pid        geom
#   <int>       <xyz>
# 1     1 (37 34 203)
# 2     3 (37 36 212)

# get the soil_layer from first_profile
first_layers <- first_profile$profile

# soil_layer plot method
plot(first_layers)

# horizon ID (hid) is calculated and added if not present in the data
first_layers$hid

# replace a value with $
first_layers$hid <- rev(first_layers$hid)

# use j index (single colname) to get just a vector
first_layers[,"hid"]
```
	
#### On similarities to `aqp`...

This experiment is leveraging knowledge and experience I (Andrew) have gained as a major contributor to the Algorithms for Quantitative Pedology ([aqp](http://ncss-tech.github.io/AQP/)) project. 

The visual representation of soil profile sketches via Dylan Beaudette's `aqp::plotSPC` is a major conceptual inspiration for this work on `soilvctrs` -- though there is no code in common. I thank Dylan and other contributors to the AQP suite of R packages (_aqp_, _soilDB_, _sharpshootR_) for constant inspiration and much opportunity. As a contributor to `aqp`, I have no intention of superseding or "competing" with it in in this package. 

However, for experimental reasons, I intend to reproduce core "bookkeeping" and graphical code with a goal to explore new capabilities in an unconstrained way.

`aqp` has set the bar high with an immense distillation of concepts into analytic software for the masses. `soilvctrs` is definitively more developer focused and deliberately has less scope. Recent extensions allow the `SoilProfileCollection` to rely on compiled code for its workhorse functions -- making demanding computations much more favorable than they were. It is likely there will be no significant performance advantages for `soilvctrs` compared with a `data.table`-enhanced `SoilProfileCollection`.

A benefit of `soilvctrs` package will be that it imports only a couple libraries (`vctrs`, `geovctrs`) and therefore is "contained" enough for inclusion or _extension_ in other packages. Further, use of _nested_ geometries and the ability to directly manipulate profile geometric elements outside of a graphics device has exciting (still theoretical) implications for _interactive_ profile plots, 2D profile descriptions, and new algorithm design.



