## Package: soilvctrs (0.0.0.1)
### Geometric and topologic operations on stratified environmental data
### Author: Andrew G. Brown

#### Purpose

The primary goal of _soilvctrs_ is to demonstrate new soil-themed geometry classes that “just work” with the _tidyverse_ and other geometry packages. In this package, the author is making a conscious turn towards _dplyr_, _tidyr_, and other _tidyverse_ packages that use _vctrs_.

The manipulation of data internally is achieved by extension of the [vctrs](https://cran.r-project.org/web/packages/vctrs/index.html) and [geovctrs](https://github.com/paleolimbot/geovctrs) packages. 

This is an experiment that seeks to abstract geometric and topologic operations on stratified environmental data such as soil profile descriptions. This project is leveraging knowledge gained while developing profile topology, integrity and logic functions for the Algorithms for Quantitative Pedology ([aqp](http://ncss-tech.github.io/AQP/)) project -- where some of these ideas were first pioneered. 

This new class implementation represents components of soil profiles as two nested S3 objects: _soil_layer_ and _soil_profile_. Unlike in _aqp_'s S4 _SoilProfileCollection_ the geometry of _soil_profile_ and _soil_layer_ objects, and their other attributes, are stored efficiently as column _vctrs_. `geom` fields are defined at both the _soil_profile_ (_site_ location) and _soil_layer_  (_horizon_ dimensions) level and can be used to visualize spatial relationships between or within profiles. Nested relationships are used to portray multiple _soil_profile_ instances: each containing zero or more _soil_layer_ elements. 

The _soil_layer_ and _soil_profile_ do not contain _data.frame_ objects. They rather are comprised of vector "fields" of length equal to number of horizons (in a single profile) or number of profiles, respectively. S3 methods for access including `$`, `[[`, `[`, and their `<-` counterparts are available.
