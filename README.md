
<!-- README.md is generated from README.Rmd. Please edit that file -->
apputils
========

[![Travis-CI Build Status](https://travis-ci.org/leonawicz/apputils.svg?branch=master)](https://travis-ci.org/leonawicz/rapputils) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/apputils?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/apputils) [![Coverage Status](https://img.shields.io/codecov/c/github/leonawicz/apputils/master.svg)](https://codecov.io/github/leonawicz/apputils?branch=master)

`apputils` is an R package containing common utilty functions, settings and references for development use across multiple Shiny apps, It has a `shinydashboard` focus. `apputils` is a satellite member of the SNAPverse collection of R packages. It supports other satellites in the verse, including `maputils` and `snaputils`.

<p style="text-align:center;">
<img src="man/figures/sv_satellites_utils_app.png">
</p>
<br>

Functionality
-------------

Package functionality and areas of support covered by `apputils` include:

-   Overrides of `shinydashboard::valueBox`, `shinydashbaord::infoBox` and `shiny::icon` that support the use of local thumbnails images.
-   Stat boxes: special type of value or info boxes for common statistics using a collection of icons provided by the package.
-   Adjusted CSS styles and integration with packages like `rintrojs` and `shinytoastr` for interactive tours and toast messages.
-   Functions for including app information widgets such as citations, contact info, frequently asked questions and more.
-   Encapsulation of working with data frames in server.R in specific contexts and use cases to simplify code.
-   Wrappers around specific use cases for Leaflet maps, data tables, and general plotting in apps.
-   Helper functions for dynmaic reports.

Installation and bug reporting
------------------------------

Install the latest development version from github with

``` r
# install.packages('devtools')
devtools::install_github("leonawicz/snaputils")
```

Please file a minimal reproducible example of any clear bug at [github](https://github.com/leonawicz/apputils/issues).

Reference
---------

The complete set of satellite packages is shown below.

<p style="text-align:center;">
<img src="man/figures/sv_satellites_all.png">
</p>
<br>

[Package reference and function documentation](https://leonawicz.github.io/apputils/)
