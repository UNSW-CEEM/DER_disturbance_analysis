#' Load libraries
#'
#' For dependencies, to use them we have to either call require() or library().
#' This doesn't install the dependency if it's required though. using fixes
#' that issue. It tries to load all packages passed to it, and if a package is
#' not found, it will install it and then load it.
#' @param ... The libraries /dependencies / packages to load.
using <- function(...) {
    libs <- unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) {
        install.packages(need)
        lapply(need, require, character.only = TRUE)
    }
}

using(
    # for the dashboard
    "shiny",

    # for `shinyalert`
    "shinyalert",

    # required for shinyTime
    "glue",
    "vctrs",
    "Rcpp",
    "magrittr",
    "fastmap",
    "htmltools",
    "later",
    "promises",
    "httpuv",
    "mime",
    "ellipsis",

    # for `timeInput`
    "shinyTime",

    # required for shinyWidgets
    "sass",
    "lazyeval",
    "jsonlite",
    "cachem",

    # for `checkboxGroupButtons` (+ potentially more)
    "shinyWidgets",

    # required for lubridate
    "timechange",

    # for dates
    "lubridate",

    # required for tidyr
    "purrr",

    # for data manipulation
    "dplyr",
    "tidyr",
    "data.table",

    # required for plotly
    "colorspace",
    "fansi",
    "utf8",
    "bit",
    "bit64",
    "processx",
    "ps",
    "tzdb",
    "tibble",
    "ggplot2",

    # for `plotlyOutput`, `renderPlotly` and many more
    "plotly",

    # TODO: can be removed
    # "shinycssloaders",

    # required for shinyFiles
    "fs",

    # for `shinyFilesButton`
    "shinyFiles",

    # for `shinyjs::hide` and `shinyjs::show`
    "shinyjs",

    # for `fastixPOSIXct`
    "fasttime",

    # for `datatable`, `DT::coerceValue` (+ potentially more)
    "DT",

    # for `suncalc::getSunlightTimes`
    "suncalc",

    # TODO: can be removed
    # "ggmap",

    # TODO: can be removed
    # "measurements",

    # for `assert_that`
    "assertthat",

    # required for geosphere
    # requires gdal to be installed
    # requires netcdf to be installed
    # requires mariadb to be installed
    "e1071",
    "classInt",
    "png",
    "ncdf4",
    "terra",
    "uuid",
    "raster",
    "spatstat.utils",
    "sf",
    "sp",
    "proxy",

    # for `distHaversine`
    # requires gdal to be installed
    # legacy packages underpinning package will retire shortly
    # see https://r-spatial.org/r/2023/05/15/evolution4.html
    "geosphere",

    # required for swfscMisc
    # requires udunits to be installed
    "units",

    # required for swfscMisc
    "yaml",

    # for `circle.polygon` (+ potentially more)
    "swfscMisc",

    # for `thicken`
    "padr",

    # required for SQL
    "rappdirs",
    "xfun",
    "RSQLite",
    "chron",

    # for SQL queries
    "sqldf",

    # TODO: can be removed
    # "gridExtra",

    # for converting to and from JSON
    "rjson",

    # for `R6::R6Class`
    "R6",

    # for `git2r::discover_repository`
    "git2r",

    # for logging information and errors
    "logging",

    # TODO: can be removed
    # required for stringr
    # "stringi",

    # TODO: can be removed
    # for string manipulations
    # "stringr",

    # required for testthat
    "brio",

    # for `test_that` and other testing functionality
    "testthat")
