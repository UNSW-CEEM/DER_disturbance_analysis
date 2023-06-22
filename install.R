required_packages <- c(
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

    # for `plotlyOutput`, `renderPlotly` and many more
    "plotly",

    # for dates
    "lubridate",

    # for data manipulation
    "dplyr",
    "tidyr",
    "data.table",

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

    # for `distHaversine`
    # requires gdal to be installed
    # legacy packages underpinning package will retire shortly
    # see https://r-spatial.org/r/2023/05/15/evolution4.html
    "geosphere",

    # for `circle.polygon` (+ potentially more)
    "swfscMisc",

    # for `thicken`
    "padr",

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

    # for `test_that` and other testing functionality
    "testthat")

for (package in required_packages) {
    if (!package %in% installed.packages()) {
        if (!"rlang" %in% installed.packages()) {
            install.packages("rlang")
        }
        print(paste0("Package ", package, " is not installed."))
        install.packages(package, dependencies = TRUE)
    }
    require(package)
}
