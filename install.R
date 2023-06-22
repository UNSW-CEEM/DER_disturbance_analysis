required_packages <- c(
    "shiny", # for the dashboard
    "shinyalert", # for `shinyalert`
    "glue", # required for shinyTime
    "vctrs", # required for shinyTime
    "Rcpp", # required for shinyTime
    "magrittr", # required for shinyTime
    "fastmap", # required for shinyTime
    "htmltools", # required for shinyTime
    "later", # required for shinyTime
    "promises", # required for shinyTime
    "httpuv", # required for shinyTime
    "mime", # required for shinyTime
    "ellipsis", # required for shinyTime
    "shinyTime", # for `timeInput`
    "sass", # required for shinyWidgets
    "lazyeval", # required for shinyWidgets
    "jsonlite", # required for shinyWidgets
    "cachem", # required for shinyWidgets
    "shinyWidgets", # for `checkboxGroupButtons` (+ potentially more)
    "plotly", # for `plotlyOutput`, `renderPlotly` and many more
    "lubridate", # for dates
    "dplyr", # for data manipulation
    "tidyr", # for data manipulation
    "data.table", # for data manipulation
    ## "shinycssloaders", # TODO: can be removed
    "fs", # required for shinyFiles
    "shinyFiles", # for `shinyFilesButton`
    "shinyjs", # for `shinyjs::hide` and `shinyjs::show`
    "fasttime", # for `fastixPOSIXct`
    "DT", # for `datatable`, `DT::coerceValue` (+ potentially more)
    "suncalc", # for `suncalc::getSunlightTimes`
    ## "ggmap", # TODO: can be removed
    ## "measurements", # TODO: can be removed
    "assertthat", # for `assert_that`
    "geosphere", # for `distHaversine` # need to have gdal installed
    "swfscMisc", # for `circle.polygon` (+ potentially more)
    "padr", # for `thicken`
    "sqldf", # for SQL queries
    ## "gridExtra", # TODO: can be removed
    "rjson", # for converting to and from JSON
    "R6", # for `R6::R6Class`
    "git2r", # for `git2r::discover_repository`
    "logging", # for logging information and errors
    "testthat") # for `test_that` and other testing functionality

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
