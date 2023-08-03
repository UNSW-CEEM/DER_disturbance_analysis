#' Load libraries
#'
#' using tries to load all libraries passed to it, and if a library is not
#' found, using will install it and then load it.
#' @param ... The libraries / dependencies / packages to load.
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
    "plotly",
    "lubridate",
    "dplyr",
    "tidyr",
    "data.table",
    "shinyFiles",
    "shinyjs",
    "fasttime",
    "DT",
    "suncalc",
    "assertthat",
    "geosphere",
    "swfscMisc",
    "padr",
    "sqldf",
    "rjson",
    "R6",
    "git2r",
    "logging",
    "testthat",
    "shiny",
    "shinyTime",
    "shinyWidgets",
    "shinyalert"
)
