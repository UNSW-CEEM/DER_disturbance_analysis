
testthat::context("Testing calc_installed_capacity_by_standard_and_manufacturer.")


load_test_file <- function(text) {
  df <- read.table(text = gsub(" ", "", text), sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("simple case",{
  
  event_time <- as.POSIXct("2018-01-01 00:00:00", tz = "Australia/Brisbane")
  
  data_by_state_and_manufacturer <- "      date, state, manufacturer, capacity, number
                                     2014-01-01,   NSW,          SMA,      100,      2
                                     2014-01-01,    WA,          SMA,       50,      3
                                     2014-01-01,   NSW,            X,      101,      4
                                     2014-01-01,    WA,            X,       51,      5
                                     2015-01-01,   NSW,          SMA,      200,      3
                                     2015-01-01,    WA,          SMA,      150,      4
                                     2015-01-01,   NSW,            X,      201,      5
                                     2015-01-01,    WA,            X,      151,      6
                                     2015-10-01,   NSW,          SMA,      300,      4
                                     2015-10-01,    WA,          SMA,      250,      5
                                     2015-10-01,   NSW,            X,      301,      6
                                     2015-10-01,    WA,            X,      251,      7
                                     2015-11-01,   NSW,          SMA,      400,      5
                                     2015-11-01,    WA,          SMA,      350,      6
                                     2015-11-01,   NSW,            X,      401,      7
                                     2015-11-01,    WA,            X,      351,      8
                                     2016-10-01,   NSW,          SMA,      500,      6
                                     2016-10-01,    WA,          SMA,      450,      7
                                     2016-10-01,   NSW,            X,      501,      8
                                     2016-10-01,    WA,            X,      451,      9
                                     2016-11-01,   NSW,          SMA,      600,      7
                                     2016-11-01,    WA,          SMA,      550,      8
                                     2016-11-01,   NSW,            X,      601,      9
                                     2016-11-01,    WA,            X,      551,     10
                                     2017-11-01,   NSW,          SMA,      700,      8
                                     2017-11-01,    WA,          SMA,      650,      9
                                     2017-11-01,   NSW,            X,      701,     10
                                     2017-11-01,    WA,            X,      651,     11"
  
  
  e_out <- "      date, s_state, manufacturer, capacity, number, Standard_Version, initial_cap, standard_capacity
            2014-01-01,     NSW,          SMA,      100,      2,    AS4777.3:2005,           0,               100                
            2014-01-01,      WA,          SMA,       50,      3,    AS4777.3:2005,           0,                50   
            2014-01-01,     NSW,            X,      101,      4,    AS4777.3:2005,           0,               101
            2014-01-01,      WA,            X,       51,      5,    AS4777.3:2005,           0,                51
            2015-01-01,     NSW,          SMA,      200,      3,    AS4777.3:2005,           0,               200
            2015-01-01,      WA,          SMA,      150,      4,    AS4777.3:2005,           0,               150
            2015-01-01,     NSW,            X,      201,      5,    AS4777.3:2005,           0,               201
            2015-01-01,      WA,            X,      151,      6,    AS4777.3:2005,           0,               151
            2015-10-01,     NSW,          SMA,      300,      4,       Transition,         200,               100
            2015-10-01,      WA,          SMA,      250,      5,       Transition,         150,               100
            2015-10-01,     NSW,            X,      301,      6,       Transition,         201,               100
            2015-10-01,      WA,            X,      251,      7,       Transition,         151,               100
            2015-11-01,     NSW,          SMA,      400,      5,       Transition,         200,               200
            2015-11-01,      WA,          SMA,      350,      6,       Transition,         150,               200
            2015-11-01,     NSW,            X,      401,      7,       Transition,         201,               200
            2015-11-01,      WA,            X,      351,      8,       Transition,         151,               200
            2016-10-01,     NSW,          SMA,      500,      6,       Transition,         200,               300
            2016-10-01,      WA,          SMA,      450,      7,       Transition,         150,               300
            2016-10-01,     NSW,            X,      501,      8,       Transition,         201,               300
            2016-10-01,      WA,            X,      451,      9,       Transition,         151,               300
            2016-11-01,     NSW,          SMA,      600,      7,    AS4777.2:2015,         500,               100
            2016-11-01,      WA,          SMA,      550,      8,    AS4777.2:2015,         450,               100
            2016-11-01,     NSW,            X,      601,      9,    AS4777.2:2015,         501,               100
            2016-11-01,      WA,            X,      551,     10,    AS4777.2:2015,         451,               100
            2017-11-01,     NSW,          SMA,      700,      8,    AS4777.2:2015,         500,               200
            2017-11-01,      WA,          SMA,      650,      9,    AS4777.2:2015,         450,               200
            2017-11-01,     NSW,            X,      701,     10,    AS4777.2:2015,         501,               200
            2017-11-01,      WA,            X,      651,     11,    AS4777.2:2015,         451,               200"

  
  data_by_state_and_manufacturer <- load_test_file(data_by_state_and_manufacturer)
  e_out <- load_test_file(e_out)
  e_out <- mutate(e_out, date = ymd(date))
  output <- calc_installed_capacity_by_standard_and_manufacturer(data_by_state_and_manufacturer)
  testthat::expect_equal(output, e_out, tolerance = 1e-4)
})

testthat::test_that("discontinued line",{
  
  event_time <- as.POSIXct("2018-01-01 00:00:00", tz = "Australia/Brisbane")
  
  data_by_state_and_manufacturer <- "       date, state, manufacturer, capacity, number
                                      2014-01-01,   NSW,          SMA,      100,      2
                                      2014-01-01,    WA,          SMA,       50,      3
                                      2014-01-01,   NSW,            X,      101,      4
                                      2014-01-01,    WA,            X,       51,      5
                                      2015-01-01,   NSW,          SMA,      200,      3
                                      2015-01-01,    WA,          SMA,      150,      4
                                      2015-01-01,   NSW,            X,      201,      5
                                      2015-01-01,    WA,            X,      151,      6
                                      2015-10-01,   NSW,          SMA,      300,      4
                                      2015-10-01,    WA,          SMA,      250,      5
                                      2015-10-01,   NSW,            X,      201,      6
                                      2015-10-01,    WA,            X,      251,      7
                                      2015-11-01,   NSW,          SMA,      400,      5
                                      2015-11-01,    WA,          SMA,      350,      6
                                      2015-11-01,   NSW,            X,      201,      7
                                      2015-11-01,    WA,            X,      351,      8
                                      2016-10-01,   NSW,          SMA,      500,      6
                                      2016-10-01,    WA,          SMA,      450,      7
                                      2016-10-01,   NSW,            X,      201,      8
                                      2016-10-01,    WA,            X,      451,      9
                                      2016-11-01,   NSW,          SMA,      600,      7
                                      2016-11-01,    WA,          SMA,      550,      8
                                      2016-11-01,   NSW,            X,      201,      9
                                      2016-11-01,    WA,            X,      551,     10
                                      2017-11-01,   NSW,          SMA,      700,      8
                                      2017-11-01,    WA,          SMA,      650,      9
                                      2017-11-01,   NSW,            X,      201,     10
                                      2017-11-01,    WA,            X,      651,     11"
  
  
  e_out <- "      date, s_state, manufacturer, capacity, number, Standard_Version, initial_cap, standard_capacity
            2014-01-01,     NSW,          SMA,      100,      2,    AS4777.3:2005,           0,               100                
            2014-01-01,      WA,          SMA,       50,      3,    AS4777.3:2005,           0,                50   
            2014-01-01,     NSW,            X,      101,      4,    AS4777.3:2005,           0,               101
            2014-01-01,      WA,            X,       51,      5,    AS4777.3:2005,           0,                51
            2015-01-01,     NSW,          SMA,      200,      3,    AS4777.3:2005,           0,               200
            2015-01-01,      WA,          SMA,      150,      4,    AS4777.3:2005,           0,               150
            2015-01-01,     NSW,            X,      201,      5,    AS4777.3:2005,           0,               201
            2015-01-01,      WA,            X,      151,      6,    AS4777.3:2005,           0,               151
            2015-10-01,     NSW,          SMA,      300,      4,       Transition,         200,               100
            2015-10-01,      WA,          SMA,      250,      5,       Transition,         150,               100
            2015-10-01,     NSW,            X,      201,      6,       Transition,         201,                 0
            2015-10-01,      WA,            X,      251,      7,       Transition,         151,               100
            2015-11-01,     NSW,          SMA,      400,      5,       Transition,         200,               200
            2015-11-01,      WA,          SMA,      350,      6,       Transition,         150,               200
            2015-11-01,     NSW,            X,      201,      7,       Transition,         201,                 0
            2015-11-01,      WA,            X,      351,      8,       Transition,         151,               200
            2016-10-01,     NSW,          SMA,      500,      6,       Transition,         200,               300
            2016-10-01,      WA,          SMA,      450,      7,       Transition,         150,               300
            2016-10-01,     NSW,            X,      201,      8,       Transition,         201,                 0
            2016-10-01,      WA,            X,      451,      9,       Transition,         151,               300
            2016-11-01,     NSW,          SMA,      600,      7,    AS4777.2:2015,         500,               100
            2016-11-01,      WA,          SMA,      550,      8,    AS4777.2:2015,         450,               100
            2016-11-01,     NSW,            X,      201,      9,    AS4777.2:2015,         201,                 0
            2016-11-01,      WA,            X,      551,     10,    AS4777.2:2015,         451,               100
            2017-11-01,     NSW,          SMA,      700,      8,    AS4777.2:2015,         500,               200
            2017-11-01,      WA,          SMA,      650,      9,    AS4777.2:2015,         450,               200
            2017-11-01,     NSW,            X,      201,     10,    AS4777.2:2015,         201,                 0
            2017-11-01,      WA,            X,      651,     11,    AS4777.2:2015,         451,               200"

  
  data_by_state_and_manufacturer <- load_test_file(data_by_state_and_manufacturer)
  e_out <- load_test_file(e_out)
  e_out <- mutate(e_out, date = ymd(date))
  output <- calc_installed_capacity_by_standard_and_manufacturer(data_by_state_and_manufacturer)
  testthat::expect_equal(output, e_out, tolerance = 1e-4)
})