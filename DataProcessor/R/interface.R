library(R6)
library(sqldf)
library(RSQLite)

#' The DataProcessor class
#' @description 
#' Class for managing creating and interacting with an sqlite database of solar
#' analytics data.
DataProcessor <- R6::R6Class("DataProcessor",
  public = list(
    db_path_name = NULL,
    #' @description 
    #' Creates a new sqlite database with the path and name provided. All 
    #' subsequent actions are peformed on this database unless the connection
    #' is changed.
    #' @param db_path_name the path and name of the database to create.
    #' @examples
    #' dp <- DataProcessor()
    #' dp$connect_to_new_database("database_one.db")
    connect_to_new_database = function(db_path_name){
      # Create the database.
      con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path_name)
      RSQLite::dbDisconnect(con)
      # Store its name so it can be accessed later.
      self$db_path_name = db_path_name
    },
    #' @description 
    #' Conects to an existing sqlite database with the path and name provided. 
    #' All subsequent actions are peformed on this database unless the 
    #' connection is changed.
    #  @param db_path_name the path and name of the database to create.
    #' @examples
    #' dp <- DataProcessor()
    #' dp$connect_to_existing_database("database_one.db")
    connect_to_existing_database = function(db_path_name){
      self$db_path_name = db_path_name
    },
    #' @description 
    #' Inserts data from csvs into the connected database.
    #' @details
    #' None yet
    #' @param time_series The path and name of a csv file containting the time
    #' series data from solar analytics. It should contain, a column for the 
    #' timestamp (time ending of the measurement), a column with the circuit id,
    #' a column with the duration of the measurement, a column with the 
    #' cumulative energy over the measurement time, and columns for volatge and
    #' frequency measurements.
    #' @param circuit_details The path and name of a csv file containting the 
    #' meta data of the measurements on a circuit basis.
    #' @param site_details The path and name of a csv file containting the 
    #' meta data of the measurements on a circuit basis.
    build_database = function(timeseries, circuit_details, site_details) {
      column_names <- names(read.csv(timeseries, nrows=3, header = TRUE))
      column_aliases <- list(ts='_ts', 'time_stamp'='_ts', c_id='_c_id', v='_v', 
                             f='_f', e='_e', d='_d')
      query <- "create table timeseries as 
                select _ts as ts, _c_id as c_id, _e as e, _v as v,
                       _f as f, _d as d from file"
      for (name in column_names){
        query <- gsub(column_aliases[[name]], name, query)
      }
      read.csv.sql(timeseries, sql = query, dbname = self$db_path_name)
      read.csv.sql(circuit_details, sql = "create table circuit_details_raw as select * from file", 
                   dbname = self$db_path_name)
      read.csv.sql(site_details, sql = "create table site_details_raw as select * from file", 
                   dbname = self$db_path_name)
    },
    clean_data = function(){
      # Creates new tables site_details and circuit_details files in the 
      # database with 'clean' contents.
      x=1
    }
  )
)
