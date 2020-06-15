library(R6)
library(sqldf)
library(RSQLite)
library(dplyr)
library(fasttime)
#source("data_cleaning_functions.R")

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
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      
      # Create table for time series data.
      RSQLite::dbExecute(con, "CREATE TABLE timeseries(
                         ts TEXT,
                         c_id INT,
                         d INT,
                         e REAL,
                         v REAL,
                         f REAL,
                         PRIMARY KEY (ts, c_id))")
      # Read in timeseries data
      # Get current column names.
      column_names <- names(read.csv(timeseries, nrows=3, header = TRUE))
      # Define a map of possible column names to names to use in the database.
      # note the prefex underscores are just for matching into the query.
      column_aliases <- list(ts='_ts', time_stamp='_ts', c_id='_c_id', v='_v', 
                             f='_f', e='_e', d='_d')
      # Define the template query.
      query <- "REPLACE INTO timeseries 
                SELECT _ts as ts, _c_id as c_id, _d as d, _e as e, _v as v,
                       _f as f from file"
      # Replace the names with underscores with the current column names.
      for (name in column_names){
        if (name %in% names(column_aliases)){
          query <- gsub(column_aliases[[name]], name, query)
        }
      }
      # Read the timeseries data into the sqlite database.
      sqldf::read.csv.sql(timeseries, sql = query, dbname = self$db_path_name, 
                          eol='\n')
      

      # Read in circuit_details data
      # Create a table for the circuit data.
      RSQLite::dbExecute(con, "CREATE TABLE circuit_details_raw(
                         c_id INT PRIMARY KEY,
                         site_id INT,
                         con_type TEXT,
                         polarity REAL)")
      # Get current column names.
      column_names <- names(read.csv(circuit_details, nrows=3, header = TRUE))
      # Define a map of possible column names to names to use in the database.
      # note the prefex underscores are just for matching into the query.
      column_aliases <- list(c_id='_c_id', site_id='_site_id', 
                             con_type='_con_type', polarity='_polarity')
      # Define the template query.
      query <- "REPLACE INTO circuit_details_raw  
                SELECT _c_id as c_id, _site_id as site_id, _con_type as con_type,
                       _polarity as polarity from file"
      # Replace the names with  underscores with the current column names.
      for (name in column_names){
        if (name %in% names(column_aliases)){
          query <- gsub(column_aliases[[name]], name, query)
        }
      }
      # Read the circuit_details data into the sqlite database.
      sqldf::read.csv.sql(circuit_details, sql = query, dbname = self$db_path_name)
      
      
      # Read in site_details data
      # Get current column names.
      column_names <- names(read.csv(site_details, nrows=3, header = TRUE))
      # Define a map of possible column names to names to use in the database.
      # note the prefex underscores are just for matching into the query.
      column_aliases <- list(site_id='_site_id', s_state='_s_state', ac='_ac',
                             dc='_dc', manufacturer='_manufacturer',
                             model='_model', s_postcode='_s_postcode',
                             pv_installation_year_month='_pv_installation_year_month')
      # Define the template query.
      query <- "create table site_details_raw as 
                select _site_id as site_id, _s_postcode as s_postcode, _s_state as s_state, 
                       _ac as ac, _dc as dc, _manufacturer as manufacturer,
                       _model as model, _pv_installation_year_month as
                       pv_installation_year_month from file"
      # Replace the names with  underscores with the current column names.
      for (name in column_names){
        if (name %in% names(column_aliases)){
          query <- gsub(column_aliases[[name]], name, query)
        }
      }
      # Read the site_details data into the sqlite database.
      sqldf::read.csv.sql(site_details, sql = query, dbname = self$db_path_name)
      
      RSQLite::dbDisconnect(con)
    },
    #' @description
    #' Delete rows in the time series table containing values that are the 
    #' header.
    #' @details
    #' Sometimes the timeseries csv files from solar analytics contain rows
    #' that are actuall just the header repeated. This method attemps to delete
    #' them from the database by deleting rows where the timestamp value equals
    #' 'ts'.
    #' @examples
    #' dp <- DataProcessor()
    #' dp$connect_to_existing_database("database_one.db")
    #' dp$drop_repeated_headers()
    drop_repeated_headers = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      RSQLite::dbExecute(con, "DELETE FROM timeseries where ts=='ts'")
      RSQLite::dbDisconnect(con)
    },
    #' @description
    #' Peforms data clean operations.
    #' @details 
    #' Iteratively retrives the timesieries data for circuits from the database in chunks of n circuits. Depending
    #' on options set different data cleaning procedrues are run and then the clean data is inserted back into the
    #' database. Timeseries data is inserted back into the table 'timeseries' and circuit and site data go into the
    #' tables 'circuit_details_clean' and 'site_details_clean'.
    #' @param max_chunk_size the number of circuits to load into memory and peform the calculation on at a time. A lower 
    #' number should lead to lower memory usage but take a longer time.
    #' @param calc_duration_values Find the time elasped between each measurement for each circuit. If the time is 5 s 
    #' then set the duration for this measurement to 5 s. This has the effect of greatly reducing the number of 
    #' measurements with missing duration data.
    #' @examples
    #' dp <- DataProcessor()
    #' dp$connect_to_existing_database("database_one.db")
    #' dp$calculate_duration_values()
    run_data_cleaning_loop = function(max_chunk_size=100, calc_duration_values=TRUE){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      ids <- RSQLite::dbGetQuery(con, "SELECT DISTINCT c_id FROM timeseries")
      
      # The setup for iteratively selecting chunks of the sql table and pulling
      # them into memory.

      # Setup for first iteration.
      length_ids <- length(ids$c_id)
      iteration_number <- 1
      start_chunk_index <- self$calc_start_chunk_index(iteration_number, max_chunk_size)
      end_chunk_index <- self$calc_end_chunk_index(length_ids, max_chunk_size, start_chunk_index)
      
      while (start_chunk_index <= length_ids){
        start_time <- Sys.time()
        # Get the ids to select on this iteration.
        
        ids_in_chunk <- ids[start_chunk_index:end_chunk_index,,drop=F]
        
        time_series <- self$get_time_series_data_by_c_id(ids_in_chunk)
        
        if (calc_duration_values){
          # Calculate the time between each measurement for each circuit id.
          time_series <- time_series %>%  dplyr::mutate(time = fasttime::fastPOSIXct(ts))
          time_series <- time_series %>% dplyr::group_by(c_id) %>% 
            dplyr::mutate(interval = time - lag(time, order_by = time))
          # If the calculated time is 5 s then set this as the duration.
          time_series <- dplyr::filter(time_series, !d %in% 5, interval %in% 5)
          time_series <- dplyr::mutate(time_series, d=ifelse(interval %in% 5,5,d))
          time_series <- dplyr::select(time_series, ts, c_id, e, f, v, d)
          
          # Replace existing data in database.
          sqldf::read.csv.sql(
            sql = "REPLACE INTO timeseries SELECT ts, c_id, d, e, v, f FROM time_series", 
            dbname = self$db_path_name)
        }

        
        # Setup for next iteration. 
        iteration_number <- iteration_number + 1
        start_chunk_index <- self$calc_start_chunk_index(iteration_number, max_chunk_size)
        end_chunk_index <- self$calc_end_chunk_index(length_ids, max_chunk_size, start_chunk_index)
        
        end_time <- Sys.time()
        print(end_time - start_time)
      }
      RSQLite::dbDisconnect(con)
    },
    get_time_series_data_by_c_id = function(c_ids){
      time_series <- sqldf::read.csv.sql(
        sql = "select * from timeseries where c_id in (select c_id from c_ids)", dbname = self$db_path_name)
      return(time_series)
    },
    calc_start_chunk_index = function(iteration_number, max_chunk_size){
      start_chunk_index <- (iteration_number - 1) * max_chunk_size + 1
      return(start_chunk_index)
    },
    calc_end_chunk_index = function(number_of_ids, max_chunk_size, start_chunk_index){
      end_chunk_index <- start_chunk_index + max_chunk_size - 1
      if (end_chunk_index > number_of_ids){end_chunk_index <- number_of_ids}
      return(start_chunk_index)
    },
    #' @description
    #' Peform minimal processing needed before site_details can be used in 
    #' analysis.
    #' @details
    #' Summarise data to one row per site, taking the first state value, first
    #' postcode value, the first ac value and the sum of ac values are recorded
    #' as these are both used in seperate parts of the subsequent analysis, 
    #' manafacture and model data is summarised by concatenating.
    #' @examples
    #' dp <- DataProcessor()
    #' dp$connect_to_existing_database("database_one.db")
    #' dp$create_processed_copy_of_site_details()
    create_processed_copy_of_site_details = function(){
      # Read the raw data from the database.
      site_details_raw <- sqldf::read.csv.sql(
        sql = "select * from site_details_raw", 
        dbname = self$db_path_name)
      
      # Peform minimal processing need before data can be used in analysis.
      site_details <- filter(site_details_raw, !is.na(ac) & ac != "")
      site_details <- mutate(site_details, s_postcode = as.character(s_postcode))
      site_details <- group_by(site_details, site_id)
      processed_site_details <- summarise(site_details, s_state=first(s_state), 
                                          pv_installation_year_month=first(pv_installation_year_month),
                                          sum_ac=sum(ac), first_ac=first(ac), s_postcode=first(s_postcode),
                                          manufacturer=paste(manufacturer, collapse=' '),
                                          model=paste(model, collapse=' '))
      processed_site_details <- as.data.frame(processed_site_details)
      
      # Create a new table for the processed data.
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS")
      RSQLite::dbExecute(con, "CREATE TABLE site_details_processed(
                         site_id INT PRIMARY KEY,
                         s_postcode TEXT,
                         s_state TEXT,
                         sum_ac REAL,
                         first_ac REAL,
                         manufacturer TEXT,
                         model TEXT,
                         pv_installation_year_month TEXT)")
      
      # Insert processed data back into the database.
      query <- "INSERT INTO table site_details_processed
                SELECT site_id,  s_postcode,  s_state, sum_ac, first_ac, 
                       manufacturer, model, pv_installation_year_month 
                  FROM processed_site_details"
      sqldf::read.csv.sql(circuit_details, sql = query, dbname = self$db_path_name)
    },
    #' @description
    #' Clean site and circuit details data.
    #' @details
    #' Find the time elasped between each measurement for each circuit. If the
    #' time is 5 s then set the duration for this measurement to 5 s. This has
    #' the effect of greatly reducing the number of measurements with missing
    #' duration data.
    #' @param max_chunk_size the number of sites to load into memory
    #' and peform the calculation on at a time. A lower number should lead to
    #' lower memory usage but take a longer time.
    #' @examples
    #' dp <- DataProcessor()
    #' dp$connect_to_existing_database("database_one.db")
    #' dp$clean_site_details()
    clean_site_details = function(max_chunk_size=100){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      ids <- RSQLite::dbGetQuery(con, "SELECT DISTINCT c_id FROM timeseries")
      
      # The setup for iteratively selecting chunks of the sql table and pulling
      # them into memory.
      length_ids <- length(ids$c_id)
      iteration_number <- 1
      start_chunk_index <- (iteration_number - 1) * max_chunk_size + 1
      end_chunk_index <- start_chunk_index + max_chunk_size - 1
      if (end_chunk_index > length_ids){end_chunk_index <- length_ids}
      # Start the iteration.
      while (start_chunk_index <= length_ids){
        start_time <- Sys.time()
        # Get the ids to select on this iteration.
        ids_in_chunk <- ids[start_chunk_index:end_chunk_index,,drop=F]
        
        time_series <- sqldf::read.csv.sql(
          sql = "select * from timeseries where c_id in (select c_id from ids_in_chunk)", 
          dbname = self$db_path_name)
        
        # Clean site details data
        site_details_cleaned <- site_details_data_cleaning(ts_data, v$site_details_raw)
        v$site_details_cleaned <- site_details_cleaned[order(site_details_cleaned$site_id),]
        
        # Clean circuit details file
        v$circuit_details_for_editing <- clean_connection_types(
          select(ts_data, ts, c_id, e, power_kW, s_postcode, con_type, polarity, first_ac), 
          v$circuit_details, v$postcode_data)
        
        # Display data cleaning output in data cleaning tab
        output$site_details_editor <- renderDT(isolate(v$site_details_cleaned), selection='single', rownames=FALSE, 
                                               editable=TRUE)
        v$proxy_site_details_editor <- dataTableProxy('site_details_editor')
        output$circuit_details_editor <- renderDT(isolate(v$circuit_details_for_editing), selection='single', 
                                                  rownames=FALSE, editable=TRUE)
        v$proxy_circuit_details_editor <- dataTableProxy('circuit_details_editor')
        
        # Add cleaned data to display data for main tab
        output$save_cleaned_data <- renderUI({actionButton("save_cleaned_data", "Save cleaned data")})
        
        
        # Setup for next iteration. 
        iteration_number <- iteration_number + 1
        start_chunk_index <- (iteration_number - 1) * max_chunk_size + 1
        end_chunk_index <- start_chunk_index + max_chunk_size - 1
        if (end_chunk_index > length_ids){end_chunk_index <- length_ids}
        end_time <- Sys.time()
        print(end_time - start_time)
      }
      RSQLite::dbDisconnect(con)
    }
  )
)
