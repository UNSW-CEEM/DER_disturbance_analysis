library(R6)
library(sqldf)
library(RSQLite)
library(dplyr)
library(data.table)
library(fasttime)
library(suncalc)

wd <- getwd()
setwd(dirname(parent.frame(2)$ofile))
source("data_cleaning_functions.R")
setwd(wd)


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
    run_data_cleaning_loop = function(max_chunk_size=100){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      
      ids <- RSQLite::dbGetQuery(con, "SELECT DISTINCT c_id FROM timeseries")
      circuit_details <- self$get_circuit_details_raw()
      site_details <- self$get_site_details_raw()
      postcode_data <- self$get_postcode_lon_lat()
      
      site_details <- site_details_data_cleaning_one(site_details)

      # Setup for first iteration.
      length_ids <- length(site_details$site_id)
      iteration_number <- 1
      start_chunk_index <- self$calc_start_chunk_index(iteration_number, max_chunk_size)
      end_chunk_index <- self$calc_end_chunk_index(length_ids, max_chunk_size, start_chunk_index)
      sites_in_chunk <- self$fiter_dataframe_by_start_and_end_index(site_details, start_chunk_index, end_chunk_index)
      circuits <- filter(circuit_details, site_id %in% sites_in_chunk$site_id)
      
    
      site_details_cleaned <- dplyr::data_frame()
      circuit_details_cleaned <- dplyr::data_frame()
      
      while (length(circuits$c_id) > 0){
        start_time <- Sys.time()

        time_series <- self$get_time_series_data_by_c_id(circuits)
        time_series <- self$clean_duration_values(time_series)
        updated_records <- self$filter_out_unchanged_records(time_series)
        self$update_timeseries_table_in_database(updated_records)
   
        
        time_series <- self$add_meta_data_to_time_series(time_series, circuit_details)
        time_series <- self$perform_power_calculations(time_series)
        
        site_details_cleaned_chunk <- site_details_data_cleaning_two(time_series, sites_in_chunk)
        site_details_cleaned <- bind_rows(site_details_cleaned, site_details_cleaned_chunk)
        
        details_to_add <- select(site_details_cleaned_chunk,  site_id, s_postcode, ac)
        time_series <- inner_join(time_series, details_to_add, by='site_id')
        
        circuit_details_cleaned_chunk <- clean_connection_types(time_series, circuits, postcode_data)
        circuit_details_cleaned <- bind_rows(circuit_details_cleaned, circuit_details_cleaned_chunk)


        # Setup for next iteration.
        iteration_number <- iteration_number + 1
        start_chunk_index <- self$calc_start_chunk_index(iteration_number, max_chunk_size)
        end_chunk_index <- self$calc_end_chunk_index(length_ids, max_chunk_size, start_chunk_index)
        sites_in_chunk <- self$fiter_dataframe_by_start_and_end_index(site_details, start_chunk_index, end_chunk_index)
        circuits <- filter(circuit_details, site_id %in% sites_in_chunk$site_id)
        if (start_chunk_index > length_ids){circuits <- ids[0:0,,drop=FALSE]}
        
        end_time <- Sys.time()
        print(end_time - start_time)
      }
      
      
      self$create_site_details_cleaned_table()
      self$insert_site_details_cleaned(site_details_cleaned)
      
      
      self$create_circuit_details_cleaned_table()
      self$insert_circuit_details_cleaned(circuit_details_cleaned)
      
      RSQLite::dbDisconnect(con)
    },
    get_circuit_details_raw = function(){
      circuit_details_raw <- sqldf::read.csv.sql(sql = "select * from circuit_details_raw", dbname = self$db_path_name)
      return(circuit_details_raw)
    },
    get_site_details_raw = function(){
      site_details_raw <- sqldf::read.csv.sql(sql = "select * from site_details_raw", dbname = self$db_path_name)
      return(site_details_raw)
    },
    get_site_details_cleaned = function(){
      site_details_cleaned <- sqldf::read.csv.sql(sql = "select * from site_details_cleaned", 
                                                  dbname = self$db_path_name)
      return(site_details_cleaned)
    },
    get_circuit_details_cleaned = function(){
      circuit_details_cleaned <- sqldf::read.csv.sql(sql = "select * from circuit_details_cleaned", 
                                                  dbname = self$db_path_name)
      return(circuit_details_cleaned)
    },
    get_site_details_processed = function(){
      site_details_processed <- sqldf::read.csv.sql(sql = "select * from site_details_processed", 
                                                  dbname = self$db_path_name)
      return(site_details_processed)
    },
    get_time_series_data_by_c_id = function(c_ids){
      time_series <- sqldf::read.csv.sql(
        sql = "select * from timeseries where c_id in (select c_id from c_ids)", dbname = self$db_path_name)
      return(time_series)
    },
    get_time_series_data = function(c_ids){
      time_series <- sqldf::read.csv.sql(sql = "select * from timeseries", dbname = self$db_path_name)
      return(time_series)
    },
    get_postcode_lon_lat = function(){
      postcodes <- sqldf::read.csv.sql(sql = "select * from postcode_lon_lat", dbname = self$db_path_name)
      return(postcodes)
    },
    calc_start_chunk_index = function(iteration_number, max_chunk_size){
      start_chunk_index <- (iteration_number - 1) * max_chunk_size + 1
      return(start_chunk_index)
    },
    calc_end_chunk_index = function(number_of_ids, max_chunk_size, start_chunk_index){
      end_chunk_index <- start_chunk_index + max_chunk_size - 1
      if (end_chunk_index > number_of_ids){end_chunk_index <- number_of_ids}
      return(end_chunk_index)
    },
    fiter_dataframe_by_start_and_end_index = function(df, start_index, end_index){
      df <- df[start_index:end_index,,drop=F]
      return(df)
    },
    clean_duration_values = function(time_series){
      time_series <- self$calc_interval_between_measurements(time_series)
      time_series <- self$flag_duration_for_updating_if_value_non_standard_and_calced_interval_is_5s(time_series)
      time_series <- self$replace_duration_value_with_calced_interval(time_series)
      return(time_series)
    },
    calc_interval_between_measurements = function(time_series){
      time_series <- time_series %>%  dplyr::mutate(time = fasttime::fastPOSIXct(ts))
      time_series <- time_series %>% dplyr::group_by(c_id) %>% 
        dplyr::mutate(interval = time - lag(time, order_by = time))
      return(time_series)
    },
    filter_out_unchanged_records = function(time_series){
      time_series <- dplyr::filter(time_series, d_change)
      time_series <- select(time_series, ts, c_id, d, e, v, f)
      return(time_series)
    },
    replace_duration_value_with_calced_interval = function(time_series){
      time_series <- dplyr::mutate(time_series, d=ifelse(d_change, 5, d))
      return(time_series)
    },
    flag_duration_for_updating_if_value_non_standard_and_calced_interval_is_5s = function(time_series){
      time_series <- dplyr::mutate(time_series, d_change=ifelse((interval %in% 5) & (!d %in% c(5, 30, 60)), TRUE, FALSE))
      return(time_series)
    },
    update_timeseries_table_in_database = function(time_series){
      sqldf::read.csv.sql(
        sql = "REPLACE INTO timeseries SELECT ts, c_id, d, e, v, f FROM time_series", 
        dbname = self$db_path_name)
    },
    perform_power_calculations = function(time_series){
      # Calculate the average power output over the sample time base on the 
      # cumulative energy and duration length.Assuming energy is joules and duration is in seconds.
      time_series <- mutate(time_series, e_polarity=e*polarity)
      time_series <- mutate(time_series, power_kW = e_polarity/(d * 1000))
      return(time_series)
    },
    add_meta_data_to_time_series = function(time_series, circuit_details){
      details_to_add <- select(circuit_details, c_id, site_id, polarity, con_type)
      time_series <- inner_join(time_series, details_to_add, by='c_id')
      return(time_series)
    },
    create_site_details_cleaned_table = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS site_details_cleaned")
      RSQLite::dbExecute(con, "CREATE TABLE site_details_cleaned(
                                 site_id INT PRIMARY KEY, s_postcode INT, s_state TEXT, pv_installation_year_month TEXT, 
                                 ac REAL, dc REAL, ac_old REAL, dc_old REAL, ac_dc_ratio REAL, manufacturer TEXT, 
                                 model TEXT, rows_grouped REAL, max_power_kW REAL, change_ac INT, change_dc INT)")
      RSQLite::dbDisconnect(con)
    },
    insert_site_details_cleaned = function(site_details){
      query <- "INSERT INTO site_details_cleaned
                            SELECT site_id, s_postcode, s_state, pv_installation_year_month, ac, dc, ac_old, dc_old, 
                                   ac_dc_ratio, manufacturer, model, rows_grouped, max_power_kW, change_ac, change_dc 
                              FROM site_details"
      sqldf::read.csv.sql(sql = query, dbname = self$db_path_name)
    },
    create_circuit_details_cleaned_table = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS circuit_details_cleaned")
      RSQLite::dbExecute(con, "CREATE TABLE circuit_details_cleaned(
                         c_id INT PRIMARY KEY, site_id INT, con_type TEXT, sunrise TEXT, sunset REAL, 
                         min_power REAL, max_power REAL, polarity INT, frac_day REAL, old_con_type TEXT, 
                         con_type_changed INT, polarity_changed TEXT)")
      RSQLite::dbDisconnect(con)
    },
    insert_circuit_details_cleaned = function(circuit_details){
      query <- "INSERT INTO circuit_details_cleaned
                            SELECT c_id, site_id, con_type, sunrise, sunset, min_power, max_power, polarity, frac_day, old_con_type, 
                                   con_type_changed, polarity_changed
                              FROM circuit_details"
      sqldf::read.csv.sql(sql = query, dbname = self$db_path_name)
    },
    add_postcode_lon_lat_to_database = function(file_path_name){
      self$create_postcode_lon_lat_table()
      self$insert_postcode_lon_lat_cleaned(file_path_name)
    },
    create_postcode_lon_lat_table = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS postcode_lon_lat")
      RSQLite::dbExecute(con, "CREATE TABLE postcode_lon_lat(postcode INT PRIMARY KEY, lon REAL, lat REAL)")
      RSQLite::dbDisconnect(con)
    },
    insert_postcode_lon_lat_cleaned = function(file_path_name){
      query <- "INSERT INTO postcode_lon_lat SELECT postcode, lon, lat FROM file"
      sqldf::read.csv.sql(file_path_name, sql = query, dbname = self$db_path_name)
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
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      
      # Read the raw data from the database.
      site_details_raw <- sqldf::read.csv.sql(sql = "select * from site_details_raw", dbname = self$db_path_name)
      
      processed_site_details <- self$process_site_details(site_details_raw)
      
      # Create a new table for the processed data.
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS site_details_processed")
      RSQLite::dbExecute(con, "CREATE TABLE site_details_processed(
                         site_id INT PRIMARY KEY,
                         s_postcode INT,
                         s_state TEXT,
                         sum_ac REAL,
                         first_ac REAL,
                         manufacturer TEXT,
                         model TEXT,
                         pv_installation_year_month TEXT)")
      
      # Insert processed data back into the database.
      query <- "INSERT INTO site_details_processed
                            SELECT site_id,  s_postcode,  s_state, sum_ac, first_ac, 
                            manufacturer, model, pv_installation_year_month 
                              FROM processed_site_details"
      sqldf::read.csv.sql(sql = query, dbname = self$db_path_name)
      RSQLite::dbDisconnect(con)
    },
    process_site_details = function(site_details_raw){
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
      return(processed_site_details)
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
